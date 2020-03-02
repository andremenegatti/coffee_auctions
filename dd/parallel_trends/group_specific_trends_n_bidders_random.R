library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = readRDS) %>% set_names(c('dd_brasil', 'dd_sp'))

cnet_lances <- readRDS('data/cnet_lances.rds')
cnet_cafe <- readRDS('data/cnet_cafe.rds')

# Participantes na fase aleatória ---------------------------------------------
# SP
df_n_bidders_random_sp <- cnet_lances %>% 
  semi_join(dd_data_list$dd_sp, by = 'id_item') %>% 
  left_join(cnet_cafe %>% select(id_item, inicio_fase_aleatoria),
            by = 'id_item') %>% 
  filter(data_hora >= inicio_fase_aleatoria) %>% 
  group_by(id_item) %>% 
  summarise(n_bidders_random = length(unique(CNPJ_CPF)))

dd_data_list$dd_sp <- dd_data_list$dd_sp %>% 
  left_join(df_n_bidders_random_sp, by = 'id_item') %>% 
  mutate(num_forn_lances = ifelse(!is.na(n_bidders_random),
                                  n_bidders_random,
                                  num_forn_lances))

# Brasil
df_n_bidders_random_brasil <- cnet_lances %>% 
  semi_join(dd_data_list$dd_brasil, by = 'id_item') %>% 
  left_join(cnet_cafe %>% select(id_item, inicio_fase_aleatoria),
            by = 'id_item') %>% 
  filter(data_hora >= inicio_fase_aleatoria) %>% 
  group_by(id_item) %>% 
  summarise(n_bidders_random = length(unique(CNPJ_CPF)))

dd_data_list$dd_brasil <- dd_data_list$dd_brasil %>% 
  left_join(df_n_bidders_random_brasil, by = 'id_item') %>% 
  mutate(num_forn_lances = ifelse(!is.na(n_bidders_random),
                                  n_bidders_random,
                                  num_forn_lances))

# Estimating DD with group-specific linear trends -----------------------------
trends_sp <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp)

trends_brasil <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_data_list$dd_brasil)

# Salvando tabelas de resultados
stargazer(trends_brasil, trends_sp,
          type = 'text',
          out = 'results/n_bidders_random/group_trends/trends.txt')

stargazer(trends_brasil, trends_sp,
          out = 'results/n_bidders_random/group_trends/trends_latex.tex',
          decimal.mark = ',', digit.separator = '.')

# HC1 SE ----------------------------------------------------------------------
trends_sp_hc1 <- 
  lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre +
       bimestre + unidade_compradora + municipio + marca_vencedor_principais,
     data = dd_data_list$dd_sp) %>% 
  PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

trends_brasil_hc1 <- 
  lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + 
       bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
     data = dd_data_list$dd_brasil) %>% 
  PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

# Salvando dados --------------------------------------------------------------
list('trends_sp_hc1' = trends_sp_hc1,
     'trends_brasil_hc1' = trends_brasil_hc1) %>% 
  saveRDS('results/n_bidders_random/group_trends/trends_hc1_results.rds')

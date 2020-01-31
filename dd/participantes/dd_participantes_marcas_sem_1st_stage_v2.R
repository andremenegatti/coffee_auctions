library(stargazer)
library(tidyverse)
library(PregoesBR)
library(lfe)

#### ABRINDO BASES ####
# Principais
BEC_cafe <- readRDS('DD/bec_cafe_dd.rds')
cnet_cafe <- readRDS('DD/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('DD/cnet_cafe_sp_dd.rds')

# Importando DFs com UASGs selecionadas
selected_uasgs_list <- list(
  readRDS('Selecionando UASGs/bec_selected_uasgs_2015.rds'),
  readRDS('Selecionando UASGs/cnet_selected_uasgs_2015.rds'),
  readRDS('Selecionando UASGs/cnet_sp_selected_uasgs_2015.rds')
) %>%
  # Selecionando apenas colunas desejadas
  map(.f = ~ .x %>%
        select(id_item,unidade_compradora_lasso = lasso) %>%
        mutate(unidade_compradora_lasso = as.character(unidade_compradora_lasso))
  ) %>% set_names('bec_selected_uasgs', 'cnet_selected_uasgs', 'cnet_sp_selected_uasgs')

#### PREPARANDO BASES ####
data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        # EXCLUINDO OBSERVACOES POSTERIORES A 2016 ## <----------
      filter(abertura_lances < '2016-01-01') %>%
        # TRIMMING: 2.5% de cada lado ## <----------------------
      # trim_df('win_bid', perc = 5) %>%
        # Selecionando apenas variaveis relevantes
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid,
               num_forn_lances,
               comprasnet,
               sigla_uf, municipio,
               unidade_compradora,
               marca_vencedor_principais, kg_por_unid,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2
        ) %>%
        # Transformando todos os factors em caracteres para evitar warnings ao juntar bases
        mutate_if(is.factor, as.character) #%>%
      # filter(kg_por_unid != 0.25) %>%
      # Incluindo residuos de 1o estagio: win_bid ~ unidade_compradora
      # add_1st_stage_residuals(formula = 'win_bid ~ unidade_compradora', residuals_column_name = 'res_EF_uasgs')
  ) %>%
  # Incluindo coluna com uasgs selecionadas
  map2(
    .y = selected_uasgs_list,
    .f = ~ left_join(.x, .y, by = 'id_item')
  ) %>%
  # Dando nomes aos DFs
  set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

# data_list$cnet_cafe_sp <- data_list$cnet_cafe_sp %>%
#   filter(kg_por_unid != 0.25)

#### MONTANDO BASES DD ####
dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$BEC_cafe) %>%
        build_dd_df()
  ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

##### Salvando como .dta para checar no Stata ####
# dd_data_list$sp_cafe_dd %>%
#   mutate(id_item = factor(id_item)) %>%
#   select(-inicio_ano, -inicio_bimestre, -inicio_mes, -inicio_semana) %>%
#   haven::write_dta('C:/Users/Dell/Desktop/sp_cafe_dd.dta')

#### DD ####
attach(dd_data_list$sp_cafe_dd)
detach(dd_data_list$sp_cafe_dd)

# Log
felm1_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre)
felm3_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora + municipio)
felm4_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora + municipio + marca_vencedor_principais)
felm6_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid | bimestre + unidade_compradora + municipio + marca_vencedor_principais)
felm8_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais)
felm9_log <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais)

felm_log_trend <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora + municipio + marca_vencedor_principais)


stargazer(felm1_log,
          felm3_log,
          felm4_log,
          felm6_log,
          felm8_log,
          felm9_log,
          felm_log_trend,
          type = 'text', decimal.mark = ',', digit.separator = '.',
          omit.stat = c('rsq', 'ser', 'f'),
          out = c('C:/Users/Dell/Desktop/participantes_sp.txt', 'C:/Users/Dell/Desktop/participantes_win_bid_sp.tex'))


# UF e UF x Bimestre
detach(dd_data_list$sp_cafe_dd)
attach(dd_data_list$full_cafe_dd)

felm_log_full1  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre)
felm_log_full2  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre)
felm_log_full3  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre + municipio)
felm_log_full4  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
felm_log_full5  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade | bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
felm_log_full6  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)
felm_log_full7  <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)
felm_log_full8 <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
felm_log_full9 <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora + marca_vencedor_principais)

felm_log_full_trend <- felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)

stargazer(felm_log_full1,
          felm_log_full2,
          # felm_log_full3,
          felm_log_full4,
          # felm_log_full5,
          felm_log_full6,
          felm_log_full7,
          felm_log_full8,
          felm_log_full9,
          felm_log_full_trend,
          type = 'text', decimal.mark = ',', digit.separator = '.',
          omit.stat = c('rsq', 'ser', 'f'),
          out = c('C:/Users/Dell/Desktop/num_forn_lances_full.txt', 'C:/Users/Dell/Desktop/num_forn_lances_full.tex'))


# Robust
lm_base <- lm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_defl + kg_por_unid + qualidade + bimestre + municipio + unidade_compradora + marca_vencedor_principais)
lm_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + futuro_defl + arab_defl + kg_por_unid + qualidade + bimestre + municipio + unidade_compradora + marca_vencedor_principais)
lm_log_uf <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
                data = dd_data_list$full_cafe_dd)


get_robust_std_errors(lm_base, HC = 'HC1')

get_robust_std_errors(lm_log, HC = 'HC1')$p_value[3:4]
df_std <- get_robust_std_errors(lm_log_uf, HC = 'HC1')



# Log
lm1_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre)
lm3_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre + unidade_compradora + municipio)
lm4_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre + unidade_compradora + municipio + marca_vencedor_principais)
lm6_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + bimestre + unidade_compradora + municipio + marca_vencedor_principais)
lm8_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + bimestre + unidade_compradora + municipio + marca_vencedor_principais)
lm9_log <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + unidade_compradora + municipio + marca_vencedor_principais)
lm_log_trend <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + bimestre + unidade_compradora + municipio + marca_vencedor_principais)


lm_full1  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre)
lm_full2  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre + sigla_uf:bimestre)
lm_full4  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
lm_full6  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)
lm_full7  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)
lm_full8  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
lm_full9  <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora + marca_vencedor_principais)
lm_full_trend <- lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + bimestre  + sigla_uf:bimestre + municipio + unidade_compradora)


get_robust_std_errors(lm_full1, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full2, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full4, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full6, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full7, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full8, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full9, HC = 'HC1') %>% slice(3:4)
get_robust_std_errors(lm_full_trend, HC = 'HC1') %>% slice(3:4)




library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = ~ readRDS(.x) %>% filter(!is.na(num_forn_lances))) %>%
  set_names(c('dd_brasil', 'dd_sp'))

# DD SP -----------------------------------------------------------------------
form <- 'log_win_bid ~ comprasnet + treat1 + treat2'

df_models_sp <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais')
    ),
  felm_models = map(.x = formula,
                    .f = ~ felm(as.formula(.x), data = dd_data_list$dd_sp)))

# Salvando tabelas de resultados
stargazer(df_models_sp$felm_models, type = 'text',
          out = 'results/log_win_bid/sp.txt')

stargazer(df_models_sp$felm_models,
          out = 'results/log_win_bid/sp_latex.tex',
          decimal.mark = ',', digit.separator = '.')

# HC1 SE
df_models_sp <- df_models_sp %>% 
  mutate(hc1_se = map(.x = formula,
                      .f = ~ str_replace(.x, '\\|', '+') %>% 
                        lm(data = dd_data_list$dd_sp) %>% 
                        PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
                        head(n = 20)))

# Salvando dados
df_models_sp %>% 
  saveRDS('results/log_win_bid/main_results_sp.rds')

# DD Brasil -------------------------------------------------------------------
df_models_brasil <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora')),
  felm_models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list$dd_brasil))) # <<<<

# Salvando tabelas de resultados
stargazer(df_models_brasil$felm_models, type = 'text',
          out = 'results/log_win_bid/brasil.txt')

stargazer(df_models_brasil$felm_models,
          out = 'results/log_win_bid/brasil_latex.tex',
          decimal.mark = ',', digit.separator = '.')

# HC1 SE
df_models_brasil <- df_models_brasil %>% 
  mutate(hc1_se = map(.x = formula,
                         .f = ~ str_replace(.x, '\\|', '+') %>% 
                           lm(data = dd_data_list$dd_brasil) %>% 
                        PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
                        head(n = 20))
  )

# Salvando dados
df_models_brasil %>% 
  saveRDS('results/log_win_bid/main_results_brasil.rds')

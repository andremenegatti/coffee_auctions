library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('ajustes-marcas/dd_brasil.rds', 'ajustes-marcas/dd_sp.rds') %>% 
  map(.f = readRDS) %>% set_names(c('dd_brasil', 'dd_sp'))

# DD SP -----------------------------------------------------------------------
form <- 'num_forn_lances ~ comprasnet + treat1 + treat2'

df_models_sp <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais')
    ),
  felm_models = map(.x = formula,
                    .f = ~ felm(as.formula(.x), data = dd_data_list$dd_sp)))

# Salvando tabelas de resultados
stargazer(df_models_sp$felm_models, type = 'text',
          out = 'ajustes-marcas/results/n-bidders-sp.txt')

stargazer(df_models_sp$felm_models,
          out = 'ajustes-marcas/results/n-bidders-sp.tex',
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
  saveRDS('ajustes-marcas/results/n-bidders-sp.rds')

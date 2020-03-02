library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
# Principais
bec_cafe <- readRDS('data/bec_cafe_dd.rds')
cnet_cafe <- readRDS('data/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        # Removendo outliers: 2.5% de cada lado # <<<<
        PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>%
        # Selecionando apenas variaveis relevantes
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_por_unid, num_forn_lances,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2
               ) %>%
        # Coercing to factor to avoid warnings when joining dataframes
        mutate_if(is.factor, as.character)
      ) %>% 
  # Dando nomes aos DFs
  set_names(c('bec', 'cnet', 'cnet_sp'))

# Montando bases DD em uma lista ----------------------------------------------
dd_data_list <- list(data_list$cnet, data_list$cnet_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$bec) %>%
        PregoesBR::build_dd_df()
      ) %>% set_names(c('dd_brasil', 'dd_sp'))

# Salvando como .dta para checar no Stata -------------------------------------
# dd_data_list$dd_sp %>%
#   mutate(id_item = factor(id_item)) %>%
#   select(-inicio_ano, -inicio_bimestre, -inicio_mes, -inicio_semana) %>%
#   haven::write_dta('sp_cafe_dd.dta')

# DD SP -----------------------------------------------------------------------
form <- 'log_win_bid ~ comprasnet + treat1 + treat2'

df_models_sp <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora'),
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade                             | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora + municipio + marca_vencedor_principais')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list$dd_sp))) # <<<<

stargazer(df_models_sp$models, type = 'text',
          out = 'results/log_win_bid/sp.txt')

# HC1 SE
lm_sp <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + 
              kg_por_unid + qualidade + bimestre + municipio + 
              unidade_compradora + marca_vencedor_principais,
            data = dd_data_list$dd_sp)

df_std_sp <- PregoesBR::get_robust_std_errors(lm_sp, HC = 'HC1')

saveRDS(df_std_sp, 'results/log_win_bid/HC1_std_sp.rds')

# DD Brasil -------------------------------------------------------------------
df_models_brasil <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade                             | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + sigla_uf:bimestre + municipio + unidade_compradora')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list$dd_brasil))) # <<<<

stargazer(df_models_brasil$models, type = 'text',
          out = 'results/log_win_bid/brasil.txt')

# HC1 SE
lm_brasil <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade 
                + kg_por_unid + arab_defl + bimestre + sigla_uf:bimestre
                + municipio + unidade_compradora,
                data = dd_data_list$dd_brasil)

df_std_brasil <- PregoesBR::get_robust_std_errors(lm_brasil, HC = 'HC1')

saveRDS(df_std_brasil, 'results/log_win_bid/HC1_std_brasil.rds')



# LASSO-SELECTED UASGS --------------------------------------------------------
# Lasso: DD SP ----------------------------------------------------------------
form <- 'log_win_bid ~ comprasnet + treat1 + treat2'

df_models_sp_lasso <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora_lasso'),
      str_c(form, ' | bimestre + unidade_compradora_lasso + municipio'),
      str_c(form, ' | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade                             | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list$dd_sp))) # <<<<

stargazer(df_models_sp_lasso$models, type = 'text',
          out = 'results/log_win_bid/sp_lasso.txt')

# HC1 SE
lm_sp_lasso <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + 
                    kg_por_unid + qualidade + bimestre + municipio + 
                    unidade_compradora_lasso + marca_vencedor_principais,
                  data = dd_data_list$dd_sp)

df_std_sp_lasso <- PregoesBR::get_robust_std_errors(lm_sp_lasso, HC = 'HC1')

saveRDS(df_std_sp_lasso, 'results/log_win_bid/HC1_std_sp_lasso.rds')

# Lasso: DD Brasil ------------------------------------------------------------
df_models_brasil_lasso <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso'),
      str_c(form, ' + qualidade                             | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list$dd_brasil))) # <<<<

stargazer(df_models_brasil_lasso$models, type = 'text',
          out = 'results/log_win_bid/brasil_lasso.txt')

# HC1 SE
lm_brasil_lasso <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade 
                      + kg_por_unid + arab_defl + bimestre + sigla_uf:bimestre
                      + municipio + unidade_compradora_lasso,
                      data = dd_data_list$dd_brasil)

df_std_brasil_lasso <- PregoesBR::get_robust_std_errors(lm_brasil_lasso, HC = 'HC1')

saveRDS(df_std_brasil_lasso, 'results/log_win_bid/HC1_std_brasil_lasso.rds')


# TESTANDO ANTECIPAÇÃO: Publicação em 2013-10-07 ------------------------------
# Montando bases DD em uma lista ----------------------------------------------
dd_data_list_pub <- list(data_list$cnet, data_list$cnet_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$bec) %>%
        PregoesBR::build_dd_df_3treat() # <<<<
  ) %>% set_names(c('dd_brasil', 'dd_sp'))

# Antecipação: DD SP ----------------------------------------------------------
form <- 'log_win_bid ~ comprasnet + treat1 + treat_pub + treat2'

df_models_sp_pub <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora'),
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade                             | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora + municipio + marca_vencedor_principais')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list_pub$dd_sp))) # <<<<

stargazer(df_models_sp_pub$models, type = 'text',
          out = 'results/log_win_bid/sp_pub.txt')

# HC1 SE
lm_sp_pub <- lm(log_win_bid ~ comprasnet + treat1 + treat_pub + treat2 +
              arab_defl + kg_por_unid + qualidade + bimestre + municipio + 
              unidade_compradora + marca_vencedor_principais,
            data = dd_data_list_pub$dd_sp)

df_std_sp_pub <- PregoesBR::get_robust_std_errors(lm_sp_pub, HC = 'HC1')

saveRDS(df_std_sp_pub, 'results/log_win_bid/HC1_std_sp_pub.rds')


# Antecipação: DD Brasil ------------------------------------------------------
df_models_brasil_pub <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade                             | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + sigla_uf:bimestre + municipio + unidade_compradora')),
  models = map(.x = formula,
               .f = ~ felm(as.formula(.x),
                           data = dd_data_list_pub$dd_brasil))) # <<<<

stargazer(df_models_brasil_pub$models, type = 'text',
          out = 'results/log_win_bid/brasil_pub.txt')

# HC1 SE
lm_brasil_pub <- lm(log_win_bid ~ comprasnet + treat1 + treat_pub
                + treat2 + qualidade 
                + kg_por_unid + arab_defl + bimestre + sigla_uf:bimestre
                + municipio + unidade_compradora,
                data = dd_data_list_pub$dd_brasil)

df_std_brasil_pub <- PregoesBR::get_robust_std_errors(lm_brasil_pub, HC = 'HC1')

saveRDS(df_std_brasil_pub, 'results/log_win_bid/HC1_std_brasil_pub.rds')



# Tendencias à la Chimeli-Soares: efeito do tratamento ao longo dos meses -----
dd_treat_trends_sp <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + qualidade + 
         treat1_trend_bimestre + treat2_trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp) # <<<<

dd_treat_trends_brasil <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + qualidade + 
         kg_por_unid + treat1_trend_bimestre + treat2_trend_bimestre | 
         bimestre + sigla_uf:bimestre + unidade_compradora + municipio,
       data = dd_data_list$dd_brasil) # <<<<

stargazer(dd_treat_trends_sp,
          dd_treat_trends_brasil,
          type = 'text')
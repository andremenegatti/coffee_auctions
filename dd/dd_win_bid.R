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

stargazer(df_models_sp$models, type = 'text')

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

stargazer(df_models_brasil$models, type = 'text')

# Robust Standard Errors ------------------------------------------------------
lm_sp <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + 
              kg_por_unid + qualidade + bimestre + municipio + 
              unidade_compradora + marca_vencedor_principais,
            data = dd_data_list$dd_sp)

lm_brasil <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade 
                + kg_por_unid + arab_defl + bimestre + sigla_uf:bimestre
                + municipio + unidade_compradora,
                data = dd_data_list$dd_brasil)

df_std_sp <- PregoesBR::get_robust_std_errors(lm_sp, HC = 'HC1')
df_std <- PregoesBR::get_robust_std_errors(lm_brasil, HC = 'HC1')

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
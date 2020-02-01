library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
# Principais
bec_cafe <- readRDS('data/bec_cafe_dd.rds')
cnet_cafe <- readRDS('data/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('data/cnet_cafe_sp_dd.rds')

# Importando DFs com UASGs selecionadas
selected_uasgs_list <- list(
  readRDS('data/bec_selected_uasgs_soft_trim.rds'),
  readRDS('data/cnet_selected_uasgs_soft_trim.rds'),
  readRDS('data/cnet_sp_selected_uasgs_soft_trim.rds')
  ) %>%
  # Selecionando apenas colunas desejadas
  map(.f = ~ select(.x, id_item,unidade_compradora_lasso = lasso) %>%
        mutate(unidade_compradora_lasso = 
                 as.character(unidade_compradora_lasso))) %>% 
  set_names('bec_selected_uasgs', 'cnet_selected_uasgs', 'cnet_sp_selected_uasgs')

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
               marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2
               ) %>%
        # Coercing to factor to avoid warnings when joining dataframes
        mutate_if(is.factor, as.character) %>%
        filter(kg_por_unid != 0.25)
      ) %>%
  # Incluindo coluna com uasgs selecionadas
  map2(
    .y = selected_uasgs_list,
    .f = ~ left_join(.x, .y, by = 'id_item')
    ) %>%
  # Dando nomes aos DFs
  set_names(c('bec_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

# Montando bases DD -----------------------------------------------------------
dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$bec_cafe) %>%
        PregoesBR::build_dd_df()
      ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

# Salvando como .dta para checar no Stata -------------------------------------
# dd_data_list$sp_cafe_dd %>%
#   mutate(id_item = factor(id_item)) %>%
#   select(-inicio_ano, -inicio_bimestre, -inicio_mes, -inicio_semana) %>%
#   haven::write_dta('sp_cafe_dd.dta')

# DD --------------------------------------------------------------------------
attach(dd_data_list$sp_cafe_dd)
detach(dd_data_list$sp_cafe_dd)

# Log
felm1_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre)
felm2_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso)
felm3_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso + municipio)
felm4_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)
felm5_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)
felm6_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)
felm7_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + arab_defl | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)
felm8_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)
felm9_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)

felm_log_trend <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora_lasso + municipio + marca_vencedor_principais)


stargazer(felm1_log,
          felm2_log,
          felm3_log,
          felm4_log,
          felm5_log,
          felm6_log,
          felm7_log,
          felm8_log,
          felm9_log,
          felm_log_trend,
          type = 'text')


# UF e UF x Bimestre
detach(dd_data_list$sp_cafe_dd)
attach(dd_data_list$full_cafe_dd)

felm_log_full1  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre)
felm_log_full2  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre)
felm_log_full3  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre + municipio)
felm_log_full4  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso)
felm_log_full5  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso)
felm_log_full6  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora_lasso)
felm_log_full7  <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora_lasso)
felm_log_full8 <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso)
felm_log_full9 <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora_lasso + marca_vencedor_principais)

felm_log_full_trend <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | bimestre  + sigla_uf:bimestre + municipio + unidade_compradora_lasso)

stargazer(felm_log_full1,
          felm_log_full2,
          felm_log_full3,
          felm_log_full4,
          felm_log_full5,
          felm_log_full6,
          felm_log_full7,
          felm_log_full8,
          felm_log_full9,
          felm_log_full_trend,
          type = 'text')


# Robust
lm_base <- lm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_defl + kg_por_unid + qualidade + bimestre + municipio + unidade_compradora + marca_vencedor_principais)
lm_log <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_defl + kg_por_unid + qualidade + bimestre + municipio + unidade_compradora + marca_vencedor_principais)
lm_log_uf <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
                data = dd_data_list$full_cafe_dd)


get_robust_std_errors(lm_base, HC = 'HC1')

get_robust_std_errors(lm_log, HC = 'HC1')$p_value[3:4]
df_std <- get_robust_std_errors(lm_log_uf, HC = 'HC1')


# Residuals
felm2_res <- felm(res_EF_uasgs ~ comprasnet + treat1 + treat2 | mes)
felm3_res <- felm(res_EF_uasgs ~ comprasnet + treat1 + treat2 | mes + marca_vencedor_principais)
felm4_res <- felm(res_EF_uasgs ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + marca_vencedor_principais)
felm5_res <- felm(res_EF_uasgs ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + marca_vencedor_principais | 0 | comprasnet)
felm6_res <- felm(res_EF_uasgs ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + marca_vencedor_principais + municipio | 0 | comprasnet)


# Teste de robustez: tendencias especificas para cada grupo
dd_group_trend <- lm(
  win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora
  + marca_vencedor_principais + marca_vencedor_principais:comprasnet
  + futuro_defl + arab_rob_defl + group_trend
  )

# Tendencias a la Ariaster Chimeli: efeito do tratamento ao longo dos meses
dd_treat_trends <- lm(
  formula =
    win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora
  + marca_vencedor_principais + marca_vencedor_principais:comprasnet
  + futuro_defl + arab_rob_defl
  + treat_trend1 + treat_trend2
  )

stargazer(dd_group_trend,
          dd_treat_trends,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_", "comprasnet:"),
          omit.stat = c("ser", "adj.rsq", "f"),
          decimal.mark = ",",
          digit.separator = ".",
          type = 'text')

library(stargazer)
library(tidyverse)
library(PregoesBR)
library(lfe)

#### ABRINDO BASES ####
BEC_cafe <- readRDS('bec_cafe_dd.rds') %>%
  mutate(sigla_uf = 'SP')
cnet_cafe <- readRDS('cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('cnet_cafe_sp_dd.rds')

#### MONTANDO BASES DD ####
data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        # EXCLUINDO OBSERVACOES POSTERIORES A 2016
        filter(abertura_lances < '2016-01-01') %>%
        # TRIMMING: 1.25% de cada lado ## <--------
        trim_df('win_bid', perc = 2.5) %>%
        select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               win_bid, comprasnet, municipio, unidade_compradora, marca_vencedor_principais,
               futuro, arab_rob, arab, rob, futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               sigla_uf) %>%
        mutate(marca_vencedor_principais = as.character(marca_vencedor_principais)) %>%
        # Residuos de 1o estagio: win_bid ~ unidade_compradora
        add_1st_stage_residuals(formula = 'win_bid ~ unidade_compradora', residuals_column_name = 'res_EF_uasgs')
      ) %>% set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

#### SELECAO UASGS ####
# Importando DFs com UASGs selecionadas segundo diferentes criterios
BEC_selected_uasgs <- readRDS('bec_selected_uasgs_2015.rds')
cnet_selected_uasgs <- readRDS('cnet_selected_uasgs_2015.rds')
cnet_sp_selected_uasgs <- readRDS('cnet_sp_selected_uasgs_2015.rds')

# Funcao para contar UASGs selecionadas e nao selecionadas
checar_selecao <- function(df) {
  df %>%
    select(-id_item, -num_unidade_compradora) %>%
    mutate_all(.funs = ~ ifelse(. == 'Outra', 'Nao selecionado', 'Selecionado')) %>%
    gather(key = 'metodo_selecao', value = 'resultado_selecao') %>%
    count(metodo_selecao, resultado_selecao)
}

# Checando selecao
map(.x = list(BEC_selected_uasgs, cnet_selected_uasgs, cnet_sp_selected_uasgs),
    .f = ~ checar_selecao(.x)) %>% set_names('BEC', 'cnet_full', 'cnet_SP')

# Substituindo coluna 'unidade_compradora' por dados selecionados
data_list <- data_list %>%
  map2(.y = list(BEC_selected_uasgs, cnet_selected_uasgs, cnet_sp_selected_uasgs),
       .f = ~ .x %>%
         left_join(.y %>%
                     # Alterar de acordo com o criterio de selecao a ser utilizado
                     select(id_item, unidade_compradora_lasso = lasso) %>%
                     mutate(unidade_compradora_lasso = as.character(unidade_compradora_lasso)),
                   by = 'id_item')
       )

#### MONTANDO BASES DD ####
dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$BEC_cafe) %>%
        build_dd_df() %>%
        mutate_if(is.character, as.factor) %>%
        mutate(log_win_bid = log(win_bid)) %>%
        mutate(comprasnet_factor = factor(comprasnet, levels = c(1, 0), labels = c('cnet', 'bec')))
      ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

# Salvando como .dta para checar no Stata
# dd_data_list$sp_cafe_dd %>%
#   mutate(id_item = factor(id_item)) %>%
#   select(-inicio_ano, -inicio_bimestre, -inicio_mes, -inicio_semana) %>%
#   haven::write_dta('C:/Users/Dell/Desktop/sp_cafe_dd.dta')

#### DD ####
attach(dd_data_list$sp_cafe_dd)
detach(dd_data_list$sp_cafe_dd)

# Nivel
felm1 <- felm(win_bid ~ comprasnet + treat1 + treat2 | mes)
felm2 <- felm(win_bid ~ comprasnet + treat1 + treat2 | mes + unidade_compradora_lasso)
felm3 <- felm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl| mes + unidade_compradora_lasso + marca_vencedor_principais)
felm4 <- felm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + unidade_compradora_lasso + marca_vencedor_principais + municipio)
felm5 <- felm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + unidade_compradora_lasso + marca_vencedor_principais | 0 | comprasnet) # Cluster
felm6 <- felm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl | mes + unidade_compradora + marca_vencedor_principais | 0 | unidade_compradora) # Cluster

# Log
felm_t1_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre)
felm_t2_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso)
felm_t3_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso + marca_vencedor_principais)
felm_t4_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso + marca_vencedor_principais + municipio)
felm_t5_log <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + comprasnet:trend_bimestre | bimestre + unidade_compradora_lasso + marca_vencedor_principais + municipio)

stargazer(felm_t1_log,
          felm_t2_log,
          felm_t3_log,
          felm_t4_log,
          felm_t5_log,
          type = 'text')

# UF e UF x Bimestre
felm_uf <- felm(log_win_bid ~ comprasnet + treat1 + treat2 | bimestre + unidade_compradora_lasso + marca_vencedor_principais + municipio + sigla_uf + sigla_uf:bimestre,
                data = dd_data_list$full_cafe_dd)

felm_uf_trend <- felm(log_win_bid ~ comprasnet + treat1 + treat2 + comprasnet:trend_bimestre | bimestre + unidade_compradora_lasso + marca_vencedor_principais + municipio + sigla_uf + sigla_uf:bimestre,
                data = dd_data_list$full_cafe_dd)

stargazer(felm_uf,
          felm_uf_trend,
          type = 'text')

# Robust
lm_base <- lm(win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl + bimestre + municipio + unidade_compradora_lasso + marca_vencedor_principais)
lm_log <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl + bimestre + municipio + unidade_compradora_lasso + marca_vencedor_principais)
lm_log_uf <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + futuro_defl + arab_rob_defl + bimestre + municipio + unidade_compradora_lasso + marca_vencedor_principais + sigla_uf + sigla_uf:bimestre,
                data = dd_data_list$full_cafe_dd)


get_robust_std_errors(lm_base, HC = 'HC1')
get_robust_std_errors(lm_log, HC = 'HC1')
get_robust_std_errors(lm_log_uf, HC = 'HC1')


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


#### NESTED MODELS ####

formulas <- c(
  'win_bid ~ comprasnet + mes + treat1 + treat2',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais + marca_vencedor_principais:comprasnet',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais + marca_vencedor_principais:comprasnet + futuro_defl',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais + marca_vencedor_principais:comprasnet + arab_rob_defl',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais + marca_vencedor_principais:comprasnet + futuro_defl + arab_rob_defl',
  'win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora + marca_vencedor_principais + marca_vencedor_principais:comprasnet + futuro_defl + arab_rob_defl + futuro_defl:comprasnet + arab_rob_defl:comprasnet'
)

model_names <- c(
  'uncontrolled',
  'uasg',
  'uasg_marca',
  'marca_int',
  'futuro',
  'cepea',
  'futuro_cepea',
  'futuro_cepea_int'
)

dd_models <- map(.x = formulas,
                 .f = ~ lm(formula = .x, data = dd_data_list$sp_cafe_dd)) %>%
  set_names(model_names)

stargazer(dd_models,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_", "comprasnet:"),
          omit.stat = c("ser", "adj.rsq", "f"),
          decimal.mark = ",",
          digit.separator = ".",
          type = 'text')



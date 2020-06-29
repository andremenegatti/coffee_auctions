library(tidyverse)

# Carregando base de dados
dd_brasil <- readRDS('data/dd_brasil.rds') %>% 
  filter(!is.na(num_forn_lances))

# Attaching DF para simplificar o codigo
attach(dd_brasil)

# Rodando Modelos Placebo -----------------------------------------------------
df_placebo <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>%
  mutate(
    treat1 = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x,
        1, 0)
    ),
    treat_placebo = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= .x & abertura_lances < data_3s,
        1, 0)
    ),
    treat2 = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= data_3s, 1, 0)
    )
  )

glimpse(df_placebo)

for (i in 1:nrow(df_placebo)) {
  
  model <- 
    lm(log_win_bid ~ df_placebo$treat1[[i]] + df_placebo$treat_placebo[[i]] +
         df_placebo$treat2[[i]] + qualidade + kg_por_unid + futuro_defl + 
         arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
  
  robust_se <- PregoesBR::get_robust_std_errors(model)
  
  saveRDS(robust_se, str_c('dd/placebo/data/final_brasil/',
                           as.character(df_placebo$data_placebo[[i]]),
                           '.rds'))
  
  rm(model, robust_se)
  
}
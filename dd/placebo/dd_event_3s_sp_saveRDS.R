library(tidyverse)

# Carregando base de dados
dd_sp <- readRDS('data/dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Rodando Modelos Placebo -----------------------------------------------------
df_event <-
  tibble(data_treat2 = seq(-420, 420, by = 3) + data_3s) %>%
  mutate(
    treat1 = map(
      .x = data_treat2,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x,
        1, 0)
    ),
    treat2 = map(
      .x = data_treat2,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= .x,
        1, 0)
    )
  )

for (i in 1:nrow(df_event)) {
  
  message(str_c('Fitting model ', i, '/', nrow(df_event)))
  
  model <- 
    lm(log_win_bid ~ df_event$treat1[[i]] + df_event$treat2[[i]] +
         qualidade + kg_por_unid + 
         arab_defl + bimestre + unidade_compradora + 
         municipio + marca_vencedor_principais)
  
  message('Estimating HC1 standard errors')
  robust_se <- PregoesBR::get_robust_std_errors(model)
  
  filename <- str_c('dd/placebo/data/event_sp/',
                    as.character(df_event$data_treat2[[i]]),
                    '.rds')
  
  message('Writing ', filename, '\n')
  
  saveRDS(robust_se, filename)
  
  rm(model, robust_se)
  
}





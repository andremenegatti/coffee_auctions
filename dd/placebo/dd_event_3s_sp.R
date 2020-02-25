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




df_event <-
  tibble(data_placebo = seq(-420, 420, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(.x = data_placebo,
               .f = ~ str_c('dd/placebo/data/event_sp/',
                            as.character(.x), '.rds') %>% readRDS()))

df_actual_treatment <- df_event %>% 
  filter(data_placebo == data_3s)

actual_treat2_effect <- df_actual_treatment$model_list[[1]]$estimate[3]

df_event <- df_event %>% 
  mutate(treat1_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[2]),
         treat2_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[3]),
         treat1_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[3]))

# Computando intervalos de confiança 95%
df_event <- df_event %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se)

df_event <- df_event %>% 
  mutate(t = seq(-420, 420, by = 3),
         treat2_norm = treat2_est / actual_treat2_effect,
         treat2_norm2 = (treat2_est - actual_treat2_effect) / actual_treat2_effect)

df_event %>% filter(data_placebo == as.Date('2013-10-7')) %>% select(t)

ggplot(df_event) +
  geom_line(aes(x = t, y = treat2_norm2, group = 1)) +
  geom_vline(xintercept = 0, col = 'steelblue') +
  geom_vline(xintercept = -87, col = 'darkred')


library(tidyverse)

# Carregando resultados em um dataframe ---------------------------------------
df_event <-
  tibble(data_placebo = seq(-420, 420, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(.x = data_placebo,
               .f = ~ str_c('dd/placebo/data/event_sp/',
                            as.character(.x), '.rds') %>% readRDS()))

# Salvando resultado para o tratamento real -----------------------------------
df_actual_treatment <- df_event %>% 
  filter(data_placebo == data_3s)

actual_treat2_effect <- df_actual_treatment$model_list[[1]]$estimate[3]

# Extraindo resultados coeficientes e erros-padrão ----------------------------
df_event <- df_event %>% 
  mutate(treat1_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[2]),
         treat2_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[3]),
         treat1_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[3]))

# Computando intervalos de confiança 95% --------------------------------------
df_event <- df_event %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se)

# Normalizando efeito ---------------------------------------------------------
df_event <- df_event %>% 
  mutate(t = seq(-420, 420, by = 3),
         treat2_norm = treat2_est / actual_treat2_effect,
         treat2_norm2 = 
           (treat2_est - actual_treat2_effect) / actual_treat2_effect)

# Período t da publicacao Regra 3s --------------------------------------------
t_publicacao <- df_event %>% 
  filter(data_placebo == as.Date('2013-10-7')) %>%
  pull(t)

# Gráfico ---------------------------------------------------------------------
ggplot(df_event) +
  geom_line(aes(x = data_placebo, y = treat2_norm2 * 100, group = 1)) +
  geom_vline(xintercept = c(data_3s, as.Date('2013-10-07')),
             col = 'gray25', linetype = 'dotted') +
  geom_label(x = data_3s, y = -25,
             label = '02/01/2014:\nImplementação\nRegra 3s',
             size = 2.5, family = 'serif') +
  geom_label(x = as.Date('2013-10-07'), y = -50,
             label = '07/10/2013:\nPublicação\nRegra 3s',
             size = 2.5, family = 'serif') +
  scale_y_continuous(breaks = seq(-60, 30, by = 10),
                     labels = function(x) str_c(x, '%')) +
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%Y') +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
  labs(
    x = 'Data do tratamento',
    y = 'Diferença relativa ao efeito estimado para 02/01/2014'
  )

ggsave('plots/event_plot_3s_sp.png', width = 6, height = 6)

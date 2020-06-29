library(tidyverse)

# Brasil ----------------------------------------------------------------------
# Carregando resultados em um dataframe
df_placebo_br <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(data_placebo,
               .f = ~ str_c('dd/placebo/data/final_brasil/',
                            as.character(.x), '.rds') %>% readRDS()))

# Extraindo coeficientes e erros-padrão
df_placebo_br <- df_placebo_br %>% 
  mutate(treat1_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[2]),
         treat2_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[4]),
         treat_placebo_est = map_dbl(.x = model_list,
                                     .f = ~ .x$estimate[3]),
         treat1_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[4]),
         treat_placebo_se = map_dbl(.x = model_list,
                                    .f = ~ .x$std_error[3]))

# Computando intervalos de confiança 95%
df_placebo_br <- df_placebo_br %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)

# ID
df_placebo_br$amostra <- 'Amostra completa'

# SP --------------------------------------------------------------------------
# Carregando resultados em um dataframe
df_placebo_sp <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(.x = data_placebo,
               .f = ~ str_c('dd/placebo/data/final_sp/',
                            as.character(.x), '.rds') %>% readRDS()))

# Extraindo coeficientes e erros-padrão
df_placebo_sp <- df_placebo_sp %>% 
  mutate(treat1_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[2]),
         treat2_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[3]),
         treat_placebo_est = map_dbl(.x = model_list,
                                     .f = ~ .x$estimate[4]),
         treat1_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[3]),
         treat_placebo_se = map_dbl(.x = model_list,
                                    .f = ~ .x$std_error[4]))

# Computando intervalos de confiança 95%
df_placebo_sp <- df_placebo_sp %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)

# ID
df_placebo_sp$amostra <- 'Apenas Estado de São Paulo'

# Joining ---------------------------------------------------------------------
df_placebo_full <- bind_rows(df_placebo_br, df_placebo_sp)

# Plotting --------------------------------------------------------------------
ggplot(df_placebo_full, aes(x = data_placebo, group = 1)) +
  geom_line(aes(y = treat_placebo_est), color = 'black') +
  geom_ribbon(aes(ymin = treat_placebo_lower, ymax = treat_placebo_upper),
              fill = 'gray', alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date('2013-10-07'),
             col = 'black', alpha = 0.7, linetype = 'dotted') +
  geom_label(x = as.Date('2013-10-07'),
             y = 0.85, label = '07/10/2013:\nPublicação da\nRegra dos 3s',
             size = 3, family = 'serif', col = 'gray25') +
  scale_x_date(breaks = as.Date(c('2012-12-01',
                                  '2013-02-01', '2013-04-01',
                                  '2013-06-01', '2013-08-01',
                                  '2013-10-01', '2013-12-01')),
               labels = c('Dez/12', 'Fev/13', 'Abr/13', 'Jun/13',
                          'Ago/13', 'Out/13', 'Dez/13')) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente do tratamento placebo',
    title = 'Efeito de Tratamento Placebo Anterior à Regra dos 3s',
    subtitle = 'Unidades compradoras de todo o Brasil'
  ) +
  facet_wrap(~ amostra, nrow = 2) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())

# Saving ----------------------------------------------------------------------
ggsave('plots/placebo_3s_faceted_brasil_sp.png', width=6, height = 7)
ggsave('~/Documents/dissertacao/images/placebo_3s_faceted_brasil_sp.png',
       width=6, height = 7)

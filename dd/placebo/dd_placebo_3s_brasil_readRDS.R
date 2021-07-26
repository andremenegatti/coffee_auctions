library(tidyverse)

# Carregando resultados em um dataframe ---------------------------------------
df_placebo <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(data_placebo,
               .f = ~ str_c('dd/placebo/data/final_brasil/',
                            as.character(.x), '.rds') %>% readRDS()))


# Extraindo coeficientes e erros-padrão ---------------------------------------
df_placebo <- df_placebo %>% 
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

# Computando intervalos de confiança 95% --------------------------------------
df_placebo <- df_placebo %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)

# Plotting: efeito do tratamento placebo --------------------------------------
ggplot(df_placebo, aes(x = data_placebo, group = 1)) +
  geom_line(aes(y = treat_placebo_est), color = 'black') +
  geom_ribbon(aes(ymin = treat_placebo_lower, ymax = treat_placebo_upper),
              fill = 'gray', alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date('2013-10-07'),
             col = 'black', alpha = 0.7, linetype = 'dotted') +
  geom_label(x = as.Date('2013-10-07'),
             y = 0.65, label = '07/10/2013:\nPublicação da\nRegra dos 3s',
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
  )

ggsave('plots/placebo_3s_brasil.png', width = 6, height = 5)

# Plotting: dois painéis: Placebo + 3s ----------------------------------------
# Finalizando organização do dataframe para construir o gráfico
df_plot <- df_placebo %>%
  select(-model_list) %>% 
  gather(key = 'treat_var',
         value = 'coefficient',
         treat1_est:treat_placebo_lower) %>%
  mutate(grupo = str_remove(treat_var, 'treat|treat_') %>%
           str_remove('_est|_se|_lower|_upper') %>%
           str_replace('_placebo', 'Tratamento Placebo') %>%
           str_replace('2', 'Tratamento Regra 3s'))

# Gráfico
ggplot(mapping = aes(x = data_placebo, group = grupo)) +
  geom_line(data = df_plot %>% 
              filter(treat_var %in% c('treat2_est', 'treat_placebo_est')),
            mapping = aes(y = coefficient),
            color = 'black') +
  geom_ribbon(data = df_plot %>% 
                filter(treat_var %in% c('treat2_lower', 'treat2_upper')) %>% 
                spread(treat_var, coefficient),
              mapping = aes(ymin = treat2_lower, ymax = treat2_upper),
              fill = "gray", alpha = 0.5) +
  geom_ribbon(data = df_plot %>%
                filter(treat_var %in% c('treat_placebo_lower',
                                        'treat_placebo_upper')) %>% 
                spread(treat_var, coefficient),
              mapping = aes(ymin = treat_placebo_lower,
                            ymax = treat_placebo_upper),
              fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ grupo, ncol = 2) +
  # scale_x_date(breaks = as.Date(c('2012-05-01', '2012-11-01',
  #                                 '2013-05-01', '2013-11-01')),
  #              labels = c('Maio/2012', 'Nov/2012', 'Maio/2013', 'Nov/2013')) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente estimado'
    # title = 'Teste Placebo - Efeito da Regra dos 3s - Amostra completa',
    # subtitle = 'Introdução de um tratamento placebo anterior à regra dos 3s'
  )

ggsave('plots/brasil_placebo_3s_pre_2paineis.png', width = 9, height = 6)

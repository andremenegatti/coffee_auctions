library(tidyverse)
library(PregoesBR)
# Outros pacotes necessarios: broom, lfe, viridis, htmlwidgets

# Executando script auxiliar
source('DD/Placebo/helpers_placebo.R')

# Carregando base de dados
dd_full <- readRDS('base_dd_full.rds')

# Attaching DF para simplificar o codigo
attach(dd_full)
# detach(dd_full)
# Opcional: Definindo tema para graficos mais bonitos
# theme_set(my_theme())

# ----------------------------- #
#### RODANDO MODELOS PLACEBO ####
# ----------------------------- #


df_placebo_pre <-
  tibble(data_placebo = seq(-615, -30, by = 15) + data_3s) %>%
  mutate(
    treat1 = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x, 1, 0)
    ),
    treat_placebo = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= .x & abertura_lances < data_3s, 1, 0)
    ),
    treat2 = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= data_3s, 1, 0)
    )
  ) %>%
  mutate(
    model_summary = pmap(
      .l = list(treat1, treat2, treat_placebo),
      .f = ~ lm(log_win_bid ~ ..1 + ..2 + ..3 + qualidade + kg_por_unid + futuro_defl + arab_defl + bimestre + sigla_uf:bimestre + municipio + unidade_compradora)
    )
  )


df_placebo_pre <- df_placebo_pre %>%
  rename(model = model_summary) %>%
  mutate(treat1_est = map_dbl(.x = model, .f = ~ coef(.x)[2]),
         treat2_est = map_dbl(.x = model, .f = ~ coef(.x)[3]),
         treat_placebo_est = map_dbl(.x = model, .f = ~ coef(.x)[4]))

df_placebo_pre <- df_placebo_pre %>%
  mutate(standard_errors = map(.x = model, .f = ~ get_robust_std_errors(.x))) %>%
  mutate(treat1_se = map_dbl(.x = standard_errors, .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = standard_errors, .f = ~ .x$std_error[3]),
         treat_placebo_se = map_dbl(.x = standard_errors, .f = ~ .x$std_error[4]))

df_placebo_pre <- df_placebo_pre %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)



df_plot_pre <-  df_placebo_pre %>%
  select(-model, -standard_errors, -treat1, -treat2, -treat_placebo) %>%
  filter(data_placebo < data_3s) %>%
  gather(key = 'treat_var', value = 'coefficient', treat1_est:treat_placebo_lower) %>%
  mutate(grupo = str_remove(treat_var, 'treat|treat_') %>%
           str_remove('_est|_se|_lower|_upper') %>%
           str_replace('_placebo', 'Tratamento Placebo') %>%
           str_replace('2', 'Tratamento Regra 3s'))

saveRDS(df_plot_pre, 'df_placebo_plot_pre_full.rds')

placebo_plot <-
  ggplot(mapping = aes(x = data_placebo, group = grupo)) +
  geom_line(data = df_plot_pre %>% filter(treat_var %in% c('treat2_est', 'treat_placebo_est')),
            mapping = aes(y = coefficient),
            color = 'black') +
  geom_ribbon(data = df_plot_pre %>% filter(treat_var %in% c('treat2_lower', 'treat2_upper')) %>% spread(treat_var, coefficient),
              mapping = aes(ymin = treat2_lower, ymax = treat2_upper),
              fill = "gray", alpha = 0.5) +
  geom_ribbon(data = df_plot_pre %>% filter(treat_var %in% c('treat_placebo_lower', 'treat_placebo_upper')) %>% spread(treat_var, coefficient),
              mapping = aes(ymin = treat_placebo_lower, ymax = treat_placebo_upper),
              fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ grupo, ncol = 2) +
  scale_x_date(breaks = as.Date(c('2012-05-01', '2012-11-01', '2013-05-01', '2013-11-01')),
               labels = c('Maio/2012', 'Nov/2012', 'Maio/2013', 'Nov/2013')) +
  scale_y_continuous(labels = function(x) formatC(x, big.mark = '.', decimal.mark = ',')) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente estimado',
    title = 'Teste Placebo - Efeito da Regra dos 3s - Amostra Completa',
    subtitle = 'Introdução de um tratamento placebo anterior à regra dos 3s'
    # caption = 'Notas:
    # 1) Resultados de modelos considerando datas alternativas para o tratamento placebo;
    # 2) Total de 60 placebos, de 22/04/2012 a 03/12/2013, intervalados em 10 dias;
    # 3) As áreas sombreadas representam intervalos de confiança de 95% (erros padrão HC1)'
    )

ggsave(plot = placebo_plot, filename = 'C:/Users/Dell/Desktop/placebo_3s_pre_full.png', width = 9, height = 6)

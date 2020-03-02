library(tidyverse)

# Carregando base de dados
dd_sp <- readRDS('data/dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Rodando Modelos Placebo -----------------------------------------------------
df_placebo <-
  tibble(data_placebo = seq(30, 620, by = 10) + data_3s) %>%
  mutate(
    treat1 = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s,
        1, 0)
    ),
    treat_placebo = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= .x, 1, 0)
    ),
    treat2 = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= data_3s & abertura_lances < .x,
        1, 0)
    )
  ) %>%
  mutate(
    model_summary = pmap(
      .l = list(treat1, treat2, treat_placebo),
      .f = ~ lm(log_win_bid ~ ..1 + ..2 + ..3 + qualidade + kg_por_unid + 
                  arab_defl + bimestre + unidade_compradora + 
                  municipio + marca_vencedor_principais)
    )
  )

# Extraindo e organizando resultados ------------------------------------------
# Extraindo coeficientes estimados
df_placebo <- df_placebo %>%
  rename(model = model_summary) %>%
  mutate(treat1_est = map_dbl(.x = model, .f = ~ coef(.x)[2]),
         treat2_est = map_dbl(.x = model, .f = ~ coef(.x)[3]),
         treat_placebo_est = map_dbl(.x = model, .f = ~ coef(.x)[4]))

# Computando erros-padrão robustos à heteroscedasticidade
df_placebo <- df_placebo %>%
  mutate(standard_errors = 
           map(.x = model,
               .f = ~ PregoesBR::get_robust_std_errors(.x))) %>%
  mutate(treat1_se = map_dbl(.x = standard_errors, .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = standard_errors, .f = ~ .x$std_error[3]),
         treat_placebo_se = map_dbl(.x = standard_errors,
                                    .f = ~ .x$std_error[4]))

# Computando intervalos de confiança 95%
df_placebo <- df_placebo %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)

# Selecionando apenas variaveis de interesse e salvando
df_placebo_results <- df_placebo %>% 
  select(-model, -standard_errors, -treat1, -treat2, -treat_placebo)

saveRDS(df_placebo_results, 'dd/placebo/data/placebo_post_3s_sp_results.rds')

# Plotting: efeito do tratamento placebo --------------------------------------
ggplot(df_placebo, aes(x = data_placebo, group = 1)) +
  geom_line(aes(y = treat_placebo_est), color = 'black') +
  geom_ribbon(aes(ymin = treat_placebo_lower, ymax = treat_placebo_upper),
              fill = 'gray', alpha = 0.5) +
  geom_hline(yintercept = 0) +
  scale_x_date(breaks = as.Date(c('2014-03-01', '2014-09-01',
                                  '2015-03-01', '2015-09-01')),
               labels = c('Mar/2014', 'Set/2014', 'Mar/2015', 'Set/2015')) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente estimado para o tratamento placebo',
    title = 'Efeito de Tratamento Placebo Posterior à Regra dos 3s',
    subtitle = 'Apenas São Paulo',
    caption = 'Notas:
    1) Resultados de modelos considerando datas alternativas para o tratamento placebo;
    2) Total de 60 placebos, de 01/02/2014 a 14/09/2014, intervalados em 10 dias;
    3) As áreas sombreadas representam intervalos de confiança de 95% (erros padrão HC1)'
  )

ggsave('plots/placebo/sp_placebo_3s_post.png', width = 6, height = 5)

# Plotting: dois painéis: Placebo + 3s ----------------------------------------
# Finalizando organização do dataframe para construir o gráfico
df_plot <- df_placebo_results %>%
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
  scale_x_date(breaks = as.Date(c('2014-03-01', '2014-09-01',
                                  '2015-03-01', '2015-09-01')),
               labels = c('Mar/2014', 'Set/2014', 'Mar/2015', 'Set/2015')) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente estimado',
    title = 'Teste Placebo - Efeito da Regra dos 3s - Apenas SP',
    subtitle = 'Introdução de um tratamento placebo posterior à regra dos 3s',
    caption = 'Notas:
    1) Resultados de modelos considerando datas alternativas para o tratamento placebo;
    2) Total de 60 placebos, de 01/02/2014 a 14/09/2014, intervalados em 10 dias;
    3) As áreas sombreadas representam intervalos de confiança de 95% (erros padrão HC1)'
  )

ggsave('plots/placebo/sp_placebo_3s_post_2paineis.png', width = 9, height = 6)

devtools::load_all(".")

arabica <- readxl::read_excel('CEPEA_arabica.xls', sheet = 1, range = 'A5:C1710', col_names = c('data', 'pr_arab_reais', 'pr_arab_dol')) %>%
  mutate(data = dmy(data))

robusta <- readxl::read_excel('CEPEA_robusta.xls', sheet = 1, range = 'A5:C1709', col_names = c('data', 'pr_rob_reais', 'pr_rob_dol')) %>%
  mutate(data = dmy(data))

precos_cafe_cepea <- arabica %>%
  left_join(robusta, by = 'data') %>%
  create_time_variables(time_var = data)

precos_cafe_cepea <- precos_cafe_cepea %>%
  group_by(inicio_mes) %>%
  mutate(pr_mensal_arab = mean(pr_arab_reais, na.rm = TRUE),
         pr_mensal_rob = mean(pr_rob_reais, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(inicio_bimestre) %>%
  mutate(pr_bimestral_arab = mean(pr_arab_reais, na.rm = TRUE),
         pr_bimestral_rob = mean(pr_rob_reais, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(inicio_semana) %>%
  mutate(pr_semanal_arab = mean(pr_arab_reais, na.rm = TRUE),
         pr_semanal_rob = mean(pr_rob_reais, na.rm = TRUE)) %>%
  ungroup()

saveRDS(precos_cafe_cepea, 'precos_cafe_cepea.rds')

ggplot(precos_cafe_cepea %>% gather(pr_semanal_arab:pr_semanal_rob, key = 'Variedade', value = 'Preço'),
       aes(x = inicio_semana, y = Preço, col = Variedade)) +
  geom_line()

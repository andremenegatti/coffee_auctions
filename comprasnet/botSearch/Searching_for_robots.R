library(PregoesBR)

# Leitura e preparacao dos dados
df_atas <- readRDS("Comprasnet/cnet_cafe_05_v2.rds") %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(regime_juridico = case_when(abertura_lances < data_20s ~ 1,
                                     abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
                                     abertura_lances >= data_3s ~ 3) %>%
           factor(labels = c('Sem intervalo minimo', 'Regra 20s', 'Regra 20s + Regra 3s')))


df_lances_por_fornecedor <- df_atas %>%
  select(id_item, regime_juridico, lances_clean) %>%
  filter(map_lgl(.x = lances_clean, .f = ~ nrow(.x) > 20)) %>%
  mutate(lances_forn = map(.x = lances_clean,
                           .f = ~ .x %>%
                             nest(-CNPJ_CPF) %>%
                             mutate(n_lances_forn = map_dbl(.x = data,
                                                            .f = ~ .x %>% nrow()
                                                            )
                                    ) %>%
                             arrange(desc(n_lances_forn)) %>%
                             slice(1)
                           )
  )

df_lances_por_fornecedor <- df_lances_por_fornecedor %>%
  unnest(lances_forn)

df_lances_por_fornecedor <- df_lances_por_fornecedor %>%
  rename(lances_forn_mais_ativo = data) %>%
  mutate(n_lances = map_dbl(.x = lances_clean,
                            .f = ~ nrow(.x))) %>%
  mutate(razao_max_lances = n_lances_forn/n_lances) %>%
  arrange(desc(razao_max_lances))

df_lances_por_fornecedor %>%
  filter(razao_max_lances > 0.4) %>%
  count(CNPJ_CPF) %>%
  arrange(desc(n)) %>%
  slice(1:10)

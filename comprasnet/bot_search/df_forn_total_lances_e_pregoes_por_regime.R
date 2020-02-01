library(PregoesBR)

df_lances_completo <- readRDS('Comprasnet/df_lances_completo.rds')

df_forn_lances_por_pregao <- readRDS('Comprasnet/df_forn_lances_por_pregao.rds')

# DF com total de lances e pregoes de cada fornecedor
df_forn_total_lances_e_pregoes_por_regime <- df_lances_completo %>%
  count(CNPJ_CPF, regime_juridico) %>%
  rename(n_bids = n) %>%
  arrange(desc(n_bids)) %>%
  inner_join(df_forn_lances_por_pregao %>%
               select(-n) %>%
               group_by(CNPJ_CPF) %>%
               summarise(n_auctions = n()) %>%
               ungroup() %>%
               arrange(desc(n_auctions)),
             by = 'CNPJ_CPF'
  ) %>%
  mutate(bids_auction_ratio = n_bids / n_auctions)

saveRDS(df_forn_total_lances_e_pregoes_por_regime, 'Comprasnet/df_forn_total_lances_e_pregoes_por_regime.rds')

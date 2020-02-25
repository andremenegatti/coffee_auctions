# Nota-se:
## 1) ECDF lances mensais: periodo Regra 20s + Regra 3s domina os demais.
#### Isso indica que altos numeros de lances mensais passaram a ser menos frequentes
## 2) ECDF lances por pregao: novamente, o ultimo periodo domina os demais.
#### Isso indica diminuicao do numero de lances por pregao.

### Legendas nao estao funcionando...

library(PregoesBR)

df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_lances_completo <- df_bid_inc %>%
  # filter(regime_juridico == 'Sem intervalo minimo') %>%
  select(id_item, regime_juridico, bid_increments) %>%
  unnest(cols = bid_increments)

df_forn_lances_por_pregao <- df_lances_completo %>%
  count(CNPJ_CPF, id_item, regime_juridico) %>%
  arrange(desc(n))

df_forn_total_lances_e_pregoes <- df_lances_completo %>%
  count(CNPJ_CPF, regime_juridico) %>%
  rename(n_bids = n) %>%
  arrange(desc(n_bids)) %>%
  inner_join(df_forn_lances_por_pregao %>%
               select(-n) %>%
               group_by(CNPJ_CPF, regime_juridico) %>%
               summarise(n_auctions = n()) %>%
               ungroup() %>%
               arrange(desc(n_auctions)),
             by = c('CNPJ_CPF', 'regime_juridico')
  ) %>%
  mutate(bids_auction_ratio = n_bids / n_auctions)


df_duracao_regimes <-
  tibble(regime_juridico =
           unique(df_bid_inc$regime_juridico),
         duracao_meses = c(as.numeric(difftime(time1 = data_20s,
                                               time2 = '2011-03-01',
                                               units = 'days') / 30),
                           as.numeric(difftime(time1 = data_3s,
                                               time2 = data_20s,
                                               units = 'days') / 30),
                           as.numeric(difftime(time1 = '2017-12-31',
                                               time2 = data_3s,
                                               units = 'days') / 30)))

df_forn_total_lances_e_pregoes <- df_forn_total_lances_e_pregoes %>%
  left_join(df_duracao_regimes, by = "regime_juridico") %>%
  mutate(auctions_per_month = n_auctions / duracao_meses,
         bids_per_month = n_bids / duracao_meses)


df_cdf <- df_forn_total_lances_e_pregoes %>%
  group_by(regime_juridico) %>%
  mutate(ecdf_bids = cume_dist(n_bids),
         ecdf_bids_per_month = cume_dist(bids_per_month),
         ecdf_bids_auction_ratio = cume_dist(bids_auction_ratio)) %>%
  ungroup()


# ECDF: lances por pregao
ggplot(data = df_cdf) +
  geom_line(mapping = aes(x = bids_auction_ratio,
                          y = ecdf_bids_auction_ratio,
                          linetype = regime_juridico,
                          color = regime_juridico),
            alpha = 0.7, size = 1) +
  labs(x = 'Número de lances por pregão',
       y = 'Probabilidade acumulada',
       title = 'Distribuição acumulada de lances por fornecedor em cada leilão',
       subtitle = 'Comparação entre os diferentes regimes jurídicos') +
  scale_color_discrete(name = 'Regime Jurídico') +
  scale_linetype_discrete(name = 'Regime Jurídico') +
  theme(legend.position = 'bottom')

# ECDF: lances por mes
ggplot(data = df_cdf) +
  geom_line(mapping = aes(x = bids_per_month,
                          y = ecdf_bids_per_month,
                          linetype = regime_juridico,
                          color = regime_juridico),
            alpha = 0.7, size = 1) +
  labs(x = 'Número de lances por mês',
       y = 'Probabilidade acumulada',
       title = 'Distribuição acumulada do número mensal de lances por fornecedor',
       subtitle = 'Comparação entre os diferentes regimes jurídicos') +
  coord_cartesian(xlim = c(0, 100)) +
  scale_color_discrete(name = 'Regime Jurídico') +
  scale_linetype_discrete(name = 'Regime Jurídico') +
  theme(legend.position = 'bottom')

# Nota-se:
## 1) Correlacao positiva entre numero de lances e participacao em pregoes, como esperado
## 2) Variancia do numero de lances eh maior para niveis menores de participacao em pregoes
## 3) Ausencia de mudanca na distribuicao associada as mudancas regulatorias
## 3.1) O ultimo ponto fica ainda mais evidente quando usamos o numero de pregoes por mes, em vez do numero total de pregoes

library(PregoesBR)

df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

df_lances_completo <- df_bid_inc %>%
  # filter(regime_juridico == 'Sem intervalo minimo') %>%
  select(id_item, regime_juridico, bid_increments) %>%
  unnest()

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

df_duracao_regimes <- tibble(regime_juridico = unique(df_bid_inc$regime_juridico),
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
  mutate(auctions_per_month = n_auctions / duracao_meses)

df_forn_total_lances_e_pregoes %>%
  ggplot() +
  geom_jitter(aes(x = n_bids, y = auctions_per_month),
              alpha = 0.6, shape = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = 'Pregoes vs Lances',
       subtitle = 'Comparação entre diferentes regimes',
       x = 'Número de lances',
       y = 'Número de pregões',
       caption = 'Nota: ambos os eixos estão em escala logaritmica.') +
  facet_wrap(~ regime_juridico)


# Teste com geom_hex
df_forn_total_lances_e_pregoes %>%
  filter(regime_juridico == 'Regra 20s') %>%
  ggplot() +
  geom_hex(aes(x = n_bids, y = auctions_per_month)) +
  scale_x_log10()

plotly::ggplotly()

# ggsave('scatter_auctionsPerMonth_vs_bids_comparacao_regimes.png', height = 6, width = 7)

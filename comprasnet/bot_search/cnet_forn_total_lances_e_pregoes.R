library(PregoesBR)
library(plotly)

# Importando dados
df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_lances <- readRDS('data/cnet_lances.rds')

cnet_cafe <- readRDS('data/cnet_cafe.rds') %>%
  select(id_item, inicio_fase_aleatoria)

pregoes_possivel_robo <- 
  readRDS('comprasnet/bot_search/pregoes_possivel_robo.rds')

# Data wrangling --------------------------------------------------------------
df_forn_lances_por_pregao <- df_lances %>%
  count(CNPJ_CPF, abertura_lances, id_item, regime_juridico) %>%
  arrange(desc(n))

df_forn_total_lances_e_pregoes <- df_lances %>%
  # filter(regime_juridico == 'Sem intervalo minimo') %>% # <<< 
  count(CNPJ_CPF, regime_juridico) %>%
  rename(n_bids = n) %>%
  arrange(desc(n_bids)) %>%
  inner_join(df_forn_lances_por_pregao %>%
               select(-n) %>%
               group_by(CNPJ_CPF, regime_juridico) %>%
               summarise(n_auctions = n()) %>%
               ungroup() %>%
               arrange(desc(n_auctions)),
             by = c('CNPJ_CPF', 'regime_juridico')) %>%
  mutate(bids_auction_ratio = n_bids / n_auctions)

saveRDS(df_forn_total_lances_e_pregoes,
        'comprasnet/bot_search/cnet_forn_total_lances_e_pregoes.rds')

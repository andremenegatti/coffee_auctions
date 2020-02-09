library(tidyverse)

df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

# Nos casos em que ha apenas um lance (apos o filtro increment < 0),
# tem-se NA no incremento por causa do lag; Filtro para ficar apenas
# com casos em que ha no min. 2 lances em 'bid_increments_negative'.
# ATENCAO! Esse passo pode nao ser interessante a depender do objetivo.
df_bid_inc_clean <- df_bid_inc %>%
  filter(map_lgl(.x = bid_increments_negative, .f = ~ nrow(.x) >= 1))

# Selecionando apenas incrementos negativos e unnesting
df_bid_inc_unnested <- df_bid_inc_clean %>%
  select(id_item, abertura_lances,
         regime_juridico, regime_juridico_20s, regime_juridico_3s,
         first_bid_kg_defl, last_bid_kg_defl,
         median_norm_inc_first, avg_norm_inc_first,
         bid_increments_negative) %>%
  unnest(cols = bid_increments_negative)

saveRDS(df_bid_inc_unnested, 'data/cnet_negative_bid_increments_unnested.rds')

library(tidyverse)

df_bid_inc <- readRDS('data/cnet_bid_increments.rds')
df_lances <- readRDS('data/cnet_lances.rds')

# Variância dos lances de um mesmo fornecedor ---------------------------------
df_sd <- df_lances %>%
  group_by(id_item, CNPJ_CPF) %>%
  summarise(sd_inc = sd(norm_inc_first),
            n = n()) %>%
  ungroup() %>%
  arrange(sd_inc)

df_sd <- df_sd %>%
  left_join(df_bid_inc %>% 
              select(id_item, regime_juridico, regime_juridico_3s),
            by = 'id_item') %>%
  filter(n > 0)

PregoesBR::get_summary_stats(df_sd, sd_inc, regime_juridico)

df_zero_var <- df_sd %>%
  filter(sd_inc == 0) %>%
  arrange(desc(n))

df_zero_var %>%
  group_by(regime_juridico) %>% 
  summarise(n_forn = n(),
            avg_bids_per_forn = mean(n))

zero_var_companies <- df_zero_var$CNPJ_CPF
zero_var_ids <- df_zero_var$id_item

df_bid_inc_zero_var <- df_bid_inc %>%
  filter(id_item %in% zero_var_ids)
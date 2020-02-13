library(tidyverse)

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < '2016-01-01')

df_regimes <- readRDS('data/cnet_bid_increments.rds') %>% 
  select(id_item, regime_juridico)

# Desvios-padrão dos lances de um mesmo fornecedor ----------------------------
df_sd <- df_lances %>%
  group_by(id_item, CNPJ_CPF) %>%
  summarise(sd_inc = sd(norm_inc_first, na.rm = TRUE),
            sd_int_anterior = sd(intervalo_anterior, na.rm=TRUE),
            sd_int_menor = sd(intervalo_menor, na.rm = TRUE),
            sd_int_proprio = sd(intervalo_proprio, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  arrange(sd_inc) %>% 
  left_join(df_regimes, by = 'id_item') %>%
  filter(n > 5) # Apenas leiloes em que o fornecedor deu mais de 5 lances

PregoesBR::get_summary_stats(df_sd, sd_inc, regime_juridico)

# Fornecedores com alta padronização em intervalo_lance_anterior --------------
df_sd %>% filter(sd_int_anterior < 5) %>% 
  arrange(desc(n)) %>% split(f = .$regime_juridico)

# Fornecedores com alta padronizacao em norm_inc_first ------------------------
df_sd %>% 
  filter(sd_inc < 0.0005, n > 10) %>% 
  arrange(desc(n)) %>% count(CNPJ_CPF) %>% arrange(desc(n))

library(tidyverse)

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < '2016-01-01')

df_regimes <- readRDS('data/cnet_bid_increments.rds') %>% 
  select(id_item, regime_juridico)

# Desvios-padrão dos lances de um mesmo fornecedor ----------------------------
df_sd <- df_lances %>%
  group_by(id_item, CNPJ_CPF) %>%
  summarise(sd_inc = sd(norm_inc_first, na.rm = TRUE),
            sd_int_anterior = sd(intervalo_lance_anterior, na.rm=TRUE),
            sd_int_menor_lance = sd(intervalo_menor_lance, na.rm = TRUE),
            sd_int_lance_proprio = sd(intervalo_lance_proprio, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  arrange(sd_inc) %>% 
  left_join(df_regimes, by = 'id_item') %>%
  filter(n > 5) # Apenas leiloes em que o fornecedor deu mais de 5 lances

PregoesBR::get_summary_stats(df_sd, sd_inc, regime_juridico)

# Fornecedores com alta padronização em intervalo_lance_anterior --------------
df_sd %>% filter(sd_int_anterior < 5) %>% 
  arrange(desc(n)) %>% split(f = .$regime_juridico)

# Fornecedores com alta padronizacao em norm_inc_first
df_sd %>% 
  filter(sd_inc < 0.0005, n > 10) %>% 
  arrange(desc(n)) %>% count(CNPJ_CPF) %>% arrange(desc(n))

# Casos interessantes ---------------------------------------------------------
# Possível robô: '22.122.311/0001-66'
df_lances %>% filter(CNPJ_CPF == '22.122.311/0001-66') %>% 
  count(id_item, data_abertura, regime_juridico) %>% arrange(desc(n))

# Possível robô: '06.946.072/0001-02'
# Atuação nos 3 regimes
df_lances %>% filter(CNPJ_CPF == '06.946.072/0001-02') %>% 
  count(id_item, data_abertura, regime_juridico) %>% arrange(desc(n))

# Leilão 13532500000520130002
# Vários participantes ativos, Forn. A cobrindo marginalmente todos os lances
df_lances %>% filter(id_item == '13532500000520130002') %>% 
  count(CNPJ_CPF) %>% arrange(desc(n))

# Possível robô: '11.282.541/0001-76'
# Bastante ativo no período pré-tratamento
df_lances %>% filter(CNPJ_CPF == "11.282.541/0001-76") %>% 
  count(id_item, data_abertura, regime_juridico) %>% arrange(desc(n))


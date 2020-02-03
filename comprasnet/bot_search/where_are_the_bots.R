library(tidyverse)

# Importando dados ------------------------------------------------------------
df_intervalo_mesmo_fornecedor <- 
  readRDS("data/cnet_intervalo_mesmo_fornecedor.rds")

df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_bid_inc_unnested <- readRDS("data/cnet_bid_increments_unnested.rds")

df_lances <- readRDS('data/cnet_lances.rds')

df_forn_total_lances_e_pregoes <- 
  readRDS('comprasnet/bot_search/cnet_forn_total_lances_e_pregoes.rds')

# Data exploration ------------------------------------------------------------
# Estatisticas relativas a incrementos e intervalos de cada fornecedor
# Apenas lances de cobertura <<<<
df_stats_menor_lance <- df_bid_inc_unnested %>%
  group_by(CNPJ_CPF) %>%
  summarise(n_inc = n(),
            avg_inc_first = mean(norm_inc_first),
            median_inc_first = median(norm_inc_first),
            sd_inc_first = sd(norm_inc_first),
            avg_intervalo_lance_anterior = mean(intervalo_lance_anterior),
            median_intervalo_lance_anterior = median(intervalo_lance_anterior),
            sd_intervalo_lance_anterior = sd(intervalo_lance_anterior),
            avg_intervalo_menor_lance = mean(intervalo_menor_lance),
            median_intervalo_menor_lance = median(intervalo_menor_lance),
            sd_intervalo_menor_lance = sd(intervalo_menor_lance)) %>%
  ungroup()

# Estatisticas relativas a intervalos e incrementos entre lances próprios
df_stats_lances_proprios <- df_intervalo_mesmo_fornecedor %>%
  group_by(CNPJ_CPF) %>%
  summarise(n_mesmo_fornecedor = n(),
            avg_int_mesmo_forn = mean(intervalo_lance_proprio),
            median_int_mesmo_forn = median(intervalo_lance_proprio),
            sd_int_mesmo_forn = sd(intervalo_lance_proprio)) %>%
  ungroup()

# Juntando bases
df_stats <- df_forn_total_lances_e_pregoes %>% 
  mutate(regime_juridico = 
           case_when(regime_juridico == 'Sem intervalo minimo' ~ 0,
                     regime_juridico == 'Regra 20s' ~ 1,
                     regime_juridico == 'Regra 20s + Regra 3s' ~ 2)) %>% 
  pivot_wider(names_from = regime_juridico,
              values_from = c(n_bids, n_auctions, bids_auction_ratio)) %>% 
  left_join(df_stats_menor_lance, by = 'CNPJ_CPF') %>% 
  left_join(df_stats_lances_proprios, by = 'CNPJ_CPF') %>% 
  arrange(desc(n_inc)) %>% 
  select(CNPJ_CPF, n_inc:sd_int_mesmo_forn, n_bids_2:bids_auction_ratio_2)

# Relacao dos 10 fornecedores que mais registraram lances
dez_mais_lances <- df_forn_total_lances_e_pregoes$CNPJ_CPF[1:10]

# Os 10 fornecedores que mais registraram lances entre mar/2011 e jan/2012
# representaram 34,47% (5606) de um total de 16260 lances submetidos no periodo
sum(df_forn_total_lances_e_pregoes$n_bids[1:10])/sum(df_forn_total_lances_e_pregoes$n_bids)

# DF com pregoes em que o fornecedor "11.282.541/0001-76" participou
pregoes_possivel_robo <- df_forn_lances_por_pregao %>% 
  filter(CNPJ_CPF == dez_mais_lances[1])
# saveRDS(pregoes_possivel_robo, 'pregoes_possivel_robo.rds')

## CHECAR ATIVIDADE DESSE FORNECEDOR APOS AS NOVAS REGRAS
## Talvez seja interessante restringir a lances de cobertura

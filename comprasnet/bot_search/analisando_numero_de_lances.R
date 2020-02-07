library(tidyverse)

# Importando dados ------------------------------------------------------------
df_intervalo_mesmo_fornecedor <- 
  readRDS("data/cnet_intervalo_mesmo_fornecedor.rds") %>% 
  filter(data_hora < '2016-01-01')

df_bid_inc <- readRDS('data/cnet_bid_increments.rds') %>% 
  filter(abertura_lances < '2016-01-01')

df_bid_inc_unnested <- readRDS("data/cnet_bid_increments_unnested.rds") %>% 
  filter(data_hora < '2016-01-01')

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < '2016-01-01')

# Criando DF com n_auctions e n_bids por fornecedor ---------------------------
# Número de lances por fornecedor em cada pregão
df_forn_lances_por_pregao <- df_lances %>%
  count(CNPJ_CPF, abertura_lances, id_item, regime_juridico) %>%
  arrange(desc(n))

# Número de leilões por fornecedor
df_forn_num_pregoes <- df_forn_lances_por_pregao %>%
  group_by(CNPJ_CPF, regime_juridico) %>%
  summarise(n_auctions = n()) %>%
  ungroup() %>%
  arrange(desc(n_auctions))

# Juntando duas bases acima
df_forn_lances_e_pregoes <- df_lances %>%
  count(CNPJ_CPF, regime_juridico) %>%
  rename(n_bids = n) %>%
  inner_join(df_forn_num_pregoes, by = c('CNPJ_CPF', 'regime_juridico')) %>%
  mutate(bids_auction_ratio = n_bids / n_auctions) %>% 
  arrange(desc(n_auctions, n_bids))

df_forn_lances_e_pregoes

# Estatisticas relativas a incrementos e intervalos de cada fornecedor --------
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

# Estatisticas relativas a intervalos entre lances próprios -------------------
df_stats_lances_proprios <- df_intervalo_mesmo_fornecedor %>%
  group_by(CNPJ_CPF) %>%
  summarise(n_mesmo_fornecedor = n(),
            avg_int_mesmo_forn = mean(intervalo_lance_proprio),
            median_int_mesmo_forn = median(intervalo_lance_proprio),
            sd_int_mesmo_forn = sd(intervalo_lance_proprio)) %>%
  ungroup()

# Número de lances registrados por cada fornecedor ----------------------------
df_num_lances <- df_lances %>% count(CNPJ_CPF) %>% 
  arrange(desc(n))

# Explorando participação no total de lances ----------------------------------
ranking_lances <- df_lances %>% 
  count(CNPJ_CPF, regime_juridico) %>% 
  split(.$regime_juridico) %>% 
  map(.f = ~ arrange(.x, desc(n)) %>%
        mutate(share_lances = n / sum(n),
               share_acumulado = cumsum(share_lances)))

# Share de lances dos 10 fornecedores que mais ativos por regime
# Queda com a regra dos 20s, mas recuperação após regra dos 3s
map_dbl(ranking_lances,
        .f = ~ slice(.x, 10) %>% select(share_acumulado) %>% unlist())

# Total de lances por regime
map_dbl(ranking_lances, .f = ~ sum(.x$n))

# HHI do share de lances, por regime
# Queda seguida de aumento
map_dbl(.x = ranking_lances,
        .f = ~ mutate(.x, share_sqrd = share_lances^2) %>% 
          select(share_sqrd) %>% sum())

# Boxplot numero de lances por fornecedor
bind_rows(ranking_lances) %>% 
  ggplot(aes(x = regime_juridico, y = n)) +
  geom_boxplot() +
  scale_y_log10() + 
  labs(title = 'Distribuição no número de lances por fornecedor em cada pregão',
       subtitle = 'Comparação entre regimes jurídicos',
       x = 'Regime Jurídico', y = 'Número de lances (escala logarítmica)')

# Boxplot share_lance dos fornecedores, por regime
bind_rows(ranking_lances) %>% 
  ggplot(aes(x = regime_juridico, y = share_lances)) +
  geom_boxplot() +
  scale_y_log10() + 
  labs(title = 'Distribuição das participações no total de lances',
       subtitle = 'Comparação entre regimes jurídicos',
       x = 'Regime Jurídico', y = 'Participação no total lances (escala logarítmica)')

# Density plot share_lance, por regime
bind_rows(ranking_lances) %>% 
  ggplot(aes(x = log(share_lances), fill = regime_juridico)) +
  geom_density(position = 'identity', alpha = 0.5)
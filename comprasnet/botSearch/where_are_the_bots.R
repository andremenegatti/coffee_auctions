library(PregoesBR)

df_intervalo_mesmo_fornecedor <- readRDS("Comprasnet/cnet_df_intervalo_mesmo_fornecedor.rds") %>%
  filter(abertura_lances > '2011-03-01')

df_bid_inc_unnested <- readRDS("Comprasnet/cnet_df_bid_inc_unnested.rds")

df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

df_forn_total_lances_e_pregoes <- readRDS('Comprasnet/df_forn_total_lances_e_pregoes.rds')

# glimpse(df_intervalo_mesmo_fornecedor)
# glimpse(df_bid_inc_unnested)
# glimpse(df_bid_inc)

df_bots1 <- df_bid_inc_unnested %>%
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


df_bots2 <- df_intervalo_mesmo_fornecedor %>%
  group_by(CNPJ_CPF) %>%
  summarise(n_mesmo_fornecedor = n(),
            avg_int_mesmo_forn = mean(intervalo_entre_lances),
            median_int_mesmo_forn = median(intervalo_entre_lances),
            sd_int_mesmo_forn = sd(intervalo_entre_lances)) %>%
  ungroup()

df_bots3 <- df_bots1 %>%
  full_join(df_bots2, by = "CNPJ_CPF") %>%
  arrange(desc(n_inc))

df_bots4 <- df_bots3 %>%
  left_join(df_forn_total_lances_e_pregoes, by = 'CNPJ_CPF')

# saveRDS(df_bots4, 'Comprasnet/df_bots4.rds')

# Relacao dos 10 fornecedores que mais registraram lances
dez_mais_lances <- df_forn_total_lances_e_pregoes$CNPJ_CPF[1:10]

# Os 10 fornecedores que mais registraram lances entre mar/2011 e jan/2012
# representaram 34,47% (5606) de um total de 16260 lances submetidos no periodo
sum(df_forn_total_lances_e_pregoes$n_bids[1:10])/sum(df_forn_total_lances_e_pregoes$n_bids)

# DF com pregoes em que o fornecedor "11.282.541/0001-76" participou
pregoes_possivel_robo <- filter(df_forn_lances_por_pregao, CNPJ_CPF == dez_mais_lances[1])
# saveRDS(pregoes_possivel_robo, 'pregoes_possivel_robo.rds')


## CHECAR ATIVIDADE DESSE FORNECEDOR APOS AS NOVAS REGRAS
## Talvez seja interessante restringir a lances de cobertura

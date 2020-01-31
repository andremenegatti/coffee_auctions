library(PregoesBR)

df_intervalo_mesmo_fornecedor <- readRDS("Comprasnet/cnet_df_intervalo_mesmo_fornecedor.rds") %>%
  filter(abertura_lances > '2011-03-01')

df_bid_inc_unnested <- readRDS("Comprasnet/cnet_df_bid_inc_unnested.rds")

df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

df_lances_completo <- readRDS('Comprasnet/df_lances_completo.rds')

df_forn_lances_por_pregao <- readRDS('Comprasnet/df_forn_lances_por_pregao.rds')

df_forn_total_lances_e_pregoes_por_regime <- readRDS('Comprasnet/df_forn_total_lances_e_pregoes_por_regime.rds')

# Calculando estatisticas descritivas dos incrementos e intervalos de cada fornecedor
# Usando DF que possui apenas incrementos negativos
df_bots1 <- df_bid_inc_unnested %>%
  filter(regime_juridico == 'Sem intervalo minimo') %>% ## <<<--- Restringindo nossa atencao ao periodo pre-tratamento
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

# Fazendo a mesma coisa, mas com o DF de intervalos e incrementos entre lances do proprio fornecedor
df_bots2 <- df_intervalo_mesmo_fornecedor %>%
  filter(regime_juridico == 'Sem intervalo minimo') %>%  ## <<<--- Restringindo nossa atencao ao periodo pre-tratamento
  group_by(CNPJ_CPF) %>%
  summarise(n_mesmo_fornecedor = n(),
            avg_int_mesmo_forn = mean(intervalo_entre_lances),
            median_int_mesmo_forn = median(intervalo_entre_lances),
            sd_int_mesmo_forn = sd(intervalo_entre_lances)) %>%
  ungroup()

# Juntando DFs criados nos dois blocos acima
df_bots3 <- df_bots1 %>%
  full_join(df_bots2, by = "CNPJ_CPF") %>%
  arrange(desc(n_inc))

# Adicionando dados de total de lances e total de pregoes em que cada fornecedor participo
df_bots4 <- df_bots3 %>%
  left_join(df_forn_total_lances_e_pregoes_por_regime %>%
              filter(regime_juridico == 'Sem intervalo minimo'), ## <<<--- Restringindo nossa atencao ao periodo pre-tratamento
            by = 'CNPJ_CPF')

# saveRDS(df_bots4, 'df_bots4.rds')

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

df_forn_lances_por_pregao


df_bid_inc_unnested %>%
  filter(CNPJ_CPF == "37.153.715/0001-94") %>%
  group_by(id_item, regime_juridico) %>%
  tally() %>%
  arrange(desc(n)) %>%
  get_summary_stats_by_period(var = n, time_var = regime_juridico)

df_bid_inc_unnested %>%
  filter(CNPJ_CPF == "37.153.715/0001-94") %>%
  group_by(id_item, regime_juridico) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ggplot() +
  geom_histogram(aes(x = n, fill = regime_juridico, group = regime_juridico), position = 'identity', alpha = 0.4)



df_bid_inc_unnested %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(id_item, regime_juridico) %>%
  tally() %>%
  arrange(desc(n)) %>%
  get_summary_stats_by_period(var = n, time_var = regime_juridico)

# A tabela a seguir ficou interessante!
# Para esse fornecedor, as medidas restritivas parecem estar associadas a grande aumento no incremento
df_bid_inc_unnested %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(regime_juridico) %>%
  get_summary_stats_by_period(var = norm_inc_first * 100, time_var = regime_juridico)


# O mesmo se verifica para incrementos entre lances proprios
df_intervalo_mesmo_fornecedor %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(regime_juridico) %>%
  get_summary_stats_by_period(var = incremento_lance_proprio, time_var = regime_juridico)

# Curiosamente, os resultados para intervalos sao contraintuitivos: esperava-se aumento do intervalo
df_bid_inc_unnested %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(regime_juridico) %>%
  get_summary_stats_by_period(var = intervalo_lance_anterior, time_var = regime_juridico)


df_bid_inc_unnested %>%
  # filter(CNPJ_CPF %in% dez_mais_lances) %>%
  # filter(intervalo_menor_lance < 200) %>%
  # group_by(id_item, regime_juridico) %>%
  # tally() %>%
  # arrange(desc(n)) %>%
  ggplot() +
  geom_histogram(aes(x = intervalo_lance_anterior, y = ..density.., fill = regime_juridico, group = regime_juridico), position = 'identity', alpha = 0.4) +
  facet_wrap(~regime_juridico)

df_bid_inc %>% filter(id_item == '15819800000320130014') %>%
  select(regime_juridico, abertura_lances)

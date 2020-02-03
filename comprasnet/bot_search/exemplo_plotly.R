library(PregoesBR)
library(plotly)

# Importando dados
df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_lances <- readRDS('data/cnet_lances.rds')

cnet_cafe <- readRDS('data/cnet_cafe.rds') %>%
  select(id_item, inicio_fase_aleatoria)

df_forn_total_lances_e_pregoes <- 
  readRDS('comprasnet/bot_search/cnet_forn_total_lances_e_pregoes.rds')

pregoes_possivel_robo <- 
  readRDS('comprasnet/bot_search/pregoes_possivel_robo.rds')

# Data wrangling --------------------------------------------------------------
df_forn_lances_por_pregao <- df_lances %>%
  count(CNPJ_CPF, abertura_lances, id_item, regime_juridico) %>%
  arrange(desc(n))

# Relacao dos 10 fornecedores que mais registraram lances
dez_mais_lances <- df_forn_total_lances_e_pregoes$CNPJ_CPF[1:10]

# DF com pregoes pré-tratamento em que houve participacao
# de um dos 3 fornecedores com mais lances
pregoes_possivel_robo <- df_forn_lances_por_pregao %>% 
  filter(CNPJ_CPF %in% dez_mais_lances[1:3])

# Tbm podemos pegar os leiloes no topo de df_forn_lances_por_pregao
df_forn_lances_por_pregao %>%
  filter(abertura_lances > data_3s)

df_forn_lances_por_pregao %>%
  filter(abertura_lances > data_3s) %>%
  group_by(id_item, abertura_lances) %>%
  summarise(n_lances = sum(n)) %>%
  arrange(desc(n_lances)) %>% 
  ungroup()

# Selecionando os lances do id_item 15407000005720110030
exemplo <- df_lances %>%
  filter(id_item == "17017700000420150012") %>%
  group_by(CNPJ_CPF) %>%
  mutate(n_lances = n()) %>%
  ungroup() %>%
  left_join(cnet_cafe, by = 'id_item')

# Plotly ----------------------------------------------------------------------
# Criando legendas
exemplo_temp <- exemplo %>%
  select(CNPJ_CPF, n_lances) %>%
  distinct() %>%
  arrange(desc(n_lances))

exemplo_legendas1 <- exemplo_temp %>%
  slice(1:5) %>%
  mutate(Fornecedor = str_c('Fornecedor ', LETTERS[1:n()]))

exemplo_legendas2 <- exemplo_temp %>%
  anti_join(exemplo_legendas1, by = 'CNPJ_CPF') %>%
  mutate(Fornecedor = 'Outros fornecedores')

exemplo_legendas <- bind_rows(exemplo_legendas1, exemplo_legendas2)

# Juntando legendas na base original
exemplo <- exemplo %>%
  left_join(exemplo_legendas, by = c('CNPJ_CPF', 'n_lances'))

# Plotting
PregoesBR::bid_plot(exemplo)



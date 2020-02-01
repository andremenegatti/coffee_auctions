library(PregoesBR)

df_lances_completo <- readRDS('Comprasnet/df_lances_completo.rds')

# DF com o numero de lances de cada fornecedor em cada pregao
df_forn_lances_por_pregao <- df_lances_completo %>%
  count(CNPJ_CPF, abertura_lances, id_item) %>%
  arrange(desc(n))

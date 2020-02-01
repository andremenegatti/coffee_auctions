library(tidyverse)

# Definindo variáveis com coeficientes estimados:
coef_sp <- 114
coef_brasil <- 114

# Abrindo bases ---------------------------------------------------------------
data_list <- 
  map(.x = str_c('data/', c('bec', 'cnet', 'cnet_sp'), '_cafe_dd.rds'),
      .f = ~ readRDS(.x) %>%
        # Removendo outliers: 2.5% de cada lado # <<<<
        PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>%
        # Coercing to factor to avoid warnings when joining dataframes
        filter(kg_por_unid != 0.25) # <<<<
  )  %>%
  # Dando nomes aos DFs
  set_names(c('bec', 'cnet', 'cnet_sp'))

# Brasil ----------------------------------------------------------------------
df_gasto_pos_3s_brasil <- data_list$cnet %>%
  filter(abertura_lances >= data_3s) %>%
  select(abertura_lances, inicio_ano, win_bid_kg, quantidade, kg_por_unid) %>%
  mutate(preco_total = win_bid_kg * kg_por_unid * quantidade)

# Gasto total apos regra 3s (milhoes de reais)
gasto_pos_3s_brasil <- sum(df_gasto_pos_3s_brasil$preco_total) / 1e+6

# Gasto adicional causado pela regra dos 3s
gasto_pos_3s_brasil - (gasto_pos_3s_brasil * 100 / coef_brasil)

# SP --------------------------------------------------------------------------
df_gasto_pos_3s_sp <- data_list$cnet_sp %>%
  filter(abertura_lances >= data_3s) %>%
  select(abertura_lances, inicio_ano, win_bid_kg, quantidade, kg_por_unid) %>%
  mutate(preco_total = win_bid_kg * kg_por_unid * quantidade)

# Gasto total apos regra 3s (milhoes de reais)
gasto_pos_3s_sp <- sum(df_gasto_pos_3s_sp$preco_total) / 1e+6

# Gasto adicional causado pela regra dos 3s
gasto_pos_3s_sp - (gasto_pos_3s_sp * 100 / coef_sp)

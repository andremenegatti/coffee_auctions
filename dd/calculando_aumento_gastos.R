

df_gasto_pos_3s_full <- data_list$cnet_cafe %>%
  filter(abertura_lances >= data_3s) %>%
  select(abertura_lances, inicio_ano, win_bid, quantidade, kg_por_unid) %>%
  mutate(preco_total = win_bid * kg_por_unid * quantidade)

# Gasto total apos regra 3s (milhoes de reais)
gasto_pos_3s_full <- sum(df_gasto_pos_3s_full$preco_total) / 1e+6

gasto_pos_3s_full - (gasto_pos_3s_full * 100 / 114)



df_gasto_pos_3s_sp <- data_list$cnet_cafe_sp %>%
  filter(abertura_lances >= data_3s) %>%
  select(abertura_lances, inicio_ano, win_bid, quantidade, kg_por_unid) %>%
  mutate(preco_total = win_bid * kg_por_unid * quantidade)

# Gasto total apos regra 3s (milhoes de reais)
gasto_pos_3s_sp <- sum(df_gasto_pos_3s_sp$preco_total) / 1e+6

gasto_pos_3s_sp - (gasto_pos_3s_sp * 100 / 114)

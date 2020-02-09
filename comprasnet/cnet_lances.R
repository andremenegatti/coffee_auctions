library(PregoesBR)

# Importando dados
df_bid_inc <- 
  readRDS('data/cnet_bid_increments.rds') %>%
  select(id_item, abertura_lances, regime_juridico, regime_juridico_20s,
         regime_juridico_3s, bid_increments)

df_intervalo_mesmo_fornecedor <-
  readRDS("data/cnet_intervalo_mesmo_fornecedor.rds") %>%
  select(id_item, CNPJ_CPF, valor_lance, data_hora,
         intervalo_proprio, incremento_proprio,
         incremento_proprio_bruto, incremento_proprio_norm)

# Desaninhando dados de df_bid_inc
df_lances <- df_bid_inc %>%
  unnest(cols = bid_increments)

# Removendo dois lances duplicados
df_lances <- df_lances %>% 
  distinct(id_item, CNPJ_CPF, valor_lance, data_hora, .keep_all = TRUE)

# Juntando dados de intervalo/incremento mesmo fornecedor
df_lances <- df_lances %>%
  left_join(df_intervalo_mesmo_fornecedor,
            by = c("id_item", "CNPJ_CPF", "valor_lance", "data_hora"))

# Ordenando/selecionando variaveis
df_lances_selected <- df_lances %>% 
  select(id_item:regime_juridico_3s, data_hora, CNPJ_CPF,
         fator_equalizacao, valor_equalizado,
         valor_lance, valor_lance_kg, valor_lance_kg_defl,
         norm_inc_first, norm_inc_reserve,
         incremento_menor, incremento_anterior, incremento_proprio,
         intervalo_menor, intervalo_anterior, intervalo_proprio,
         incremento_menor_bruto, incremento_proprio_bruto, incremento_proprio_norm,
         )

saveRDS(df_lances_selected, 'data/cnet_lances.rds')

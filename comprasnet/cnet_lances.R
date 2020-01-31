library(PregoesBR)

# Abrindo bases
df_intervalo_mesmo_fornecedor <-
  readRDS("Comprasnet/cnet_df_intervalo_mesmo_fornecedor_v2.rds")

df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

# Criando DF de lances, com todas as informacoes sobre intervalos e incrementos
df_lances_completo <- df_bid_inc %>%
  select(id_item, data_abertura, abertura_lances,
         regime_juridico, regime_juridico_20s, regime_juridico_3s,
         bid_increments) %>%
  unnest(cols = bid_increments) %>%
  left_join(df_intervalo_mesmo_fornecedor %>%
              select(id_item, CNPJ_CPF, valor_lance, data_hora,
                     intervalo_lance_proprio, incremento_lance_proprio,
                     desconto_lance_proprio_bruto),
            by = c("id_item", "CNPJ_CPF", "valor_lance", "data_hora")) %>%
  # Em dois pregoes, ha dois lances duplicados:
  # Isso nao gera problemas na analise de incrementos, mas vamos exclui-los
  distinct(id_item, CNPJ_CPF, valor_lance, data_hora, .keep_all = TRUE)

# saveRDS(df_lances_completo, 'Comprasnet/cnet_df_lances_completo.rds')

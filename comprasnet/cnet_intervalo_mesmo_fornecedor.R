library(PregoesBR)

df_bid_inc <- readRDS('data/cnet_bid_increments.rds') %>% 
  select(id_item, abertura_lances, regime_juridico_20s, bid_increments)

# Aninhando por CNPJ (dentro de cada DF de bid_increments)
df_intervalo2 <- df_bid_inc %>%
  mutate(intervalo_mesmo_fornecedor =
           map(.x = bid_increments,
               .f = ~ .x %>% 
                 select(data_hora, CNPJ_CPF, valor_lance, valor_lance_kg_defl) %>% 
                 nest(data = c(-CNPJ_CPF))))

# Excluindo DFs referentes a fornecedores que deram menos de 2 lances por leilao
df_intervalo3 <- df_intervalo2 %>%
  mutate(intervalo_mesmo_fornecedor_clean =
           map(.x = intervalo_mesmo_fornecedor,
               .f = ~ filter(.x, map_lgl(.x = data, .f = ~ nrow(.x) > 1))))

# Calculando intervalo e incremento entre lances
df_intervalo4 <- df_intervalo3 %>%
  mutate(
    intervalo_mesmo_fornecedor_clean =
      map(
        .x = intervalo_mesmo_fornecedor_clean,
        .f = ~ .x %>%
          mutate(
            data =
              map(
                .x = data,
                .f = ~ .x %>%
                  arrange(data_hora) %>% 
                  mutate(
                    intervalo_proprio =
                      calcular_intervalo_lances(data_hora),
                    incremento_proprio = 
                      valor_lance_kg_defl - lag(cummin(.$valor_lance_kg_defl)),
                    incremento_proprio_bruto =
                      valor_lance - lag(valor_lance),
                    incremento_proprio_norm =
                      incremento_proprio_bruto / lag(valor_lance)
                  ) %>%
                  # Excluindo NA do primeiro lance de cada fornecedor
                  filter(!is.na(intervalo_proprio))
              )
          )
      )
  )

# Desaninhando DFs dos fornecedores
df_intervalo5 <- df_intervalo4 %>%
  mutate(intervalo_mesmo_fornecedor_clean =
           map(.x = intervalo_mesmo_fornecedor_clean,
               .f = ~ unnest(.x, cols = data) %>% 
                 arrange(data_hora)))

# Selecionando colunas e desaninhando DFs de intervalos
df_intervalo6 <- df_intervalo5 %>%
  select(id_item, abertura_lances, regime_juridico_20s,
         intervalo_mesmo_fornecedor_clean) %>%
  unnest(cols = intervalo_mesmo_fornecedor_clean) %>% 
  select(-data)

# Removendo dois lances duplicados
df_intervalo7 <- df_intervalo6 %>% 
  distinct(id_item, CNPJ_CPF, valor_lance, data_hora, .keep_all = TRUE)

saveRDS(df_intervalo7, 'data/cnet_intervalo_mesmo_fornecedor.rds')

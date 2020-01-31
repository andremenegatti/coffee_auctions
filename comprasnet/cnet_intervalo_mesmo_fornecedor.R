library(PregoesBR)

df_atas <- readRDS("comprasnet/cnet_cafe.rds")

# Filtrando e selecionando
df_intervalo <- df_atas %>%
  # CORTE: APENAS APOS MARCO/2011
  filter(abertura_lances >= '2011-03-01') %>%
  select(id_item, id_item,
         regime_juridico, regime_juridico_20s, abertura_lances,
         inicio_ano, inicio_trimestre, inicio_bimestre,
         inicio_semestre, inicio_mes, inicio_semana,
         lances_clean) %>%
  # Removendo observacoes em que nao houve lances (apenas propostas)
  filter(map_dbl(.x = lances_clean, .f = ~ nrow(.x)) > 1) %>%
  # Excluindo obs sem coluna 'valor_lance' no historico de lances
  filter(map_lgl(.x = lances_clean, .f = ~ 'valor_lance' %in% names(.x)))

# Aninhando por CNPJ (dentro de cada DF de lances_clean)
df_intervalo2 <- df_intervalo %>%
  mutate(intervalo_mesmo_fornecedor =
           map(.x = lances_clean,
               .f = ~ nest(.x, data = c(valor_lance, data_hora))))

# Excluindo DFs referentes a fornecedores que deram menos de 2 lances por leilao
df_intervalo3 <- df_intervalo2 %>%
  mutate(intervalo_mesmo_fornecedor_clean =
           map(.x = intervalo_mesmo_fornecedor,
               .f = ~ filter(.x, map_lgl(.x = data, .f = ~ nrow(.x) > 1))))

# Calculando intervalo e incremento entre lances
df_intervalo4 <- df_intervalo3 %>%
  mutate(
    intervalo_mesmo_fornecedor_clean =
      map(.x = intervalo_mesmo_fornecedor_clean,
          .f = ~ .x %>%
            mutate(data =
                     map(.x = data,
                         .f = ~ .x %>%
                           mutate(

                             intervalo_lance_proprio =
                               calcular_intervalo_lances(data_hora),

                             desconto_lance_proprio_bruto =
                               valor_lance - lag(valor_lance),

                             incremento_lance_proprio_bruto =
                               desconto_lance_proprio_bruto/first(valor_lance)

                             ) %>%
                           # Excluindo NA do primeiro lance de cada fornecedor
                           filter(!is.na(intervalo_lance_proprio))))
          )
    )

# Desaninhando DFs dos fornecedores
df_intervalo5 <- df_intervalo4 %>%
  mutate(intervalo_mesmo_fornecedor_clean =
           map(.x = intervalo_mesmo_fornecedor_clean,
               .f = ~ unnest(.x, cols = data)))

# Selecionando colunas e desaninhando DFs de intervalos
df_intervalo6 <- df_intervalo5 %>%
  select(id_item, id_item, regime_juridico, regime_juridico_20s,
         abertura_lances, inicio_ano, inicio_trimestre, inicio_bimestre,
         inicio_semestre, inicio_mes, inicio_semana,
         intervalo_mesmo_fornecedor_clean) %>%
  unnest(cols = intervalo_mesmo_fornecedor_clean)

# saveRDS(df_intervalo6, 'Comprasnet/cnet_df_intervalo_mesmo_fornecedor.rds')

library(PregoesBR)

df_atas <- readRDS("data/cnet_cafe.rds") %>%
  filter(abertura_lances >= '2011-03-01') # <<<<<<

# Selecionando variaveis e excluindo leiloes em que houve menos de 2 lances
df_bid_inc <- df_atas %>%
  select(id_item, data_abertura, abertura_lances, inicio_ano, inicio_semestre,
         inicio_mes, inicio_bimestre, inicio_trimestre, inicio_semana,
         regime_juridico, regime_juridico_20s, regime_juridico_3s,
         lances_clean, melhor_lance, quantidade, kg_por_unid, menor_proposta,
         menor_proposta_global, indice_ipca_mes, inicio_mes, reserve_kg) %>%
  filter(map_lgl(.x = lances_clean, .f = ~ nrow(.x) > 1))

# Correcao de valores com base nos dados de propostas
df_bid_inc <- df_bid_inc %>%
  mutate(
    bid_increments =
      pmap(.l = list(lances_clean,
                     menor_proposta,
                     menor_proposta_global,
                     quantidade,
                     kg_por_unid),
           .f = ~ ..1 %>%
             mutate(valor_lance_kg =
                      ajustar_preco_global(preco = valor_lance / ..5,
                                           menor_proposta = ..2 / ..5,
                                           menor_proposta_global = ..3 / ..5,
                                           quantidade = ..4)))
  )

# Deflacionando
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments =
           map2(.x = bid_increments, .y = indice_ipca_mes,
                .f = ~  mutate(.x, valor_lance_kg_defl =
                                 deflacionar(valor_lance_kg,
                                             # IPCA dez/2015
                                             indice_referencia = 4493.17,
                                             indice_no_periodo = .y)))) %>%
  # Variaveis com primeiros e ultimos lances apos correcao e deflacao
  mutate(last_bid_kg_defl = map_dbl(.x = bid_increments,
                                      .f = ~ last(.x$valor_lance_kg_defl)),
         first_bid_kg_defl = map_dbl(.x = bid_increments,
                                       .f = ~ first(.x$valor_lance_kg_defl)))

# Calculando incrementos para valores corrigidos
df_bid_inc <- df_bid_inc %>%
  mutate(
    bid_increments =
      pmap(.l = list(bid_increments,
                     reserve_kg,
                     first_bid_kg_defl),
           .f = ~ ..1 %>%
             mutate(
               lowest_bid_so_far_kg_defl =
                 cummin(..1$valor_lance_kg_defl),
               incremento_anterior = 
                 valor_lance_kg_defl - lag(valor_lance_kg_defl),
               incremento_menor = 
                 valor_lance_kg_defl - lag(lowest_bid_so_far_kg_defl),
               incremento_menor_bruto = 
                 valor_lance - lag(cummin(..1$valor_lance)),
               norm_inc_reserve = incremento_menor / ..2,
               norm_inc_first = incremento_menor / ..3
             )
      )
  )

# Calculando intervalo entre lances consecutivos
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments =
           map(.x = bid_increments,
               .f = ~ .x %>%
                 mutate(intervalo_anterior =
                          calcular_intervalo_lances(data_hora1 = data_hora))))

# Calculando o intervalo entre um lance e o melhor lance que o antecedeu
df_bid_inc <- df_bid_inc %>%
  mutate(
    bid_increments =
      map(.x = bid_increments,
          .f = ~ .x %>%
            mutate(
              # Criando coluna com lag do melhor lance registrado ate o momento
              ## P/ cada lance, indica hora do melhor lance registrado antes
              hora_lowest_bid_so_far =
                get_time_lowest_bid(valor_lance_kg_defl, data_hora,
                                    lowest_bid = lowest_bid_so_far_kg_defl),
              # Intervalo entre cada lance e o melhor lance anterior
              intervalo_menor =
                calcular_intervalo_lances(
                  data_hora1 = data_hora,
                  data_hora2 = lag(hora_lowest_bid_so_far)
                )
            ))
  )

# Excluindo variaveis desnecessarias de bid_increments
df_bid_inc <- df_bid_inc %>% 
  mutate(bid_increments = map(.x = bid_increments,
                              .f = ~ .x %>% 
                                select(-lowest_bid_so_far_kg_defl,
                                       -hora_lowest_bid_so_far)))

# Criando colunas com dfs filtrados:
# - apenas incrementos NEGATIVOS (sem lances intermediarios)
# - ou não-negativos (apenas lances intermediarios)
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments_negative = 
           map(.x = bid_increments,
               .f = ~ filter(.x, incremento_menor < 0)),
         bid_increments_non_negative = 
           map(.x = bid_increments,
               .f = ~ filter(.x, incremento_menor >= 0)))

# Media do incremento normalizado de cada pregao
mean_narm <- partial(mean, na.rm = TRUE)
median_narm <- partial(median, na.rm = TRUE)

df_bid_inc <- df_bid_inc %>%
  mutate(
    avg_norm_inc_reserve = map_dbl(.x = bid_increments_negative,
                                   .f = ~ mean_narm(.x$norm_inc_reserve)),
    avg_norm_inc_first = map_dbl(.x = bid_increments_negative,
                                 .f = ~ mean_narm(.x$norm_inc_first)),
    median_norm_inc_reserve = map_dbl(.x = bid_increments_negative,
                                      .f = ~ median_narm(.x$norm_inc_reserve)),
    median_norm_inc_first = map_dbl(.x = bid_increments_negative,
                                    .f = ~ median_narm(.x$norm_inc_first))
  )

# Selecionando variaveis e salvando
df_bid_inc_selected <- df_bid_inc %>% 
  select(id_item, abertura_lances, regime_juridico:regime_juridico_3s,
         first_bid_kg_defl, last_bid_kg_defl,
         avg_norm_inc_reserve:median_norm_inc_first, 
         bid_increments, bid_increments_negative, bid_increments_non_negative)

saveRDS(df_bid_inc_selected, 'data/cnet_bid_increments.rds')

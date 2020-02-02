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
             mutate(valor_lance_kg = valor_lance / ..5) %>%
             mutate(valor_lance_corr =
                      ajustar_preco_global(preco = valor_lance_kg,
                                           menor_proposta = ..2 / ..5,
                                           menor_proposta_global = ..3 / ..5,
                                           quantidade = ..4)))
    )

# Deflacionando
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments =
           map2(.x = bid_increments, .y = indice_ipca_mes,
                .f = ~  mutate(.x, valor_lance_corr_defl =
                                 deflacionar(valor_lance_corr,
                                             indice_no_periodo = .y)))) %>%
  # Variaveis com primeiros e ultimos lances apos correcao e deflacao
  mutate(last_bid_corr_defl = map_dbl(.x = bid_increments,
                                      .f = ~ last(.x$valor_lance_corr_defl)),
         first_bid_corr_defl = map_dbl(.x = bid_increments,
                                       .f = ~ first(.x$valor_lance_corr_defl)))

# Calculando incrementos para valores corrigidos
df_bid_inc <- df_bid_inc %>%
  mutate(
    bid_increments =
      pmap(.l = list(bid_increments,
                     reserve_kg,
                     first_bid_corr_defl),
           .f = ~ ..1 %>%
             mutate(
               lowest_bid_so_far_corr_defl =
                 find_lowest(..1$valor_lance_corr_defl),
               lag_lowest_bid_corr_defl =  lag(lowest_bid_so_far_corr_defl),
               increment = valor_lance_corr_defl - lag_lowest_bid_corr_defl,
               norm_inc_reserve = increment / ..2,
               norm_inc_first = increment / ..3,
               desconto_bruto =
                 valor_lance - lag(find_lowest(..1$valor_lance))
               )
         )
    )

# Calculando intervalo entre lances consecutivos
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments =
           map(.x = bid_increments,
               .f = ~ .x %>%
                 mutate(intervalo_lance_anterior =
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
                get_time_lowest_bid(valor_lance_corr_defl, data_hora,
                                    lowest_bid = lowest_bid_so_far_corr_defl),
              # Intervalo entre cada lance e o melhor lance anterior
              intervalo_menor_lance =
                calcular_intervalo_lances(
                  data_hora1 = data_hora,
                  data_hora2 = lag(hora_lowest_bid_so_far)
                  )
              ))
         )

# Criando colunas com dfs filtrados:
# - apenas incrementos NEGATIVOS (sem lances intermediarios)
# - ou não-negativos (apenas lances intermediarios)
df_bid_inc <- df_bid_inc %>%
  mutate(bid_increments_negative = map(.x = bid_increments,
                                       .f = ~ filter(.x, increment < 0)),
         bid_increments_non_negative = map(.x = bid_increments,
                                           .f = ~ filter(.x, increment >= 0)))

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

saveRDS(df_bid_inc, 'data/cnet_bid_increments.rds')

library(PregoesBR)

# Dados principais
bec_cafe <- readRDS('bec/montando_base_v2/bec_cafe_raw_v2.rds')

# Dados de municipios
bec_municipios <- readRDS("data/bec_municipios_pregoes.rds") %>%
  inner_join(readRDS("data/bec_municipios_geocoded.rds"),
             by = c('municipio' = 'id'))

# Limpando, criando variaveis, filtrando --------------------------------------
bec_cafe <- bec_cafe %>%
  mutate(
    kg_por_unid =
      case_when(
        str_detect(unidade_fornecimento, 'QUILOGRAMA') ~ 1,
        str_detect(unidade_fornecimento, '500[\\.,]00 GRAMA') ~ 0.5,
        str_detect(unidade_fornecimento, '250[\\,.]00 GRAMA') ~ 0.25
      ),
    kg_fornecidos = quantidade * kg_por_unid,
    melhor_lance_bruto = melhor_lance / kg_por_unid
    )

# Juntando dados limpos de municipios, latitude e longitude -------------------
bec_cafe <- bec_cafe %>%
  select(-municipio) %>%
  left_join(bec_municipios, by = 'id_item')

# Deflacionando ---------------------------------------------------------------
bec_cafe <- bec_cafe %>%
  # Criando variaveis de tempo
  create_time_variables(time_var = dt_inicio) %>%
  # Juntando dados do ipca para desinflacionar
  left_join(read_csv('data/ipca.csv') %>%
              mutate(inicio_mes = lubridate::ymd(inicio_mes)),
            by = 'inicio_mes') %>%
  rename(indice_ipca_mes = ipca) %>%
  # Deflacionando melhor lance bruto
  mutate(melhor_lance_bruto_defl =
           deflacionar(melhor_lance_bruto,
                       indice_no_periodo = indice_ipca_mes))

# Correção: limite de preços --------------------------------------------------
bec_cafe <- bec_cafe %>%
  mutate(win_bid_kg = corrigir_erro_de_medida(melhor_lance_bruto_defl,
                                              corte = 40,
                                              quantidade))

# Removendo observacoes duplicadas --------------------------------------------
bec_cafe <- bec_cafe %>%
  distinct(id_item, .keep_all = TRUE)

# Criando variaveis relativas a lances e fornecedores -------------------------
# DF temporario: apenas pregoes em que houve lances
bec_cafe_lances <- bec_cafe %>%
  filter(map_lgl(.x = lances, .f = ~ is.data.frame(.x))) %>%
  filter(map_lgl(.x = lances, .f = ~ nrow(.x) > 0))

# Calculando variaveis nos pregoes em que houve lances
bec_cafe_lances <- bec_cafe_lances %>%
  mutate(lances_clean =
           map(.x = lances,
               .f = ~ .x %>%
                 mutate(data = dmy_hms(Data),
                        valor = str_replace_all(Valor, '\\.', '') %>%
                          str_replace(',', '\\.') %>% as.numeric()) %>%
                 select(licitante = Licitante, valor,
                        data, situacao = Situacao))) %>%
  # Media de lances por participante de cada pregao
  mutate(avg_bids_per_bidder = map_dbl(.x = lances,
                                       .f = ~mean(table(.x["Licitante"])))) %>%
  # Numero de participantes de cada pregao
  mutate(
    num_forn_lances =
      map_int(.x = lances,
              .f = possibly(.f = function(x) length(unique(x$Licitante)),
                            otherwise = 0L))
  ) %>%
  # Numero de lances de cada pregao
  mutate(num_lances_total = map_int(.x = lances, .f = ~ nrow(.x)))

# Incluindo novas variaveis na base completa
bec_cafe <- bec_cafe %>%
  left_join(bec_cafe_lances %>%
              select(id_item, lances_clean, avg_bids_per_bidder,
                     num_forn_lances, num_lances_total),
            by = 'id_item')

# Limpando dados de propostas
# DF temporario para limpar dados de propostas
bec_cafe_propostas <- bec_cafe %>%
  filter(map_lgl(.x = propostas, .f = ~ is.data.frame(.x))) %>%
  filter(map_lgl(.x = propostas, .f = ~ nrow(.x) > 0)) %>%
  mutate(propostas_clean =
           map(.x = propostas,
               .f = ~ .x %>%
                 mutate(valor = str_remove(Valor, 'R. ') %>%
                          str_replace_all('\\.', '') %>%
                          str_replace(',', '\\.') %>% as.numeric(),
                        data = lubridate::dmy_hms(Data),
                        marca = toupper(Marca)) %>%
                 select(ordem = Ordem, licitante = Licitante,
                        valor, data, situacao = Situacao,
                        marca, justificativa = Justificativa,
                        itens_grupo = ItensDoGrupo))) %>%
  select(id_item, propostas_clean)

# Juntando dados de propostas na base principal
bec_cafe <- left_join(bec_cafe, bec_cafe_propostas, by = 'id_item')

# Reordenando e dropando lances e propostas originais -------------------------
bec_cafe <- bec_cafe %>%
  select(id_item:quantidade, propostas = propostas_clean,
         lances = lances_clean, preferencia_df:win_bid_kg,
         avg_bids_per_bidder:num_lances_total)

saveRDS(bec_cafe, 'bec/montando_base_v2/bec_cafe_v2.rds')

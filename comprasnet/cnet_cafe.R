library(PregoesBR)

# Definindo índice de referência para deflacionar valores ---------------------
ipca_periodo_referencia <- 4493.17 # IPCA dez/2015

# CARREGANDO BASES ------------------------------------------------------------
df_API_original <- readRDS('data/cnet_api_cafe.rds')

df_atas_original <- readRDS('data/cnet_atas_cafe.rds')

uasgs <-
  readRDS('data/cnet_uasgs_geocoded.rds') %>%
  select(co_uasg = id, nome_uasg = nome, id_municipio_uasg = id_municipio,
         municipio, sigla_uf, lon, lat) %>%
  mutate(co_uasg = str_pad(co_uasg, width = 6, side = 'left', pad = '0'))

# DATA WRANGLING --------------------------------------------------------------
# Adicionando UF, municipio e outros dados da UASG, selecionando variaveis
df_API <- df_API_original %>%
  mutate(co_uasg = str_extract(id_item, '\\d{6}')) %>%
  select(-nome_uasg) %>% # Essa variavel esta errada na base df_API <<<<<
  left_join(uasgs, by = 'co_uasg') %>%
  select(id_item, kg_por_unid,
         valor_estimado_API = valor_estimado_item,
         valor_negociado_API = valor_negociado,
         sigla_uf, id_municipio_uasg, municipio, nome_uasg, lon, lat)

# Unnesting
df_atas <- df_atas_original %>%
  unnest(cols = dados)

# Selecionando/reordenando colunas
df_atas <- df_atas %>%
  select(id_item, id_pregao, indice_item, data_abertura, data_encerramento,
         descricao, descricao_complementar, valor_estimado, quantidade,
         unidade_fornecimento, resultado, situacao, tratamento_diferenciado,
         decreto_7174, margem_preferencia, abertura_lances:eventos_item) %>%
  # Ajustando formato de variaveis de tempo
  mutate_at(vars(c("data_abertura", "data_encerramento")), ymd_hm) %>%
  mutate_at(vars(c("abertura_lances", "iminencia_encerramento",
                   "inicio_fase_aleatoria", "inicio_desempate",
                   "item_encerrado", "encerramento_lances")), dmy_hms)

# Parsing resultados
df_atas <- df_atas %>%
  mutate(resultado_parsed =
           map(.x = resultado,
               .f = ~ ComprasnetAtasParsing::parse_resultado(.x))) %>%
  unnest(cols = resultado_parsed)

# Joining dados de itens/resultados API
df_atas <- df_atas %>%
  inner_join(df_API, by = 'id_item')

# Excluindo obs. em que nao ha coluna 'valor_lance' no historico de lances
# 3 observacoes excluidas; em vez de 'valor_lance', ha 'valor_com_desconto'
df_atas <- df_atas %>%
  filter(map_lgl(.x = lances, .f = ~ 'valor_lance' %in% names(.x)))

# Criando variaveis indicativas do regime juridico
df_atas <- df_atas %>%
  mutate(

    regime_juridico =
      case_when(abertura_lances < data_20s ~ 1,
                abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
                abertura_lances >= data_3s ~ 3) %>%
      factor(labels = c('Sem intervalo minimo',
                        'Regra 20s', 'Regra 20s + Regra 3s')),

    regime_juridico_3s = ifelse(regime_juridico == 'Regra 20s + Regra 3s',
                                'Após regra dos 3s',
                                'Antes da regra dos 3s'),

    regime_juridico_20s = ifelse(regime_juridico != 'Sem intervalo minimo',
                                 'Após regra dos 20s',
                                 'Antes da regra dos 20s')
    )

# Parsing e cleaning lances
df_atas <- df_atas %>%
  # Convertendo data_hora e valor_lance
  mutate(
    lances =
      map(.x = lances,
          .f = ~ .x %>%
            mutate(
              data_hora = ymd_hms(ComprasnetAtasParsing::toISOdate(data_hora)),
              valor_lance = ComprasnetAtasParsing::limpar_valores(valor_lance)
              ))
    ) %>%
  # Criando coluna com dfs de lances limpo
  # (sem propostas, que possuem data anterior a abertura)
  mutate(lances_clean = map2(.x = lances, .y = abertura_lances,
                             .f = ~ filter(.x, data_hora >= .y)))

# Parsing propostas
df_atas <- df_atas %>%
  mutate(
    propostas =
      map(.x = propostas,
          .f = ~ .x %>%
            mutate(valor_unitario =
                     ComprasnetAtasParsing::limpar_valores(valor_unitario),
                   valor_global =
                     ComprasnetAtasParsing::limpar_valores(valor_global)))
    )

# Criando variaveis com as menores propostas (unitaria e global)
df_atas <- df_atas %>%
  mutate(menor_proposta = map_dbl(.x = propostas,
                                  .f = ~ min(.x$valor_unitario)),
         menor_proposta_global = map_dbl(.x = propostas,
                                         .f = ~ min(.x$valor_global)))

# Criando variaveis de primeiro lance e menor lance EM UM NOVO DATAFRAME
df_primeiro_e_menor <- df_atas %>%
  filter(map_lgl(.x = lances_clean, .f = ~ nrow(.x) > 0)) %>%
  mutate(primeiro_lance = map_dbl(.x = lances_clean,
                                  .f = ~ first(.x$valor_lance)),
         menor_lance = map_dbl(.x = lances_clean,
                               .f = ~ min(.x$valor_lance))) %>%
  select(id_item, primeiro_lance, menor_lance)

# Juntando dados de primeiro e menor lance no DF principal
df_atas <- df_atas %>%
  left_join(df_primeiro_e_menor, by = 'id_item')

# Limpando e criando variaveis
df_atas <- df_atas %>%
  mutate(valor_estimado =
           ComprasnetAtasParsing::limpar_valores(valor_estimado),

         quantidade =
           readr::parse_number(quantidade,
                               locale = readr::locale(decimal_mark = ",")),

         situacao = as.factor(situacao)) %>%
  mutate(kg_fornecidos = quantidade * kg_por_unid) %>%
  mutate(melhor_lance_kg = melhor_lance / kg_por_unid) %>%
  mutate(val_est_kg = valor_estimado / kg_por_unid)

# DEFLACIONANDO ---------------------------------------------------------------

# Criando variaveis de tempo
df_atas <- df_atas %>%
  create_time_variables()

# Adicionado coluna com indices do IPCA
df_atas <- read_csv('data/ipca.csv') %>%
  mutate(inicio_mes = lubridate::ymd(inicio_mes)) %>%
  right_join(df_atas, by = 'inicio_mes') %>%
  rename(indice_ipca_mes = ipca)

# Deflacionando
df_atas <- df_atas %>%
  mutate(
    val_est_kg_defl =
      PregoesBR::deflacionar(val_est_kg,
                             indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes),

    melhor_lance_kg_defl =
      PregoesBR::deflacionar(melhor_lance_kg,
                             indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes)
    ) %>%
  filter(ano != '2018') # <<<<

# CORRIGINDO: LIMITE DE PRECOS ------------------------------------------------

# Correcao com base em limite de precos deflacionados
df_atas <- df_atas %>%
  # win_bid: melhor lance por resma, corrigido e deflacionado
  mutate(win_bid_limite =
           corrigir_erro_de_medida(val_est_kg_defl, 40, quantidade),
         # reserve: valor estimado por resma, corrigido e deflacionado
         reserve_limite =
           corrigir_erro_de_medida(melhor_lance_kg_defl, 40, quantidade))

# CORRIGINDO: MISSING DATA ----------------------------------------------------

# Correcao com base em missing data de quantidade no resultado
df_atas <- df_atas %>%
  mutate(
    melhor_lance_missing =
      corrigir_erro_missing(quantidade_resultado, melhor_lance_kg, quantidade),

    val_est_missing =
      corrigir_erro_missing(quantidade_resultado, val_est_kg, quantidade),

    win_bid_missing =
      PregoesBR::deflacionar(x = melhor_lance_missing,
                             indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes),

    reserve_missing =
      PregoesBR::deflacionar(x = val_est_missing,
                             indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes)
    )

# CORRIGINDO: DADOS DE PROPOSTAS ----------------------------------------------
# Criando coluna logica para identificar casos de melhor lance global
df_atas <- df_atas %>%
  mutate(preco_global =
           checar_preco_global(menor_proposta = menor_proposta,
                               menor_proposta_global = menor_proposta_global,
                               lance_referencia = melhor_lance))

# Correcao com base no valor da menor proposta (unitaria vs. global)
df_atas <- df_atas %>%
  mutate(

    win_bid_kg =
      if_else(preco_global, melhor_lance_kg / quantidade, melhor_lance_kg) %>%
      PregoesBR::deflacionar(indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes),

    reserve_kg =
      if_else(preco_global, val_est_kg / quantidade, val_est_kg) %>%
      PregoesBR::deflacionar(indice_referencia = ipca_periodo_referencia,
                             indice_no_periodo = indice_ipca_mes)
    )

# NUMERO DE FORNECEDORES ------------------------------------------------------
calcular_numero_fornecedores <-
  possibly(.f = function(x) length(unique(x$CNPJ_CPF)), otherwise = 0L)

df_atas <- df_atas %>%
  # Media da tabela de frequencia do CNPJ_CPF dos lances apos a abertura
  mutate(avg_bids_per_bidder = map_dbl(.x = lances_clean,
                                       .f = ~ mean(table(.x["CNPJ_CPF"]))),
         median_bids_per_bidder = map_dbl(.x = lances_clean,
                                       .f = ~ median(table(.x["CNPJ_CPF"]))),
         num_forn_lances = map_int(.x = lances_clean,
                                   .f = calcular_numero_fornecedores),
         num_forn_propostas = map_int(.x = propostas,
                                      .f = calcular_numero_fornecedores))

# MARGEM PREFERENCIA ----------------------------------------------------------
df_atas <- df_atas %>%
  mutate(margem_preferencia =
           ifelse(is.na(margem_preferencia), 'Nao', margem_preferencia) %>%
           str_replace('Não', 'Nao') %>%
           str_remove_all('\t') %>% str_remove_all(',0000') %>%
           str_remove_all('Sim .+Percentual: ') %>% str_trim(side = 'both') %>%
           as.factor())

# DECRETO 7174 ----------------------------------------------------------------
df_atas <- df_atas %>%
  mutate(decreto_7174 = ifelse(is.na(decreto_7174), 'Nao', 'Sim') %>%
           str_replace('Não', 'Nao') %>%
           as.factor())

# EXCLUINDO OBS. DE CAFÉ SOLÚVEL OU EM GRÃOS ----------------------------------
# Observações identificadas mediante análise dos dados de qualidade
df_atas <- df_atas %>%
  filter(!id_item  %in% c('16047300000120110040', '16047300000120120040',
                         '20020600000120120004', '16047300000620110040',
                         '17032000000220140002', '78700000001220130020'))

# EXCLUINDO LEILOES QUE NAO FORAM CONCLUÍDOS ----------------------------------
# Não há dados do vencedor/marca vencedora para esses leiloes
df_atas <- df_atas %>%
  filter(!situacao %in% c('Em análise', 'Cancelado na aceitação'))

saveRDS(df_atas, "data/cnet_cafe.rds")

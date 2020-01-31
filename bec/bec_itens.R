library(tidyverse)

# Partial function para limpar dados numéricos
parse_number_bec <- partial(parse_number,
                            locale = locale(decimal_mark = ',',
                                            grouping_mark = '..'))

# Iterando entre arquivos com dados brutos obtidos pelo API
for (i in 1:3) {
  filename <- str_c('data/bec_dados_parte', i, '.rds')
  message(paste0('Reading ', filename, '...'))
  message(paste0('Processing ', filename, '...'))

  # Lê dados, seleciona variaveis, limpa nomes
  bec_partial <- readRDS(filename) %>%
    select(id_pregao = OC, municipio = MUNICIPIO,
           dt_inicio = DESC_ATA_GERADAPR.OCCompleta.DataInicio,
           dt_fim = DT_FIM, qtd_itens = QTD_ITENS,
           unidade_compradora = UNIDADE_COMPRADORA,
           ente_federativo = DESC_ATA_GERADAPR.OCCompleta.EnteFederativo,
           andamentos = DESC_ATA_GERADAPR.OCCompleta.AndamentosItensGrupos,
           itens = ITENS,
           encerramento = DESC_ATA_GERADAPR.OCCompleta.Encerramento)


  # Seleciona apenas pregoes com dados em ITENS, desaninha e cria id_item
  itens_unnested <- bec_partial %>%
    select(-andamentos, -encerramento) %>%
    filter(map_lgl(.x = itens, .f = ~ is.data.frame(.x))) %>%
    unnest(cols = itens) %>%
    mutate(id_item =
             str_c(id_pregao, str_pad(string = NR_SEQUENCIA_ITEM,
                                      width = 4,side = 'left', pad = '0'))) %>%
    distinct()

  names(itens_unnested)[8:14] <- tolower(names(itens_unnested)[8:14])

  # Desaninha 'andamentos' e cria 'id_item'
  andamentos_unnested <- bec_partial %>%
    select(id_pregao, andamentos) %>%
    unnest(cols = andamentos) %>%
    mutate(indice_item = str_extract(Descricao, '\\d+')) %>%
    mutate(id_item =
             str_c(id_pregao, str_pad(string = indice_item, width = 4,
                               side = 'left', pad = '0'))) %>%
    distinct(id_item, .keep_all = TRUE)

    andamentos_unnested <- andamentos_unnested %>%
      select(id_item, id_pregao,
             propostas = ListaPropostas, lances = ListaLances,
             preferencia_df = ListaDireitoPreferencia,
             negociacao_df = ListaNegociacoes,
             aceitabilidade_df = ListaAnaliseAceitabilidade,
             adesao_df = ListaAdesao,
             habilitacao_df = ListaHabilitacao,
             preferencia_texto = TextoDireitoPreferencia,
             negociacao_texto = TextoNegociacao,
             aceitabilidade_texto = TextoAnaliseAceitabilidade,
             adesao_texto = TextoAdesao,
             habilitacao_texto = TextoHabilitacao,
             tipo = FichaItemGrupo.Tipo,
             descricao_detalhada = FichaItemGrupo.Descricao,
             menor_valor = FichaItemGrupo.MenorValor,
             quantidade_andamento = FichaItemGrupo.Qdte,
             unidade_fornecimento_andamento = FichaItemGrupo.UnidadeFornecimento,
             municipio_andamento = FichaItemGrupo.Municipio,
             cnpj_fornecedor = FichaItemGrupo.CNPJ,
             nome_fornecedor = FichaItemGrupo.NomeFornecedor,
             num_propostas = FichaItemGrupo.QtdePropostas,
             num_propostas_desistencia = FichaItemGrupo.QtdePropostasDesistencia,
             num_propostas_entregues = FichaItemGrupo.QtdePropostasEntregues,
             num_propostas_classificadas = FichaItemGrupo.QtdePropostasClassificadas
             )

  # Seleciona apenas pregoes com dados de Encerramento, desaninha, cria id_item
  encerramento_unnested <- bec_partial %>%
    select(id_pregao, encerramento) %>%
    filter(map_lgl(.x = encerramento, .f = ~ is.data.frame(.x))) %>%
    unnest(cols = encerramento) %>%
    mutate(dt_encerramento = paste(Data, Hora)) %>%
    select(id_pregao, responsavel = Responsavel,
           encerramento_texto = Descricao, dt_encerramento) %>%
    distinct()

  # Left join
  bec_partial_join <- itens_unnested %>%
    left_join(andamentos_unnested, by = c('id_pregao', 'id_item')) %>%
    left_join(encerramento_unnested, by = 'id_pregao')

  # Reordenando e corrigindo tipos de variaveis
  bec_partial_join <- bec_partial_join %>%
    select(id_item, id_pregao, municipio, dt_inicio, dt_fim,
           dt_encerramento, qtd_itens:encerramento_texto) %>%
    mutate(dt_inicio = lubridate::dmy_hms(dt_inicio),
           dt_fim = lubridate::dmy_hms(dt_fim),
           dt_encerramento = lubridate::dmy_hms(dt_encerramento),
           qtd_itens = as.numeric(qtd_itens),
           nr_sequencia_item = as.numeric(nr_sequencia_item),
           quantidade = parse_number_bec(quantidade),
           menor_valor = parse_number_bec(menor_valor))

  # Salvando
  output_filename <- str_c('bec/montando_base_v2/bec_itens_parte', i, '_v2.rds')
  message(paste0('Writing ', output_filename, '...'))
  saveRDS(bec_partial_join, output_filename)
  message('Done!')

  # Liberando RAM
  rm(list = ls()[str_detect(ls(), 'bec_|_unnested')])
  gc()
}

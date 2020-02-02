library(plotly)
library(trelliscopejs)
library(PregoesBR)

# Importando dados ------------------------------------------------------------
df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_lances_completo <- readRDS('data/cnet_lances.rds')

df_atas <- readRDS('data/cnet_cafe.rds') %>%
  select(id_item, inicio_fase_aleatoria, situacao, tratamento_diferenciado,
         decreto_7174, margem_preferencia, melhor_lance, valor_negociado,
         kg_fornecidos, kg_por_unid, sigla_uf, municipio, nome_uasg,
         menor_proposta, menor_proposta_global, menor_lance, valor_estimado,
         win_bid_kg, reserve_kg, ano, avg_bids_per_bidder,
         num_forn_propostas, num_forn_lances)

# Data wrangling --------------------------------------------------------------
df_plot_data <- df_lances_completo %>%
  select(-regime_juridico_20s, -regime_juridico_3s, -data_abertura) %>%
  mutate(mes = lubridate::month(abertura_lances) %>% as.factor()) %>%
  # Aninhando dados relativos aos lances de cada leilao
  group_by(id_item, abertura_lances, mes, regime_juridico) %>%
  nest() %>%
  # Joining dados relativos a leiloes
  left_join(df_atas, by = 'id_item') %>%
  mutate(total_lances = map_dbl(.x = data, .f = nrow)) %>%
  arrange(desc(total_lances)) %>%
  # Coluna constante com inicio da fase aleatoria nos DFs com dados de lances
  mutate(data =
           map2(.x = data, .y = inicio_fase_aleatoria,
                .f = ~ mutate(.x, inicio_fase_aleatoria = .y))) %>%
  # Criando ranking de participantes, segundo número de lances submetidos
  mutate(
    data =
      map(.x = data,
          .f = ~ group_by(.x, CNPJ_CPF) %>%
            mutate(n_lances_forn = n()) %>%
            ungroup() %>%
            mutate(ranking_lances = dense_rank(desc(n_lances_forn))) %>%
            mutate(Fornecedor = integer_to_letter(ranking_lances)))
         ) %>%
  mutate(
    # Variavel com o numero de lances do fornecedor que mais deu lances
    n_lances_forn1 = map_dbl(.x = data,
                             .f = ~ filter(.x, ranking_lances == 1) %>% nrow()),
    # Variavel com o numero de lances do segundo fornecedor que mais deu lances
    n_lances_forn2 = map_dbl(.x = data,
                             .f = ~ filter(.x, ranking_lances == 2) %>% nrow()),
    # CNPJ do fornecedor que mais deu lances
    cnpj_forn1 = map_chr(.x = data,
                         .f = ~ filter(.x, ranking_lances == 1) %>% 
                           select(CNPJ_CPF) %>% slice(1) %>% unlist()),
    # Incremento/desconto normalizado mediano
    median_inc = map_dbl(.x = data,
                         .f = ~ median(.x$norm_inc_first, na.rm = TRUE))
    )

# Montando gráficos plotly ----------------------------------------------------
df_plot <- df_plot_data %>%
  mutate(panel = 
           map_plot(.x = data, .f = ~ PregoesBR::bid_plot(data = .x))) %>%
  select(-data)

# Ajustando cognostics --------------------------------------------------------
df_plot$id_item <- 
  cog(df_plot$id_item,
      default_label = TRUE,
      desc = 'Numero de 20 digitos identificador do leilao')

df_plot$abertura_lances <- 
  cog(df_plot$abertura_lances,
      desc = 'Data e horario de abertura da fase de lances')

df_plot$mes <- 
  cog(df_plot$mes,
      desc = 'Numero correspondente ao mes em que o leilao foi realizado')

df_plot$regime_juridico <- 
  cog(df_plot$regime_juridico, desc = 'Regras aplicaveis de intervalo minimo',
      default_label = TRUE)

df_plot$inicio_fase_aleatoria <- 
  cog(df_plot$inicio_fase_aleatoria,
      desc = 'Inicio da fase aleatoria (encerramento iminente)')

df_plot$situacao <- 
  cog(df_plot$situacao, desc = 'Situacao do pregao')

df_plot$tratamento_diferenciado <- 
  cog(df_plot$tratamento_diferenciado, default_label = FALSE)

df_plot$margem_preferencia <-
  cog(df_plot$margem_preferencia, default_label = FALSE)

df_plot$decreto_7174 <- 
  cog(df_plot$decreto_7174, default_label = FALSE)

df_plot$melhor_lance <- 
  cog(df_plot$melhor_lance, desc = 'Lance vencedor, tal como informado na ata')

df_plot$valor_negociado <- 
  cog(df_plot$valor_negociado,
      desc = str_c('Valor apos negociacao;',
                   'indicado apenas para casos em que houve negociacao'))

df_plot$kg_fornecidos <-
  cog(df_plot$kg_fornecidos,
      desc = 'Quantidade de cafe negociada no leilao, em quilogramas')

df_plot$kg_por_unid <- 
  cog(df_plot$kg_por_unid,
      desc = 'Quilogramas por unidade de fornecimento utilizada no leilao')

df_plot$sigla_uf <-
  cog(df_plot$sigla_uf,
      desc = 'Unidade da Federacao do orgao publico comprador (UASG)',
      default_label = TRUE)

df_plot$municipio <- 
  cog(df_plot$municipio,
      desc = 'Municipio do orgao publico comprador (UASG)')

df_plot$nome_uasg <- 
  cog(df_plot$nome_uasg, desc = 'Nome do orgao publico comprador (UASG)')

df_plot$menor_proposta <- 
  cog(df_plot$menor_proposta,
      desc = 'Menor proposta unitaria apresentada, em reais')

df_plot$menor_proposta_global <- 
  cog(df_plot$menor_proposta_global,
      desc = 'Menor proposta global apresentada, em reais')

df_plot$menor_lance <- 
  cog(df_plot$menor_lance,
      desc = str_c('Menor lance submetido; pode nao coincidir com o lance',
                   'vencedor, em casos de lances invalidos ou fornecedor ',
                   'que nao consegue se habilitar, por exemplo'))

df_plot$valor_estimado <- 
  cog(df_plot$valor_estimado,
      desc = str_c('Valor estimado do produto, tal como ',
                   'informado pelo orgao publico comprador no edital'))

df_plot$win_bid_kg <-
  cog(df_plot$win_bid_kg, default_label = TRUE,
      desc = 'Lance vencedor em R$/kg, deflacionado pelo IPCA')

df_plot$reserve_kg <-
  cog(df_plot$reserve_kg,
      desc = str_c('Preco de reserva; valor estimado informado',
                   ' no edital, em R$/kg, deflacionado pelo IPCA'))

df_plot$ano <- 
  cog(df_plot$ano, desc = 'Ano de realizacao do leilao')

df_plot$avg_bids_per_bidder <- 
  cog(df_plot$avg_bids_per_bidder,
      desc = 'Numero medio de lances por fornecedor')

df_plot$num_forn_propostas <- 
  cog(df_plot$num_forn_propostas, default_label = FALSE,
      desc = 'Numero de fornecedores que apresentaram propostas')

df_plot$num_forn_lances <- 
  cog(df_plot$num_forn_lances, default_label = TRUE,
      desc = 'Numero de fornecedores que participaram da fase de lances')

df_plot$total_lances <- 
  cog(df_plot$total_lances, default_label = TRUE,
      desc = 'Total de lances registrados no leilao')

df_plot$n_lances_forn1 <- 
  cog(df_plot$n_lances_forn1, default_label = FALSE, 
      desc = str_c('Numero de lances submetidos ',
                   'pelo participante que mais deu lances'))

df_plot$n_lances_forn2 <- 
  cog(df_plot$n_lances_forn2, default_label = FALSE,
      desc = str_c('Numero de lances submetidos ',
                   'pelo segundo participante que mais deu lances'))

df_plot$cnpj_forn1 <- 
  cog(df_plot$cnpj_forn1,
      desc = 'CNPJ do fornecedor que mais registrou lances')

df_plot$median_inc <- 
  cog(df_plot$median_inc,
      desc = str_c('Mediana do incremento entre menores lances, ',
                   'normalizado pelo primeiro lance'))

# Compilando e salvando -------------------------------------------------------
trelliscope(df_plot, name = 'Comprasnet', path = 'plots/', nrow = 1, ncol = 2,
            desc = str_c('Leiloes eletronicos de compra de cafe realizados no',
                         ' Comprasnet entre 01/03/2011 e 31/12/2017'))
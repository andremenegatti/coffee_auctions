library(plotly)
library(crosstalk)
library(tidyverse)
library(trelliscopejs)

# Importando dados ------------------------------------------------------------
df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

df_lances_completo <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < lubridate::ymd('2016-01-01')) %>%
  select(-regime_juridico_20s, -regime_juridico_3s)

df_atas <- readRDS('data/cnet_cafe.rds') %>%
  select(id_item, abertura_lances, inicio_fase_aleatoria, situacao, 
         tratamento_diferenciado, decreto_7174, margem_preferencia,
         melhor_lance, valor_negociado, kg_fornecidos, kg_por_unid,
         sigla_uf, municipio, nome_uasg, menor_proposta,
         menor_proposta_global, menor_lance, valor_estimado,
         win_bid_kg, reserve_kg, ano, avg_bids_per_bidder,
         num_forn_propostas, num_forn_lances)

# Data wrangling --------------------------------------------------------------
# Nesting df_lances no nível do leilão
df_plot_data <- df_lances_completo  %>%
  nest(bids = c(-id_item, -abertura_lances, -regime_juridico)) %>% 
  mutate(mes = lubridate::month(abertura_lances) %>% as.factor())

# Joining dados do leilão
df_plot_data <- df_plot_data %>% 
  inner_join(df_atas, by = c('id_item', 'abertura_lances')) %>% 
  mutate(n_bids = map_dbl(.x = bids, .f = nrow)) %>%
  arrange(desc(n_bids))
  
# Nova coluna bids_random apenas com lances da fase aleatória
df_plot_data <- df_plot_data %>% 
  mutate(bids_random = map2(.x = bids, .y = inicio_fase_aleatoria,
                              .f = ~ filter(.x, data_hora >= .y)),
         # Coluna com total de lances na fase aleatória
         n_bids_random = map_dbl(.x = bids_random, .f = nrow))  %>% 
  # Apenas leiloes com pelo menos 2 participantes na fase aleatória
  filter(map_lgl(.x = bids_random, .f = ~ length(unique(.x$CNPJ_CPF)) > 1))

# Criando ranking de participantes, segundo número de lances submetidos
df_plot_data <- df_plot_data %>% 
  mutate(bids_random = map(.x = bids_random,
                           .f = ~ count(.x, CNPJ_CPF) %>% 
                             mutate(ranking_bids = row_number(-n)) %>% 
                             rename(n_bids_firm = n) %>%
                             right_join(.x, by = 'CNPJ_CPF')))

# Colunas com lances dos 2 fornecedores mais ativos
df_plot_data <- df_plot_data %>% 
  mutate(bidder_no1 = map(.x = bids_random,
                            .f = ~ filter(.x, ranking_bids == 1)),
         bidder_no2 = map(.x = bids_random, 
                           .f = ~ filter(.x, ranking_bids == 2)))

# Numero de lances e CNPJ dos 2 fornecedores mais ativos
df_plot_data <- df_plot_data %>% 
  mutate(n_bids_firm1 = map_dbl(.x = bidder_no1, .f = nrow),
         n_bids_firm2 = map_dbl(.x = bidder_no2, .f = nrow),
         cnpj_firm1 = map_chr(.x = bidder_no1, .f = ~ .x$CNPJ_CPF[1]),
         cnpj_firm2 = map_chr(.x = bidder_no2, .f = ~ .x$CNPJ_CPF[1])) %>% 
  select(-bidder_no1, -bidder_no2) # Não serão mais utilizadas

# Variáveis com mediana e média do incremento normalizado na fase de lances
df_plot_data <- df_plot_data %>% 
  mutate(
    median_inc_random = 
      map_dbl(.x = bids_random, .f = ~ median(.x$norm_inc_first, na.rm = TRUE)),
    avg_inc_random = 
      map_dbl(.x = bids_random, .f = ~ mean(.x$norm_inc_first, na.rm = TRUE))
    )

# Convertendo ranking em factor para usar nos gráficos
df_plot_data <- df_plot_data %>% 
  mutate(bids_random = 
           map(.x = bids_random,
               .f = ~ .x %>% 
                 mutate(Fornecedor = fct_reorder(CNPJ_CPF, ranking_bids)))) %>% 
  arrange(desc(n_bids_random))

# Montando gráficos plotly ----------------------------------------------------
source('comprasnet/bot_search/auction_plotly.R')

df_plot <- df_plot_data %>%
  mutate(panel = map_plot(.x = bids_random, .f = ~ auction_plotly(.x)))

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
      desc = 'Mes em que o leilao foi realizado')

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
      desc = 'Lance vencedor em R$/kg, em preços de dez/2015 (IPCA)')

df_plot$reserve_kg <-
  cog(df_plot$reserve_kg,
      desc = str_c('Preco de reserva; valor estimado informado',
                   ' no edital, em R$/kg, em preços de dez/2015 (IPCA)'))

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

df_plot$n_bids <- 
  cog(df_plot$n_bids, default_label = TRUE,
      desc = 'Total de lances registrados no leilao')

df_plot$n_bids_random <- 
  cog(df_plot$n_bids_random, default_label = TRUE,
      desc = 'Total de lances registrados durante a fase aleatoria')

df_plot$n_bids_firm1 <- 
  cog(df_plot$n_bids_firm1, default_label = FALSE, 
      desc = str_c('Numero de lances submetidos ',
                   'pelo participante que mais deu lances'))

df_plot$n_bids_firm2 <- 
  cog(df_plot$n_bids_firm2, default_label = FALSE,
      desc = str_c('Numero de lances submetidos ',
                   'pelo segundo participante que mais deu lances'))

df_plot$cnpj_firm1 <- 
  cog(df_plot$cnpj_firm1,
      desc = 'CNPJ do fornecedor que mais registrou lances')

df_plot$median_inc_random <- 
  cog(df_plot$median_inc_random,
      desc = str_c('Mediana do incremento entre lances de cobertura ',
                   'registrados na fase aleatória ',
                   '(incremento normalizado pelo primeiro lance)'))

df_plot$avg_inc_random <- 
  cog(df_plot$avg_inc_random,
      desc = str_c('Media do incremento entre lances de cobertura ',
                   'registrados na fase aleatoria ',
                   '(incremento normalizado pelo primeiro lance)'))

# Compilando e salvando -------------------------------------------------------
trelliscope(df_plot, nrow = 1, ncol = 2,
            name = 'Comprasnet', path = 'plots/trelliscope/complete',
            desc = str_c('Leiloes eletronicos de compra de cafe realizados no',
                         ' Comprasnet entre 01/03/2011 e 31/12/2017'))

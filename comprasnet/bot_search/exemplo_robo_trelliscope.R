library(PregoesBR)
library(plotly)
library(trelliscopejs)
source('R/bid_plot.R')

#### IMPORTANDO DADOS ####
df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

df_lances_completo <- readRDS('Comprasnet/cnet_df_lances_completo_v2.rds')

# df_marcas <- readRDS("~/Mestrado/Dissertacao/Auctions/Marcas/marcas_full.rds") %>%
#   select(id_item, marca_vencedor_principais, marca_vencedor, vencedor) %>%
#   filter(!str_detect(id_item, 'OC'))

df_atas <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  select(id_item, inicio_fase_aleatoria, situacao, tratamento_diferenciado,
         decreto_7174, margem_preferencia, melhor_lance, valor_negociado, kg_fornecidos, kg_por_unid,
         sigla_uf, municipio_uasg, nome_uasg,
         menor_proposta, menor_proposta_global, menor_lance, valor_estimado, win_bid_kg, reserve_kg,
         ano,
         # inicio_ano, inicio_semana, inicio_bimestre, inicio_trimestre, inicio_semestre,
         avg_bids_per_bidder, num_forn_propostas, num_forn_lances)

#### DATA-WRANGLING ####

df_plot <- df_lances_completo %>%
  select(-regime_juridico_20s, -regime_juridico_3s, -data_abertura) %>%
  mutate(mes = lubridate::month(abertura_lances) %>% as.factor()) %>%
  group_by(id_item, abertura_lances, mes, regime_juridico) %>%
  nest() %>%
  left_join(df_atas, by = 'id_item') %>%
  mutate(total_lances = map_dbl(.x = data, .f = nrow)) %>%
  arrange(desc(total_lances)) %>%
  # slice(1:25) %>%
  mutate(data = map2(.x = data, .y = inicio_fase_aleatoria,
                     .f = ~ .x %>% mutate(inicio_fase_aleatoria = .y)
                     )
         )


df_plot2 <- df_plot %>%
  mutate(data = map(.x = data,
                    .f = ~ .x %>%
                      group_by(CNPJ_CPF) %>%
                      mutate(n_lances_forn = n()) %>%
                      ungroup() %>%
                      mutate(ranking_lances = dense_rank(desc(n_lances_forn))) %>%
                      # mutate(Fornecedor = ifelse(ranking_lances < 6, CNPJ_CPF, "Outros")) %>%
                      mutate(Fornecedor = integer_to_letter(ranking_lances))
                    )
         ) %>%
  mutate(n_lances_forn1 = map_dbl(.x = data, .f = ~ .x %>% filter(ranking_lances == 1) %>% nrow()),
         n_lances_forn2 = map_dbl(.x = data, .f = ~ .x %>% filter(ranking_lances == 2) %>% nrow()),
         cnpj_forn1 = map_chr(.x = data, .f = ~ .x %>% filter(ranking_lances == 1) %>% select(CNPJ_CPF) %>% slice(1) %>% unlist())) %>%
  mutate(median_inc = map_dbl(.x = data, .f = ~ median(.x$norm_inc_first, na.rm = TRUE)))

# Montando graficos plotly
df_plot2 <- df_plot2 %>%
  mutate(panel = map_plot(.x = data, .f = ~ bid_plot(data = .x, lines = FALSE, bestFit = FALSE) )) %>%
  select(-data)

# Ajustando cognostics
df_plot2$id_item <- cog(df_plot2$id_item, default_label = TRUE, desc = 'Numero de 20 digitos identificador do leilao')
df_plot2$abertura_lances <- cog(df_plot2$abertura_lances, desc = 'Data e horario de abertura da fase de lances')
df_plot2$mes <- cog(df_plot2$mes, desc = 'Numero correspondente ao mes em que o leilao foi realizado')
df_plot2$regime_juridico <- cog(df_plot2$regime_juridico, desc = 'Regras aplicaveis de intervalo minimo', default_label = TRUE)
df_plot2$inicio_fase_aleatoria <- cog(df_plot2$inicio_fase_aleatoria, desc = 'Inicio da fase aleatoria (encerramento iminente)')
df_plot2$situacao <- cog(df_plot2$situacao, desc = 'Situacao do pregao')
df_plot2$tratamento_diferenciado <- cog(df_plot2$tratamento_diferenciado, default_label = FALSE)
df_plot2$margem_preferencia <- cog(df_plot2$margem_preferencia, default_label = FALSE)
df_plot2$decreto_7174 <- cog(df_plot2$decreto_7174, default_label = FALSE)
df_plot2$melhor_lance <- cog(df_plot2$melhor_lance, desc = 'Lance vencedor, tal como informado na ata')
df_plot2$valor_negociado <- cog(df_plot2$valor_negociado, desc = 'Valor apos negociacao; indicado apenas para casos em que houve negociacao')
df_plot2$kg_fornecidos <- cog(df_plot2$kg_fornecidos, desc = 'Quantidade de cafe negociada no leilao, em quilogramas')
df_plot2$kg_por_unid <- cog(df_plot2$kg_por_unid, desc = 'Quilogramas por unidade de fornecimento utilizada no leilao')
df_plot2$sigla_uf <- cog(df_plot2$sigla_uf, desc = 'Unidade da Federacao do orgao publico comprador (UASG)', default_label = TRUE)
df_plot2$municipio_uasg <- cog(df_plot2$municipio_uasg, desc = 'Municipio do orgao publico comprador (UASG)')
df_plot2$nome_uasg <- cog(df_plot2$nome_uasg, desc = 'Nome do orgao publico comprador (UASG)')
df_plot2$menor_proposta <- cog(df_plot2$menor_proposta, desc = 'Menor proposta unitaria apresentada, em reais')
df_plot2$menor_proposta_global <- cog(df_plot2$menor_proposta_global, desc = 'Menor proposta global apresentada, em reais')
df_plot2$menor_lance <- cog(df_plot2$menor_lance, desc = 'Menor lance submetido; pode nao coincidir com o lance vencedor, em casos de lances invalidos ou fornecedor que nao consegue se habilitar, por exemplo')
df_plot2$valor_estimado <- cog(df_plot2$valor_estimado, desc = 'Valor estimado do produto, tal como informado pelo orgao publico comprador no edital')
df_plot2$win_bid_kg <- cog(df_plot2$win_bid_kg, default_label = TRUE, desc = 'Lance vencedor em R$/kg, deflacionado pelo IPCA')
df_plot2$reserve_kg <- cog(df_plot2$reserve_kg, desc = 'Preco de reserva; valor estimado informado no edital, em R$/kg, deflacionado pelo IPCA')
df_plot2$ano <- cog(df_plot2$ano, desc = 'Ano de realizacao do leilao')
df_plot2$avg_bids_per_bidder <- cog(df_plot2$avg_bids_per_bidder, desc = 'Numero medio de lances por fornecedor')
df_plot2$num_forn_propostas <- cog(df_plot2$num_forn_propostas, default_label = FALSE, desc = 'Numero de fornecedores que apresentaram propostas')
df_plot2$num_forn_lances <- cog(df_plot2$num_forn_lances, default_label = TRUE, desc = 'Numero de fornecedores que participaram da fase de lances')
df_plot2$total_lances <- cog(df_plot2$total_lances, default_label = TRUE, desc = 'Total de lances registrados no leilao')
df_plot2$n_lances_forn1 <- cog(df_plot2$n_lances_forn1, default_label = FALSE, desc = 'Numero de lances submetidos pelo participante que mais deu lances')
df_plot2$n_lances_forn2 <- cog(df_plot2$n_lances_forn2, default_label = FALSE, desc = 'Numero de lances submetidos pelo segundo participante que mais deu lances')
df_plot2$cnpj_forn1 <- cog(df_plot2$cnpj_forn1, desc = 'CNPJ do fornecedor que mais registrou lances')
df_plot2$median_inc <- cog(df_plot2$median_inc, desc = 'Mediana do incremento entre menores lances, normalizado pelo primeiro lance')


# Compilando e salvando
trelliscope(df_plot2,
            name = 'Comprasnet',
            path = 'C:/Users/Dell/Desktop/Trelliscope',
            desc = 'Leiloes eletronicos de compra de cafe realizados no Comprasnet entre 01/03/2011 e 31/12/2017',
            nrow = 1, ncol = 2)



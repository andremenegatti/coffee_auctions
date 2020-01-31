library(PregoesBR)

df_atas <- readRDS("Comprasnet/cnet_cafe_04.rds")


##### CDF da distancia dos ultimos lances de cada participante em relacao ao final do pregao ####

## ATENCAO: usamos a base sem pregoes com fase C superior a 30 min

df_atas <- df_atas %>%
  # Criando coluna com dfs que contem o horario do ultimo lance de cada fornecedor
  # Em cada df, criamos tambem uma coluna com a diferenca desse horario em relacao ao encerramento de lances
  mutate(ultimos_lances = pmap(.l = list(lances_POSIXct, encerramento_lances, abertura_lances),
                               .f = ~ filter(..1, data_hora > ..3) %>%  group_by(CNPJ_CPF) %>%
                                 summarise(last_bid = last(data_hora)) %>%
                                 mutate(diff = difftime(last_bid, ..2, units = 'secs'))))

# Criacao de dummies para identificar grupos de pregoes conforme duracao da fase C
df_atas <- df_atas %>%
  mutate(dummy_C = case_when(duracao_C > 10 & duracao_C <= 11 ~ '(10, 11]',
                             duracao_C > 15 & duracao_C <= 16 ~ '(15, 16]',
                             duracao_C > 20 & duracao_C <= 21 ~ '(20, 21]'))

# df_cdf: selecionando apenas variaveis de interesse e unnesting (ultimos lances)
df_cdf <- df_atas %>%
  select(id_item, abertura_lances, ano, duracao_C, duracao_total, dummy_C, ultimos_lances) %>%
  filter(!is.na(dummy_C)) %>%
  unnest() %>%
  mutate(diff_minutes = as.numeric(diff) / 60)

# df_cdf_quasi_hard <- df_cdf %>%
#   filter(abertura_lances < '2011-03-01')
#
# df_cdf_random <- df_cdf %>%
#   filter(abertura_lances >= '2011-03-01')


# Grafico: distribuicao acumulada dos ultimos lances dos fornecedores ao longo do tempo
# Vide Figura 5 de Szerman
## - Nota um aumento de atividade (maior inclinacao) no inicio da fase C
## - Tambem nota-se que parcela relevante dos ultimos lances sao submetidos antes da fase C
ggplot() +
  geom_step(data = df_cdf,
            mapping = aes(x = diff_minutes, color = dummy_C, linetype = dummy_C),
            stat = 'ecdf',
            size = 1) +
  coord_cartesian(xlim = c(-30,0)) +
  ylab('Probabilidade acumulada') +
  xlab('Minutos remanescentes') +
  scale_x_continuous(breaks = seq(-30, 0, 5),
                     labels = seq(30, 0, -5)) +
  scale_color_manual(name = 'Duracao fase C',
                     values = c('(10, 11]' = 'steelblue', '(15, 16]' = 'darkred', '(20, 21]' = 'darkgreen')) +
  scale_linetype_manual(name = 'Duracao fase C',
                        values = c('(10, 11]' = 'solid', '(15, 16]' = 'dashed', '(20, 21]' = 'dotted')) +
  geom_point(aes(x = -10, y = ecdf(x = df_cdf$diff_minutes[df_cdf$dummy_C == '(10, 11]']) (v = -10)),
             color = 'steelblue',
             size = 2) +
  geom_point(aes(x = -15, y = ecdf(x = df_cdf$diff_minutes[df_cdf$dummy_C == '(15, 16]']) (v = -15)),
             color = 'darkred',
             size = 2) +
  geom_point(aes(x = -20, y = ecdf(x = df_cdf$diff_minutes[df_cdf$dummy_C == '(20, 21]']) (v = -20)),
             color = 'darkgreen',
             size = 2) +
  theme(legend.position = c(0.2,0.8),
        legend.justification = 'center',
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.text.align = 0.5,
        legend.background = element_rect(fill = 'white', color = 'gray'))

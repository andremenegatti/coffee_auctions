library(PregoesBR)

df_atas <- readRDS('Comprasnet/cnet_cafe_02_v2.rds')

#NUMERO DE LANCES POR FORNECEDOR
df_atas <- df_atas %>%
  # Media da tabela de frequencia do CNPJ_CPF dos lances registrados apos a abertura
  mutate(avg_bids_per_bidder = map_dbl(.x = lances_clean,
                                       .f = ~ mean(table(.x["CNPJ_CPF"])))) %>%
  mutate(num_forn_lances = map_int(.x = lances_clean, .f = possibly(.f = function(x) length(unique(x$CNPJ_CPF)), otherwise = 0L))) %>%
  mutate(num_forn_propostas = map_int(.x = propostas, .f = possibly(.f = function(x) length(unique(x$CNPJ_CPF)), otherwise = 0L)))

# Criando colunas com dfs dos lances registrados nas fases A, B e C
df_atas <- df_atas %>%
  mutate(lances_A = pmap(.l = list(lances, abertura_lances, iminencia_encerramento),
                         .f = ~ filter(..1, data_hora >= ..2) %>% filter(data_hora < ..3))) %>%
  mutate(lances_B = pmap(.l = list(lances, iminencia_encerramento, inicio_fase_aleatoria),
                         .f = ~  filter(..1, data_hora >= ..2) %>% filter(data_hora < ..3))) %>%
  mutate(lances_C = map2(.x = lances, .y = inicio_fase_aleatoria,
                         .f = ~ filter(.x, data_hora >= .y)))

# Calculando numero medio de lances por fornecedor, em cada fase, para cada pregao
df_atas <- df_atas %>%
  mutate(avg_bids_per_bidder_A = map_dbl(.x = lances_A, .f = ~ mean(table(.x$CNPJ_CPF))),
         avg_bids_per_bidder_B = map_dbl(.x = lances_B, .f = ~ mean(table(.x$CNPJ_CPF))),
         avg_bids_per_bidder_C = map_dbl(.x = lances_C, .f = ~ mean(table(.x$CNPJ_CPF))))


#### HISTOGRAMAS ####

# Histogramas do numero medio de lances por fornecedor (cortados para melhorar escala)
## - Histogramas C e Total sao parecidos, assim como A e B
## - Quase todos os lances sao submetidos na fase C
hist_avg_bids_per_bidder <- ggplot(data = df_atas) +
  coord_cartesian(xlim = c(0, 30)) +
  xlab('Lances por fornecedor') +
  ylab('Contagem')

hist1 <- hist_avg_bids_per_bidder +
  geom_histogram(aes(x = avg_bids_per_bidder), binwidth = 1, fill = 'steelblue') +
  ggtitle('Numero medio de lances por fornecedor', subtitle =  'Todos os lances')

hist2 <- hist_avg_bids_per_bidder +
  geom_histogram(aes(x = avg_bids_per_bidder_C), binwidth = 1, fill = 'steelblue') +
  ggtitle('Numero medio de lances por fornecedor', subtitle =  'Apenas fase aleatoria - C')

hist3 <- hist_avg_bids_per_bidder +
  geom_histogram(aes(x = avg_bids_per_bidder_B), binwidth = 1, fill = 'steelblue') +
  ggtitle('Numero medio de lances por fornecedor', subtitle =  'Apenas iminencia de encerramento - B')

hist4 <- hist_avg_bids_per_bidder +
  geom_histogram(aes(x = avg_bids_per_bidder_A), binwidth = 1, fill = 'steelblue') +
  ggtitle('Numero medio de lances por fornecedor', subtitle =  'Apenas fase inicial - A')

gridExtra::grid.arrange(hist1, hist2, hist3, hist4)

# df_atas %>% select(avg_bids_per_bidder_A, avg_bids_per_bidder_B, avg_bids_per_bidder_C) %>% gather(key = 'var', value = 'bids_per_bidder') %>%
#   ggplot(aes(x = bids_per_bidder, fill = var)) +
#   geom_histogram(position = 'identity', alpha = 0.5)

## - Nao ha impacto evidente das regras de encerramento na fracao de lances fase C/total
ggplot(data = df_atas %>% select(avg_bids_per_bidder, avg_bids_per_bidder_C, regra_encerramento) %>% gather(-regra_encerramento, key = 'key', value =  'value')) +
  geom_histogram(aes(x = value, fill = key), position = 'identity', binwidth = 2, alpha = 0.4) +
  facet_wrap(facets = "regra_encerramento", scales = "free_y") +
  coord_cartesian(xlim = c(0, 30)) +
  xlab('Lances por fornecedor') +
  ylab('Contagem') +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_fill_discrete(labels = c('Total', 'Fase C')) +
  ggtitle('Histogramas - Média de lances por fornecedor')


#### CALCULANDO ESTATISTICAS DESCRITIVAS ####

# Estatisticas descritivas do numero medio de lances por fornecedor
## - Em media, ha mais lances na fase C
df_atas %>% select(starts_with('avg_')) %>% summary()
df_atas %>% select(starts_with('avg_')) %>% as.data.frame() %>% stargazer::stargazer(type = 'text')

# Mediana, media e maximo do numero medio de lances por fornecedor, por ano e fase
df_evolucao_num_lances <- df_atas %>%
  select(inicio_ano, starts_with('avg_')) %>%
  gather(-inicio_ano, key = 'fase', value = 'avg_bids_per_bidder') %>%
  mutate(fase = if_else(fase == 'avg_bids_per_bidder', 'Total', fase)) %>%
  mutate(fase = str_remove(fase, 'avg_bids_per_bidder_')) %>%
  group_by(inicio_ano, fase) %>%
  summarise(median = median(avg_bids_per_bidder, na.rm = TRUE),
            mean = mean(avg_bids_per_bidder, na.rm = TRUE),
            max = max(avg_bids_per_bidder, na.rm = TRUE))

df_evolucao_num_lances_trimestral <- df_atas %>%
  select(inicio_trimestre, starts_with('avg_')) %>%
  gather(-inicio_trimestre, key = 'fase', value = 'avg_bids_per_bidder') %>%
  mutate(fase = if_else(fase == 'avg_bids_per_bidder', 'Total', fase)) %>%
  mutate(fase = str_remove(fase, 'avg_bids_per_bidder_')) %>%
  group_by(inicio_trimestre, fase) %>%
  summarise(median = median(avg_bids_per_bidder, na.rm = TRUE),
            mean = mean(avg_bids_per_bidder, na.rm = TRUE),
            max = max(avg_bids_per_bidder, na.rm = TRUE))



#### GRAFICOS DE LINHA ####

# Evolucao anual
ggplot(data = df_evolucao_num_lances %>% filter(fase != 'Total'),
       mapping = aes(x = inicio_ano, group = fase, color = fase)) +
  geom_line(aes(y = mean), size = 1) +
  ylab('Lances') +
  xlab('Ano') +
  scale_color_discrete(name = 'Fase') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(2, 12, by = 2)) +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017)) +
  # ggtitle('Numero medio de lances por fornecedor') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted')

## Evolucao trimestral
ggplot(data = df_evolucao_num_lances_trimestral %>% filter(fase != 'Total'),
       mapping = aes(x = inicio_trimestre, group = fase, color = fase)) +
  geom_line(aes(y = mean), size = 1) +
  ylab('Lances') +
  xlab('Tempo') +
  scale_color_discrete(name = 'Fase') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  # ggtitle('Lances por fornecedor', subtitle = 'Média trimestral') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months') +
  scale_y_continuous(breaks = seq(2, 12, by = 2))


## Comparacao media e mediana (anual)
ggplot(data = df_evolucao_num_lances %>% filter(fase == 'C') %>% select(-max, -fase) %>% gather(-inicio_ano, key = 'stat', value = 'valor'),
       mapping = aes(x = inicio_ano, group = stat, color = stat)) +
  geom_line(aes(y = valor), size = 1) +
  ylab('Lances') +
  xlab('Ano') +
  scale_color_discrete(name = 'Estatistica', labels = c('Media', 'Mediana')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  ggtitle('Lances por fornecedor - media e mediana') +
  geom_vline(xintercept = c(5, 7), color = 'darkred', linetype = 'dashed')

# saveRDS(df_atas, "Comprasnet/cnet_cafe_03_v2.rds")

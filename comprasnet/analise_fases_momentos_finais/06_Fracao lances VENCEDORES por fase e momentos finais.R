library(PregoesBR)

df_atas <- readRDS("Comprasnet/cnet_cafe_05_v2.rds")


##### FRACAO DE LANCES VENCEDORES ####

df_lances_vencedores <- df_atas %>%
  # Eliminando as poucas observacoes em que o df de lances nao possui a coluna 'valor_lance'
  filter(map_lgl(.x = lances, .f = ~ 'valor_lance' %in% names(.x))) %>%
  # Criando coluna com dataframe que contem apenas o(s) menor(es) lance(s) de cada pregao
  mutate(df_winning_bid = map(.x = lances,
                              .f = ~ .x %>%
                                filter(valor_lance == min(valor_lance)))) %>%
  # No caso de empate, pegamos apenas o lance que foi registrado primeiro
  mutate(df_winning_bid = map(.x = df_winning_bid, .f = ~ slice(.x, 1))) %>%
  # Criando variavel com o horario do menor lance
  mutate(hora_win_bid = map_chr(.x = df_winning_bid, .f = ~ as.character(.x$data_hora)) %>% ymd_hms() ) %>%
  # Variaveis indicando a fase em que o menor lance foi submetido
  mutate(fase_win_bid = case_when(hora_win_bid > abertura_lances & hora_win_bid <= iminencia_encerramento ~ 'A',
                                  hora_win_bid > iminencia_encerramento & hora_win_bid <= inicio_fase_aleatoria ~ 'B',
                                  hora_win_bid > inicio_fase_aleatoria & hora_win_bid <= (encerramento_lances + 1) ~ 'C',
                                  hora_win_bid > encerramento_lances ~ 'Apos encerramento',
                                  hora_win_bid <= abertura_lances ~ 'Proposta')) %>%
  # Dummies indicando se o menor lance foi dado em certos intervalos proximos ao fim do pregao
  mutate(win_bid_last10sec = if_else(difftime(encerramento_lances, hora_win_bid) <= 10, TRUE, FALSE),
         win_bid_last30sec = if_else(difftime(encerramento_lances, hora_win_bid) <= 30, TRUE, FALSE),
         win_bid_last1min = if_else(difftime(encerramento_lances, hora_win_bid) <= 60, TRUE, FALSE),
         win_bid_last5min = if_else(difftime(encerramento_lances, hora_win_bid) <= 300, TRUE, FALSE),
         win_bid_last10min = if_else(difftime(encerramento_lances, hora_win_bid) <= 600, TRUE, FALSE))

df_lances_vencedores %>%
  group_by(regra_encerramento) %>%
  summarise(media = mean(win_bid_last10sec, na.rm = TRUE))

# Share de cada etapa no total de lances vencedores
## - A maior parte dos lances vencedores e registrada na etapa C
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento'))) %>%
  arrange(fase_win_bid)

# RANDOM CLOSE: média um pouco menor na fase C
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(dummy_regra_encerramento == 'random close') %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(dummy_regra_encerramento == 'random close') )) %>%
  arrange(fase_win_bid)

# QUASI-HARD: média bastante maior em C
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(dummy_regra_encerramento == 'quasi-hard close') %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(dummy_regra_encerramento == 'quasi-hard close')) ) %>%
  arrange(fase_win_bid)

# A PARTIR DE MARCO/2011:
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(data_abertura >= '2011-03-01') %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(data_abertura >= '2011-03-01') ) ) %>%
  arrange(fase_win_bid)

# Apos mudanca de regras de encerramento, mas antes da regra dos 20s
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(data_abertura > as.Date('2011-02-28'),
         data_abertura >= data_20s) %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(data_abertura > as.Date('2011-02-28'),
                                      data_abertura >= data_20s) ))


# Entre regra dos 20s e regra dos 3s
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(data_abertura >= data_20s,
         data_abertura < data_3s) %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(data_abertura >= data_20s,
                                      data_abertura < data_3s) ))

# Apos regra dos 3s
df_lances_vencedores %>%
  filter(fase_win_bid != 'Apos encerramento') %>%
  filter(data_abertura > data_3s) %>%
  group_by(fase_win_bid) %>%
  summarise(n = n(),
            share = n / nrow(df_lances_vencedores %>%
                               filter(fase_win_bid != 'Apos encerramento') %>%
                               filter(data_abertura > data_3s) ))


# Share de lances ganhadores por etapa: evolucao ANUAL
df_last_bids_fases <- df_lances_vencedores %>%
  group_by(inicio_ano) %>%
  mutate(count_ano = n()) %>%
  ungroup() %>%
  group_by(inicio_ano, fase_win_bid) %>%
  summarise(n = n(),
            count_ano = mean(count_ano),
            share = n / count_ano) %>%
  ungroup() %>%
  filter(fase_win_bid != "Apos encerramento")

# Share de lances ganhadores por etapa: evolucao TRIMESTRAL
df_last_bids_fases_trimestral <- df_lances_vencedores %>%
  group_by(inicio_trimestre) %>%
  mutate(count_trimestre = n()) %>%
  ungroup() %>%
  group_by(inicio_trimestre, fase_win_bid) %>%
  summarise(n = n(),
            count_trimestre = mean(count_trimestre),
            share = n / count_trimestre) %>%
  ungroup() %>%
  filter(fase_win_bid != "Apos encerramento")


# Share de lances ganhadores por etapa: evolucao SEMESTRAL
df_last_bids_fases_semestral <- df_lances_vencedores %>%
  group_by(inicio_semestre) %>%
  mutate(count_semestre = n()) %>%
  ungroup() %>%
  group_by(inicio_semestre, fase_win_bid) %>%
  summarise(n = n(),
            count_semestre = mean(count_semestre),
            share = n / count_semestre) %>%
  ungroup() %>%
  filter(fase_win_bid != "Apos encerramento")

# Criando graficos: evolucao da participacao (percentual) de cada fase no total de lances vencedores
# df_last_bids_fases_nested <- df_last_bids_fases %>%
#   nest(-fase_win_bid) %>%
#   mutate(plot = map2(.x = data, .y = fase_win_bid,
#                      .f = ~ ggplot(.x,
#                                    mapping = aes(x = inicio_ano, y = share, group = 1)) +
#                        geom_line(color = 'steelblue', size = 1) +
#                        ggtitle(.y)  +
#                        xlab('Ano') +
#                        ylab('Fracao') +
#                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#                              panel.grid.major.x = element_blank(),
#                              panel.grid.minor.x = element_blank()) +
#                        geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#                        geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
#                        geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
#                        scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017), date_minor_breaks = '3 months')
#   ))
#
# # Exibindo graficos: evolucao da participacao (percentual) de cada fase no total de lances vencedores
# ## Relacao inversa entre C e Propostas: porque ha um aumento em Propostas a partir de 2014???
# gridExtra::grid.arrange(df_last_bids_fases_nested$plot[[1]],
#                         df_last_bids_fases_nested$plot[[2]],
#                         df_last_bids_fases_nested$plot[[3]],
#                         df_last_bids_fases_nested$plot[[4]])

ggplot(df_last_bids_fases) +
  geom_line(mapping = aes(x = inicio_ano, y = share, group = 1), size = 1, color = 'steelblue') +
  xlab('Ano') +
  ylab('Fracao') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017), date_minor_breaks = '3 months') +
  facet_wrap( ~ fase_win_bid, scales = "free_y")

ggplot(df_last_bids_fases_trimestral) +
  geom_line(mapping = aes(x = inicio_trimestre, y = share, group = 1), size = 1, color = 'steelblue') +
  xlab('Trimestre') +
  ylab('Fracao') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017), date_minor_breaks = '3 months') +
  facet_wrap( ~ fase_win_bid, scales = "free_y")

ggplot(df_last_bids_fases_semestral) +
  geom_line(mapping = aes(x = inicio_semestre, y = share, group = 1), size = 1, color = 'steelblue') +
  xlab('Tempo') +
  ylab('Fracao') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, '- 1S'), date_minor_breaks = '3 months') +
  facet_wrap( ~ fase_win_bid, scales = "free_y")

# Mesmos dados, mas com escala igual entre os graficos
## - Valores relativamente constantes
ggplot(df_last_bids_fases,
       mapping = aes(x = inicio_ano, y = share, group = fase_win_bid, color = fase_win_bid)) +
  geom_line(size = 1) +
  # ggtitle('Fracao de lances vencedores por fase') +
  xlab('Ano') +
  ylab('Fracao do total de pregoes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = 'Fase') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017), date_minor_breaks = '3 months')


df_last_bids_fases_trimestral %>%
  ggplot(mapping = aes(x = inicio_trimestre, y = share, group = fase_win_bid, color = fase_win_bid)) +
  geom_line(size = 1) +
  # ggtitle('Fracao de lances vencedores por fase', subtitle = 'Médias trimestrais') +
  xlab('Tempo') +
  ylab('Fracao do total de pregoes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = 'Fase') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')



# Calculando shares de lances vencedores em momentos finais em relacao ao total de lances vencedores
df_last_bids_momentos_finais <- df_lances_vencedores %>%
  group_by(inicio_ano) %>%
  summarise(last10sec = mean(win_bid_last10sec),
            last30sec = mean(win_bid_last30sec),
            last1min = mean(win_bid_last1min),
            last5min = mean(win_bid_last5min),
            last10min = mean(win_bid_last10min))

df_last_bids_momentos_finais_trimestral <- df_lances_vencedores %>%
  group_by(inicio_trimestre) %>%
  summarise(last10sec = mean(win_bid_last10sec),
            last30sec = mean(win_bid_last30sec),
            last1min = mean(win_bid_last1min),
            last5min = mean(win_bid_last5min),
            last10min = mean(win_bid_last10min))

df_last_bids_momentos_finais_semestral <- df_lances_vencedores %>%
  group_by(inicio_semestre) %>%
  summarise(last10sec = mean(win_bid_last10sec),
            last30sec = mean(win_bid_last30sec),
            last1min = mean(win_bid_last1min),
            last5min = mean(win_bid_last5min),
            last10min = mean(win_bid_last10min))

# Grafico: evolucao anual do share de lances vencedores nos momentos finais
## Atencao para queda entre 2014 e 2016
ggplot(data = df_last_bids_momentos_finais %>%
         gather(-inicio_ano, key = remaining_time, value = share) %>%
         mutate(remaining_time = factor(remaining_time,
                                        levels = c('last10min', 'last5min', 'last1min', 'last30sec', 'last10sec'),
                                        labels = c('10 min', '5 min', '1 min', '30 seg', '10 seg'))),
       mapping = aes(x = inicio_ano, y = share, group = remaining_time, color = remaining_time)) +
  geom_line(size = 1) +
  xlab('Ano') +
  ylab('Fracao do total de pregoes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0.1, 0.7, 0.05)) +
  # ggtitle('Fracao de menores lances', subtitle = 'Final do pregao') +
  scale_color_discrete(name = 'Tempo restante') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017), date_minor_breaks = '3 months')


ggplot(data = df_last_bids_momentos_finais_trimestral %>%
         gather(-inicio_trimestre, key = remaining_time, value = share) %>%
         mutate(remaining_time = factor(remaining_time,
                                        levels = c('last10min', 'last5min', 'last1min', 'last30sec', 'last10sec'),
                                        labels = c('10 min', '5 min', '1 min', '30 seg', '10 seg'))),
       mapping = aes(x = inicio_trimestre, y = share, group = remaining_time, color = remaining_time)) +
  geom_line(size = 1) +
  xlab('Tempo') +
  ylab('Fracao do total de pregoes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0.1, 0.7, 0.05)) +
  # ggtitle('Fracao de menores lances', subtitle = 'Final do pregao') +
  scale_color_discrete(name = 'Tempo restante') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017), date_minor_breaks = '3 months')


ggplot(data = df_last_bids_momentos_finais_semestral %>%
         gather(-inicio_semestre, key = remaining_time, value = share) %>%
         mutate(remaining_time = factor(remaining_time,
                                        levels = c('last10min', 'last5min', 'last1min', 'last30sec', 'last10sec'),
                                        labels = c('10 min', '5 min', '1 min', '30 seg', '10 seg'))),
       mapping = aes(x = inicio_semestre, y = share, group = remaining_time, color = remaining_time)) +
  geom_line(size = 1) +
  xlab('Tempo') +
  ylab('Fracao do total de pregoes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0.1, 0.7, 0.05)) +
  # ggtitle('Fracao de menores lances', subtitle = 'Final do pregao') +
  scale_color_discrete(name = 'Tempo restante') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017), date_minor_breaks = '3 months')


# Tabela de shares

# Todo o período 2008-2017
df_lances_vencedores %>%
  select(win_bid_last10sec, win_bid_last30sec, win_bid_last1min, win_bid_last5min, win_bid_last10min) %>%
  gather(tempo_remanescente, bool) %>%
  group_by(tempo_remanescente) %>%
  summarise(n = sum(bool),
            mean = mean(bool)) %>%
  arrange(desc(mean)) %>%
  mutate(tempo_remanescente = str_remove(tempo_remanescente, 'win_bid_last'))


df_lances_vencedores %>%
  filter(data_abertura > as.Date('2011-02-28')) %>%
  select(win_bid_last10sec, win_bid_last30sec, win_bid_last1min, win_bid_last5min, win_bid_last10min) %>%
  gather(tempo_remanescente, bool) %>%
  group_by(tempo_remanescente) %>%
  summarise(n = sum(bool),
            mean = mean(bool)) %>%
  arrange(desc(mean)) %>%
  mutate(tempo_remanescente = str_remove(tempo_remanescente, 'win_bid_last'))

# saveRDS(df_lances_vencedores, 'Comprasnet/06_df_lances_vencedores_v2.rds')

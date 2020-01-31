library(PregoesBR)

df_atas <- readRDS("Comprasnet/cnet_cafe_01_v2.rds")

##### DURACAO DAS FASES ####

df_atas <- df_atas %>%
  mutate(duracao_A = difftime(iminencia_encerramento, abertura_lances, units = 'mins'),
         duracao_B = difftime(inicio_fase_aleatoria, iminencia_encerramento, units = 'mins'),
         duracao_C = difftime(encerramento_lances, inicio_fase_aleatoria, units = 'mins'),
         duracao_total = difftime(encerramento_lances, abertura_lances, units = 'mins'))

# Mudancas nas regras de encerramento, segundo Szerman:
## - Setembro/2010: de quasi-hard close para random close
## - Dezembro/2010: quasi-hard close novamente
## - Fevereiro/2011: volta para random close
df_atas <- df_atas %>%
  mutate(regra_encerramento = case_when(abertura_lances < date('2010-09-01') ~ 'Período I (quasi-hard close)',
                                        abertura_lances >= date('2010-09-01') & abertura_lances < date('2010-12-14') ~ 'Período II (random close)',
                                        abertura_lances >= date('2010-12-14') & abertura_lances < date('2011-03-01') ~ 'Período III (quasi-hard close)',
                                        abertura_lances >= date('2011-03-01') ~ 'Período IV (random close)') %>% as.factor(),
         dummy_regra_encerramento = if_else(regra_encerramento %in% c('Período I (quasi-hard close)', 'Período III (quasi-hard close)'), 'quasi-hard close', 'random close') %>% as.factor())

# Em algumas observacoes, a fase C durou mais do que 30 segundos, o que nao seria possivel pela lei
df_atas$duracao_C %>% as.numeric() %>%  summary()

# Excluindo observacoes em que a fase C durou mais de 30 segundos (INVESTIGAR MAIS)
df_atas_C30 <- df_atas %>%
  filter(duracao_C <= 30)

df_atas_C30 %>%
  group_by(regra_encerramento) %>%
  summarise(duracao_C = mean(duracao_C, na.rm = TRUE))

# Estatisticas descritivas da duracao das fases
# 1) Grande variacao na duracao da fase A: erro de medida associado a suspensao nos pregoes
# 2) Pequena duracao relativa da fase B
# 3) Media de duracao da fase C fica um pouco acima do que seria de se esperar de uma distribuicao uniforme (15 min)
df_atas_C30 %>% select(starts_with('duracao')) %>% mutate_all(as.numeric) %>% summary()
str_c(c('SD duracao_A: ' , 'SD duracao_B: ', 'SD duracao_C '), round(map_dbl(.x = list(df_atas_C30$duracao_A, df_atas_C30$duracao_B, df_atas_C30$duracao_C), .f = ~ sd(.x)), digits = 2))

# TRIMMING: 2,5% mais altos de duracao_A
df_duracao_clean <- trim_df(df_atas_C30, perc = 2.5, vars = 'duracao_A', tail = 'higher')

# Estatisticas descritivas e toda a amostra
df_duracao_clean %>% select(starts_with('duracao')) %>% mutate_all(as.numeric) %>% summary()
str_c(c('SD duracao_A: ' , 'SD duracao_B: ', 'SD duracao_C '), round(map_dbl(.x = list(df_duracao_clean$duracao_A, df_duracao_clean$duracao_B, df_duracao_clean$duracao_C), .f = ~ sd(.x)), digits = 2))
df_duracao_clean %>%
  select(starts_with('duracao')) %>%
  mutate_all(as.numeric) %>%
  as.data.frame() %>%
  stargazer::stargazer(decimal.mark = ',',
                       # type = 'text',
                       digit.separator = '.')

# Estatisticas descritivas QUASI-HARD CLOSE
df_duracao_clean %>%
  filter(dummy_regra_encerramento == 'quasi-hard close') %>%
  select(starts_with('duracao')) %>% mutate_all(as.numeric) %>%
  as.data.frame() %>%
  stargazer::stargazer(decimal.mark = ',',
                       # type = 'text',
                       digit.separator = '.')

# Estatisticas descritivas RANDOM CLOSE
df_duracao_clean %>%
  filter(dummy_regra_encerramento == 'random close') %>%
  select(starts_with('duracao')) %>%
  mutate_all(as.numeric) %>%
  as.data.frame() %>%
  stargazer::stargazer(decimal.mark = ',',
                       # type = 'text',
                       digit.separator = '.')

# Investigando a distribuicao da duracao da fase C ao longo dos anos, identifica-se mudanca de regimes de encerramento:
# library(plyr)
duracao_media_fase_C <- plyr::ddply(df_atas_C30,
                 plyr::.(regra_encerramento),
                 plyr::summarize,
                 duracao_media = paste("Média:", mean(duracao_C, na.rm = TRUE) %>% str_replace('\\.', ',') %>% str_extract('\\d{2},\\d{2}'), "minutos"))

ggplot(df_atas_C30) +
  geom_density(aes(x = as.numeric(duracao_C)), fill = 'steelblue', alpha = 0.7) +
  facet_wrap(facets = 'regra_encerramento') +
  theme(legend.position = 'none', panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0,30, 5)) +
  labs(x = 'Duração da fase aleatória (minutos)', y = 'Densidade') +
  geom_text(data = duracao_media_fase_C, aes(x = 7, y = 0.45, label = duracao_media),
            colour = "black", inherit.aes = FALSE, parse = FALSE)
  # ggtitle('Distribuição da duração da fase aleatória', subtitle = 'Comparação entre diferentes regras de encerramento')

# Dataframe com dados de duracao media anual de cada fase
df_duracao_media_fases <- df_duracao_clean %>%
  group_by(inicio_ano) %>%
  summarise(Total = mean(duracao_total),
            C = mean(duracao_C),
            B = mean(duracao_B),
            A = mean(duracao_A)) %>%
  ungroup() %>%
  mutate_at(vars(-inicio_ano), as.numeric) %>%
  gather(-inicio_ano, key = 'Fase', value = 'Duracao media (mins)')

# Agregacao trimestral
df_duracao_media_fases_trimestral <- df_duracao_clean %>%
  group_by(inicio_trimestre) %>%
  summarise(Total = mean(duracao_total),
            C = mean(duracao_C),
            B = mean(duracao_B),
            A = mean(duracao_A)) %>%
  ungroup() %>%
  mutate_at(vars(-inicio_trimestre), as.numeric) %>%
  gather(-inicio_trimestre, key = 'Fase', value = 'Duracao media (mins)') %>%
  rename(Tempo = inicio_trimestre)

# Grafico da evolucao da duracao media
## - Queda na duracao da fase C
ggplot(data = df_duracao_media_fases %>%
         filter(Fase %in% c('C', 'B', 'A')),
       mapping = aes(x = inicio_ano, y = `Duracao media (mins)`, group = Fase, color = Fase)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  # ggtitle('Evolução da duração dos pregoes - 2008 a 2017', subtitle = 'Médias anuais') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-01-31')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  ylab('Minutos') +
  xlab('Ano') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017), date_minor_breaks = '3 months')

df_duracao_media_fases_trimestral %>%
  filter(Fase %in% c('C', 'B', 'A')) %>%
  ggplot(mapping = aes(x = Tempo, y = `Duracao media (mins)`, group = Fase, color = Fase)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  # ggtitle('Duracao das fases A, B e C', subtitle = 'Média trimestral') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(as.character(2008:2017), ' - Q1'), date_minor_breaks = '3 months') +
  ylab('Minutos') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-01-31')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted')

# Colocando no grafico apenas a fase C, fica evidente o impacto da mudanca de regra de encerramento
ggplot(data = df_duracao_media_fases %>% filter(Fase == 'C'),
       mapping = aes(x = inicio_ano, y = `Duracao media (mins)`, group = 1)) +
  geom_line(size = 1, color = '#00BFC4') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  # ggtitle('Duração da fase C', subtitle = 'Média anual') +
  ylab('Minutos')

# Tambem nota-se um aumento na duracao media da fase B
ggplot(data = df_duracao_media_fases %>% filter(Fase == 'B'),
       mapping = aes(x = inicio_ano, y = `Duracao media (mins)`, group = 1)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  ggtitle('Duracao media de iminencia de encerramento')

# Grafico de area: participacao de cada fase na duracao total do pregao
## - Aumento na participacao das fases B e C
## - Consequencia da reducao da fase A (que pode ser decorrente de erro de medida/suspensao dos pregoes)
areaplt_frac_duration <- ggplot(data = df_duracao_media_fases %>%
                                  mutate(inicio_ano = as.numeric(str_extract(inicio_ano, '\\d{4}'))) %>%
                                  filter(Fase != 'Total')) +
  geom_area(aes(x = inicio_ano, y = `Duracao media (mins)`, fill = Fase), position = 'fill') +
  theme_minimal() +
  scale_fill_manual(labels = c('Fase A ', 'Fase B ', 'Fase C '), values = c('steelblue2', 'steelblue3', 'steelblue4')) +
  xlab('Ano') +
  ylab('Participação') +
  # ggtitle('Participação de cada fase na duração total do pregão', subtitle = 'Médias anuais') +
  scale_x_continuous(breaks = 2008:2017, labels = as.character(2008:2017)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0,00", "0,25", "0,50", "0,75", "1,00")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title = element_text(size = 19),
        legend.text = element_text(size = 17),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom')

areaplt_frac_duration

# saveRDS(df_atas, "Comprasnet/cnet_cafe_02_v2.rds")

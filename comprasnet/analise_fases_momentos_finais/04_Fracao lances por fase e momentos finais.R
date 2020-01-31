library(PregoesBR)

df_atas <- readRDS("Comprasnet/cnet_cafe_03_v2.rds")


#### FRACAO DE LANCES POR FASE/MOMENTOS FINAIS ####

df_atas <- df_atas %>%
  # Variaveis com o total de lances registrados no total e em determinados intervalos finais
  mutate(num_lances_total = map_int(.x = lances_clean, .f = ~ nrow(.x)),
         num_lances_last10sec = map2_int(.x = lances_clean, .y = encerramento_lances, .f = ~ filter(.x, data_hora >= .y - 10) %>% nrow()),
         num_lances_last30sec = map2_int(.x = lances_clean, .y = encerramento_lances, .f = ~ filter(.x, data_hora >= .y - 30) %>% nrow()),
         num_lances_last1min = map2_int(.x = lances_clean, .y = encerramento_lances, .f = ~ filter(.x, data_hora >= .y - 60) %>% nrow()),
         num_lances_last5min = map2_int(.x = lances_clean, .y = encerramento_lances, .f = ~ filter(.x, data_hora >= .y - 300) %>% nrow()),
         num_lances_last10min = map2_int(.x = lances_clean, .y = encerramento_lances, .f = ~ filter(.x, data_hora >= .y - 600) %>% nrow())) %>%
  # Variaveis com a fracao de lances em cada intervalo, em relacao ao total de lances
  mutate(fraction_last10sec = num_lances_last10sec / num_lances_total,
         fraction_last30sec = num_lances_last30sec / num_lances_total,
         fraction_last1min = num_lances_last1min / num_lances_total,
         fraction_last5min = num_lances_last5min / num_lances_total,
         fraction_last10min = num_lances_last10min / num_lances_total)

# Variaveis com numero absoluto de lances e fracao de lances em cada fase
df_atas <- df_atas %>%
  mutate(num_lances_A = map_int(.x = lances_A, .f = ~ nrow(.x)),
         num_lances_B = map_int(.x = lances_B, .f = ~ nrow(.x)),
         num_lances_C = map_int(.x = lances_C, .f = ~ nrow(.x)),
         num_lances_AB = num_lances_A + num_lances_B) %>%
  mutate(fraction_A = num_lances_A / num_lances_total,
         fraction_B = num_lances_B / num_lances_total,
         fraction_C = num_lances_C / num_lances_total,
         fraction_AB = num_lances_AB / num_lances_total)

# Tabela com media ANUAL do numero de lances e fracao de lances, por fases/momentos finais
df_lances_gather_summary <- df_atas %>%
  select(inicio_ano, starts_with("num_lances"), starts_with("fraction_")) %>%
  filter(num_lances_total > 0) %>%
  gather(-inicio_ano, key = var, value = value) %>%
  group_by(inicio_ano, var) %>%
  summarise(mean = mean(value)) %>%
  ungroup()

# Tabela com media TRIMESTRAL do numero de lances e fracao de lances, por fases/momentos finais
df_lances_gather_summary_trimestral <- df_atas %>%
  select(inicio_trimestre, starts_with("num_lances"), starts_with("fraction_")) %>%
  filter(num_lances_total > 0) %>%
  gather(-inicio_trimestre, key = var, value = value) %>%
  group_by(inicio_trimestre, var) %>%
  summarise(mean = mean(value)) %>%
  ungroup()


### Graficos ANUAIS

# Definindo configuracoes gerais
lineplot_lances_anual <- ggplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab('Ano') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))


# Lineplot: evolucao do numero medio de lances por fase
lnplt_nlances1 <- lineplot_lances_anual +
  geom_line(data = df_lances_gather_summary %>% filter(var %in% c('num_lances_A', 'num_lances_B', 'num_lances_C')),
            mapping = aes(x = inicio_ano, y = mean, group = var, color = var),
            size = 1) +
  scale_color_discrete(name = 'Fase', labels = c('A', 'B', 'C')) +
  # ggtitle('Media de lances por fase') +
  ylab('Numero de lances')

# Lineplot: evolucao do numero medio de lances nos minutos/segundos finais
lnplt_nlances2 <- lineplot_lances_anual +
  geom_line(data = df_lances_gather_summary %>%
              filter(str_detect(var, 'num_lances_last')) %>%
              mutate(var = factor(var, levels = c('num_lances_last10min', 'num_lances_last5min', 'num_lances_last1min', 'num_lances_last30sec', 'num_lances_last10sec'),
                                  labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec'))),
            mapping = aes(x = inicio_ano, y = mean, group = var, color = var),
            size = 1) +
  # ggtitle('Media de lances no final do pregao') +
  scale_color_discrete(name = 'Tempo restante') +
  ylab('Numero de lances')

# Lineplot: zoom no grafico acima
lnplt_nlances3 <- lineplot_lances_anual +
  geom_line(data = df_lances_gather_summary %>%
              filter(var %in% c("num_lances_last1min", "num_lances_last30sec", "num_lances_last10sec")) %>%
              mutate(var = factor(var, levels = c('num_lances_last1min', 'num_lances_last30sec', 'num_lances_last10sec'),
                                  labels = c('1 min', '30 sec', '10 sec'))),
            mapping = aes(x = inicio_ano, y = mean, group = var, color = var),
            size = 1) +
  # ggtitle('Media de lances no final do pregao') +
  scale_color_manual(name = 'Tempo restante', values = c("#00BF7D", "#00B0F6", "#E76BF3")) +
  ylab('Numero de lances')


# Areaplot: evolucao da fracao de lances em cada etapa
areaplt_fraclances1 <- ggplot(data = df_lances_gather_summary %>%
                                filter(var %in% c('fraction_C', 'fraction_A', 'fraction_B')) %>%
                                mutate(ano = as.numeric(str_extract(inicio_ano, '\\d{4}')))) +
  geom_area(aes(x = ano, y = mean, fill = var)) +
  theme_minimal() +
  scale_fill_manual(labels = c('Fase A ', 'Fase B ', 'Fase C '), values = c('steelblue2', 'steelblue3', 'steelblue4')) +
  xlab('Ano') +
  ylab('Participação') +
  scale_x_continuous(breaks = 2008:2017, labels = as.character(2008:2017)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = c("0,00", "0,25", "0,50", "0,75", "1,00")) +
  # ggtitle('Participação de cada fase no total de lances registrados', subtitle = 'Médias anuais') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title = element_text(size = 19),
        legend.text = element_text(size = 17),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom')

# Lineplot: evolucao da fracao media de lances registrados nos minutos/segundos finais
lnplt_frac1 <- lineplot_lances_anual +
  geom_line(data = df_lances_gather_summary %>%
              filter(str_detect(var, 'fraction_last')) %>%
              mutate(var = factor(var, levels = c('fraction_last10min', 'fraction_last5min', 'fraction_last1min', 'fraction_last30sec', 'fraction_last10sec'),
                                  labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec'))),
            mapping = aes(x = inicio_ano, y = mean, group = var, color = var),
            size = 1) +
  # ggtitle('Fracao de lances registrados no final do pregao') +
  ylab('Fracao de lances (média anual)') +
  scale_color_discrete(name = 'Tempo restante')

# Lineplot: zooom no grafico acima
lnplt_frac2 <- lineplot_lances_anual +
  geom_line(data = df_lances_gather_summary %>%
              filter(var %in% c('fraction_last1min', 'fraction_last30sec', 'fraction_last10sec')) %>%
              mutate(var = factor(var, levels = c('fraction_last1min', 'fraction_last30sec', 'fraction_last10sec'),
                                  labels = c('1 min', '30 sec', '10 sec'))),
            mapping = aes(x = inicio_ano, y = mean, group = var, color = var),
            size = 1) +
  # ggtitle('Fracao de lances registrados no final do pregao') +
  ylab('Fracao de lances (média anual)') +
  scale_color_manual(name = 'Tempo restante', values = c("#00BF7D", "#00B0F6", "#E76BF3"))


### Graficos TRIMESTRAIS

# Lineplot: evolucao do numero medio de lances por fase
lnplt_nlances1_trimestral <- df_lances_gather_summary_trimestral %>%
  filter(var %in% c('num_lances_A', 'num_lances_B', 'num_lances_C')) %>%
  ggplot(mapping = aes(x = inicio_trimestre, y = mean, group = var, color = var)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = 'Fase', labels = c('A', 'B', 'C')) +
  labs(y = 'Numero de lances', x = 'Tempo') +
  # ggtitle('Número de lances por fase', subtitle = 'Médias trimestrais') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')

# Lineplot: evolucao do numero medio de lances nos minutos/segundos finais
lnplt_nlances2_trimestral <- df_lances_gather_summary_trimestral %>%
  filter(str_detect(var, 'num_lances_last')) %>%
  mutate(var = factor(var, levels = c('num_lances_last10min', 'num_lances_last5min', 'num_lances_last1min', 'num_lances_last30sec', 'num_lances_last10sec'),
                      labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec'))) %>%
  ggplot(mapping = aes(x = inicio_trimestre, y = mean, group = var, color = var)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = 'Tempo restante') +
  labs(y = 'Numero de lances', x = 'Tempo') +
  # ggtitle('Número de lances ao final do pregão', subtitle = 'Médias trimestrais') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')

# Lineplot: zoom no grafico acima
lnplt_nlances3_trimestral <- df_lances_gather_summary_trimestral %>%
  filter(var %in% c('num_lances_last1min', 'num_lances_last30sec','num_lances_last10sec')) %>%
  mutate(var = factor(var, levels = c('num_lances_last10min', 'num_lances_last5min', 'num_lances_last1min', 'num_lances_last30sec', 'num_lances_last10sec'),
                      labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec'))) %>%
  mutate(var = droplevels(var)) %>%
  ggplot(mapping = aes(x = inicio_trimestre, y = mean, group = var, color = var)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(name = 'Tempo restante', values = c('#00BF7D', '#00B0F6', '#E76BF3')) +
  labs(y = 'Numero de lances', x = 'Tempo') +
  # ggtitle('Número de lances ao final do pregão - Pequenos intervalos', subtitle = 'Médias trimestrais') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')

# Lineplot: evolucao da fracao media de lances registrados nos minutos/segundos finais
lnplt_frac1_trimestral <- df_lances_gather_summary_trimestral %>%
  filter(str_detect(var, 'fraction_last')) %>%
  mutate(var = factor(var, levels = c('fraction_last10min', 'fraction_last5min', 'fraction_last1min', 'fraction_last30sec', 'fraction_last10sec'),
                      labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec'))) %>%

  ggplot(mapping = aes(x = inicio_trimestre, y = mean, group = var, color = var)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = 'Tempo restante') +
  labs(y = 'Fração do total lances', x = 'Tempo') +
  # ggtitle('Fração de lances registrados ao final do pregão', subtitle = 'Médias trimestrais') +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')

# Lineplot: zoom no grafico acima
lnplt_frac2_trimestral <- df_lances_gather_summary_trimestral %>%
  filter(var %in% c('fraction_last1min', 'fraction_last30sec', 'fraction_last10sec')) %>%
  mutate(var = factor(var, levels = c('fraction_last10min', 'fraction_last5min', 'fraction_last1min', 'fraction_last30sec', 'fraction_last10sec'),
                      labels = c('10 min', '5 min', '1 min', '30 sec', '10 sec')) %>% droplevels()) %>%
  ggplot(mapping = aes(x = inicio_trimestre, y = mean, group = var, color = var)) +
  geom_line(size = 1) +
  labs(y = 'Fração do total lances', x = 'Tempo') +
  scale_color_manual(name = 'Tempo restante', values = c('#00BF7D', '#00B0F6', '#E76BF3')) +
  # ggtitle('Fração de lances registrados ao final do pregão', subtitle = 'Médias trimestrais') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-14'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')

# Nota-se:
## -- Reducao do numero de lances registrados na fase C ate 2012/2013, com queda brusca de 2010 para 2011 (mudanca regra encerramento)
## -- Essa queda pode estar associada a mudanca de regra de encerramento (quasi-hard para random), que implicou diminuicao da duracao media da fase C
lnplt_nlances1
lnplt_nlances1_trimestral
areaplt_fraclances1
## -- Relacao inversa entre duracao das fases e fracao de lances
gridExtra::grid.arrange(areaplt_frac_duration, areaplt_fraclances1)
## -- Mudanca no numero de lances nos momentos finais reflete a mudanca no numero de lances da fase C
gridExtra::grid.arrange(lnplt_nlances2, lnplt_nlances3, ncol = 1)
gridExtra::grid.arrange(lnplt_nlances2_trimestral, lnplt_nlances3_trimestral, ncol = 1)
gridExtra::grid.arrange(lnplt_nlances2_trimestral, lnplt_nlances1_trimestral, ncol = 1)
## -- Entre 2008 e 2011: aumento na fracao de lances nos min/seg finais: robos ou regra de encerramento?
gridExtra::grid.arrange(lnplt_frac1, lnplt_frac2, ncol = 1)
gridExtra::grid.arrange(lnplt_frac1_trimestral, lnplt_frac2_trimestral, ncol = 1)

# Criando dummies de periodos relevantes
df_atas <- df_atas %>%
  mutate(periodos2 = if_else(ano %in% as.character(2008:2011), '2008-2011', '2012-2017'),
         periodos3 = case_when(ano %in% as.character(2008:2011) ~ '2008-2011',
                               ano %in% c('2012', '2013') ~ '2012-2013',
                               ano %in% as.character(2014:2017) ~ '2014-2017'))

# Tabelas com medias em periodos relevantes: 2 periodos
## - Aumento na fracao de lances no final do pregao no segundo periodo
## - Mas sabemos que houve grande crescimento no primeiro periodo, seguido de certa estabilidade
## - Diminuicao de fraction_C e aumento de fraction_AB
df_atas %>%
  select(periodos2, starts_with("num_lances"), starts_with("fraction_")) %>%
  filter(num_lances_total > 0) %>%
  gather(-periodos2, key = var, value = value) %>%
  group_by(periodos2, var) %>%
  summarise(mean = mean(value)) %>%
  ungroup() %>%
  spread(key = periodos2, value = mean)

# Tabelas com medias em periodos relevantes: 3 periodos
df_atas %>%
  select(periodos3, starts_with("num_lances"), starts_with("fraction_")) %>%
  filter(num_lances_total > 0) %>%
  gather(-periodos3, key = var, value = value) %>%
  group_by(periodos3, var) %>%
  summarise(mean = mean(value)) %>%
  ungroup() %>%
  spread(key = periodos3, value = mean)

# saveRDS(df_atas, 'Comprasnet/cnet_cafe_04_v2.rds')

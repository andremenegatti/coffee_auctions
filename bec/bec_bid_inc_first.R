# source("07_1_bid increments.R")

# Histogramas sugerem grande concentracao de incrementos em valores proximos de zero
ggplot(BEC_bid_inc_unnested, aes(x = -norm_inc_first_corr)) +
  geom_histogram(fill = 'steelblue', alpha = 0.8, bins = 40) +
  labs(x = "Incremento negativo normalizado", y = "Número de obseravações")
ggplot(BEC_bid_inc_unnested, aes(x = -norm_inc_first_corr)) +
  geom_histogram(fill = 'steelblue', alpha = 0.8, bins = 50) +
  xlim(c(0, 0.1)) +
  labs(x = "Incremento negativo normalizado", y = "Número de obseravações")


# # Removendo outliers (5% mais baixo de cada ano) e incrementos positivos
BEC_bid_inc_unnested_clean_first <- BEC_bid_inc_unnested %>%
  # Garantindo que temos apenas incrementos negativo
  filter(norm_inc_first_corr < 0) %>% 
  # Trimming (5% de cada ano)
  trim_df_grouped(vars = "norm_inc_first_corr", group = 'inicio_ano', tail = "lower") %>%
  # Nova variavel
  mutate(normalized_negative_increment = -norm_inc_first_corr)

# Histograma ficou mais razoavel
ggplot(BEC_bid_inc_unnested_clean_first, aes(x = normalized_negative_increment)) +
  geom_histogram(fill = 'steelblue', alpha = 0.8, bins = 40) +
  labs(x = "Incremento negativo normalizado", y = "Número de obseravações")

# Calculando incremento medio na amostra limpa de outliers
## - Valores proximos aos reportados por Szerman
BEC_bid_inc_unnested_clean_first %>% 
  select(normalized_negative_increment) %>% 
  summary()

BEC_bid_inc_unnested_clean_first$normalized_negative_increment %>% sd()

## Apos mudancas nas regras de encerramento
BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2011-03-01') %>% 
  select(normalized_negative_increment) %>% summary()

(BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2011-03-01') %>% 
    select(normalized_negative_increment))$normalized_negative_increment %>% sd

## Apos regra dos 20s
BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2012-01-17') %>% 
  select(normalized_negative_increment) %>% summary()

(BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2012-01-17') %>% 
    select(normalized_negative_increment))$normalized_negative_increment %>% sd

## Apos regra dos 3s
BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2014-01-02') %>% 
  select(normalized_negative_increment) %>% summary()

(BEC_bid_inc_unnested_clean_first %>% filter(data_abertura >= '2012-01-02') %>% 
    select(normalized_negative_increment))$normalized_negative_increment %>% sd  



#### ESTATISTICAS ANUAIS ####

bid_inc_first_summary_yearly <- BEC_bid_inc_unnested_clean_first %>% 
  filter(!is.na(normalized_negative_increment)) %>% 
  group_by(inicio_ano) %>% 
  summarise(mean = mean(normalized_negative_increment),
            sd = sd(normalized_negative_increment),
            percentile25 = quantile(normalized_negative_increment, 0.25),
            median = median(normalized_negative_increment),
            percentile75 = quantile(normalized_negative_increment, 0.75),
            n = n())

bid_inc_first_summary_yearly_gathered <- bid_inc_first_summary_yearly %>% 
  gather(-inicio_ano, -n, -sd, key = statistic, value = valor)


#### GRAFICOS ANUAIS #### 

lnplot_norm_inc_median <- ggplot(data = bid_inc_first_summary_yearly,
                                 mapping = aes(x = inicio_ano, group = 1, y = median)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: mediana') +
  xlab('Ano') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))


lnplot_norm_inc_mean <- ggplot(data = bid_inc_first_summary_yearly,
                               mapping = aes(x = inicio_ano, group = 1, y = mean)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: media') +
  xlab('Ano') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))

gridExtra::grid.arrange(lnplot_norm_inc_mean, lnplot_norm_inc_median)

ggplot(data = bid_inc_first_summary_yearly_gathered,
       mapping = aes(x = inicio_ano, group = statistic, color = statistic, y = valor)) + 
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
  labs(x = "Ano", y = "Incremento negativo normalizado") +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))


#### ESTATISTICAS TRIMESTRAIS ####

bid_inc_first_summary_trimestral <- BEC_bid_inc_unnested_clean_first %>% 
  filter(!is.na(normalized_negative_increment)) %>% 
  group_by(inicio_trimestre) %>% 
  summarise(mean = mean(normalized_negative_increment),
            sd = sd(normalized_negative_increment),
            percentile25 = quantile(normalized_negative_increment, 0.25),
            median = median(normalized_negative_increment),
            percentile75 = quantile(normalized_negative_increment, 0.75),
            n = n())

bid_inc_first_summary_trimestral_gathered <- bid_inc_first_summary_trimestral %>% 
  gather(-inicio_trimestre, -n, -sd, key = statistic, value = valor)


#### GRAFICOS TRIMESTRAIS ####

lnplot_norm_inc_median_trimestral <- ggplot(data = bid_inc_first_summary_trimestral,
                                            mapping = aes(x = inicio_trimestre, group = 1, y = median)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: mediana') +
  xlab('Tempo') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'))


lnplot_norm_inc_mean_trimestral <- ggplot(data = bid_inc_first_summary_trimestral,
                                          mapping = aes(x = inicio_trimestre, group = 1, y = mean)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: media') +
  xlab('Tempo') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'))

gridExtra::grid.arrange(lnplot_norm_inc_mean_trimestral, lnplot_norm_inc_median_trimestral)

ggplot(data = bid_inc_first_summary_trimestral_gathered,
       mapping = aes(x = inicio_trimestre, group = statistic, color = statistic, y = valor)) + 
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  # scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
  labs(x = "Tempo", y = "Incremento negativo normalizado") +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1')) +
  facet_wrap(~statistic, scales = 'free_y')


# make_lineplot(df = BEC_bid_inc_unnested_clean_first, var = normalized_negative_increment, time_var = inicio_trimestre)


#### ESTATISTICAS SEMESTRAIS ####

bid_inc_first_summary_semestral <- BEC_bid_inc_unnested_clean_first %>% 
  filter(!is.na(normalized_negative_increment)) %>% 
  group_by(inicio_semestre) %>% 
  summarise(mean = mean(normalized_negative_increment),
            sd = sd(normalized_negative_increment),
            percentile25 = quantile(normalized_negative_increment, 0.25),
            median = median(normalized_negative_increment),
            percentile75 = quantile(normalized_negative_increment, 0.75),
            n = n())

bid_inc_first_summary_semestral_gathered <- bid_inc_first_summary_semestral %>% 
  rename(Média = mean,
         Mediana = median,
         "Primeiro Quartil" = percentile25,
         "Terceiro Quartil" = percentile75) %>% 
  gather(-inicio_semestre, -n, -sd, key = statistic, value = valor)


#### GRAFICOS SEMESTRAIS ####

lnplot_norm_inc_mean_semestral <- ggplot(data = bid_inc_first_summary_semestral,
                                         mapping = aes(x = inicio_semestre, group = 1, y = mean)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: media') +
  xlab('Tempo (semestres)') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - S1'))


lnplot_norm_inc_median_semestral <- ggplot(data = bid_inc_first_summary_semestral,
                                           mapping = aes(x = inicio_semestre, group = 1, y = median)) + 
  geom_line(size = 1, color = 'steelblue') +
  # ggtitle('Evolucao do incremento normalizado: mediana') +
  xlab('Tempo (semestres)') +
  ylab('Incremento negativo normalizado') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - S1'))

gridExtra::grid.arrange(lnplot_norm_inc_mean_semestral, lnplot_norm_inc_median_semestral)

ggplot(data = bid_inc_first_summary_semestral_gathered,
       mapping = aes(x = inicio_semestre, group = statistic, y = valor)) + 
  geom_line(size = 1, color = 'steelblue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  # scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
  labs(x = "Tempo (semestres)", y = "Incremento negativo normalizado") +
  geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
  geom_vline(xintercept = as.Date(c('2010-09-01', '2011-02-01')), color = 'darkgreen', linetype = 'dashed') +
  geom_vline(xintercept = as.Date('2010-12-01'), color = 'darkgreen', linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - S1')) +
  facet_wrap(~statistic, scales = 'free_y')


# #### ESTATISTICAS ANUAIS ####
# 
# bid_inc_summary_yearly <- BEC_bid_inc_unnested_clean_first %>% 
#   filter(!is.na(normalized_negative_increment)) %>% 
#   group_by(inicio_ano) %>% 
#   summarise(mean = mean(normalized_negative_increment),
#             sd = sd(normalized_negative_increment),
#             percentile25 = quantile(normalized_negative_increment, 0.25),
#             median = median(normalized_negative_increment),
#             percentile75 = quantile(normalized_negative_increment, 0.75),
#             n = n())
# 
# bid_inc_summary_yearly_gathered <- bid_inc_summary_yearly %>% 
#   gather(-inicio_ano, -n, -sd, key = statistic, value = valor)
# 
# 
# #### GRAFICOS ANUAIS #### 
# 
# lnplot_norm_inc_median <- ggplot(data = bid_inc_summary_yearly,
#                                  mapping = aes(x = inicio_ano, group = 1, y = -median)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: mediana') +
#   xlab('Ano') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))
# 
# 
# lnplot_norm_inc_mean <- ggplot(data = bid_inc_summary_yearly,
#                                mapping = aes(x = inicio_ano, group = 1, y = -mean)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: media') +
#   xlab('Ano') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred')
# 
# gridExtra::grid.arrange(lnplot_norm_inc_mean, lnplot_norm_inc_median)
# 
# ggplot(data = bid_inc_summary_yearly_gathered,
#        mapping = aes(x = inicio_ano, group = statistic, color = statistic, y = -valor)) + 
#   geom_line(size = 1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
#   labs(x = "Ano", y = "Incremento negativo normalizado") +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))
# 
# 
# #### ESTATISTICAS TRIMESTRAIS ####
# 
# bid_inc_summary_trimestral <- BEC_bid_inc_unnested_clean_first %>% 
#   filter(!is.na(normalized_negative_increment)) %>% 
#   group_by(inicio_trimestre) %>% 
#   summarise(mean = mean(normalized_negative_increment),
#             sd = sd(normalized_negative_increment),
#             percentile25 = quantile(normalized_negative_increment, 0.25),
#             median = median(normalized_negative_increment),
#             percentile75 = quantile(normalized_negative_increment, 0.75),
#             n = n())
# 
# bid_inc_summary_trimestral_gathered <- bid_inc_summary_trimestral %>% 
#   gather(-inicio_trimestre, -n, -sd, key = statistic, value = valor)
# 
# 
# #### GRAFICOS TRIMESTRAIS ####
# 
# lnplot_norm_inc_median_trimestral <- ggplot(data = bid_inc_summary_trimestral,
#                                           mapping = aes(x = inicio_trimestre, group = 1, y = -median)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: mediana') +
#   xlab('Tempo') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'))
# 
# 
# lnplot_norm_inc_mean_trimestral <- ggplot(data = bid_inc_summary_trimestral,
#                                             mapping = aes(x = inicio_trimestre, group = 1, y = -mean)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: media') +
#   xlab('Tempo') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'))
# 
# 
# gridExtra::grid.arrange(lnplot_norm_inc_mean_trimestral, lnplot_norm_inc_median_trimestral)
# 
# ggplot(data = bid_inc_summary_trimestral_gathered,
#        mapping = aes(x = inicio_trimestre, group = statistic, color = statistic, y = -valor)) + 
#   geom_line(size = 1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
#   labs(x = "Tempo", y = "Incremento negativo normalizado") +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))
# 
# 
# make_lineplot(df = BEC_bid_inc_unnested_clean_first, var = normalized_negative_increment, time_var = inicio_trimestre)
# 
# 
# #### ESTATISTICAS SEMESTRAIS ####
# 
# bid_inc_summary_semestral <- BEC_bid_inc_unnested_clean_first %>% 
#   filter(!is.na(normalized_negative_increment)) %>% 
#   group_by(inicio_semestre) %>% 
#   summarise(mean = mean(normalized_negative_increment),
#             sd = sd(normalized_negative_increment),
#             percentile25 = quantile(normalized_negative_increment, 0.25),
#             median = median(normalized_negative_increment),
#             percentile75 = quantile(normalized_negative_increment, 0.75),
#             n = n())
# 
# bid_inc_summary_semestral_gathered <- bid_inc_summary_semestral %>% 
#   gather(-inicio_semestre, -n, -sd, key = statistic, value = valor)
# 
# 
# #### GRAFICOS SEMESTRAIS ####
# 
# lnplot_norm_inc_mean_semestral <- ggplot(data = bid_inc_summary_semestral,
#                                         mapping = aes(x = inicio_semestre, group = 1, y = -mean)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: media') +
#   xlab('Tempo (semestres)') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - S1'))
# 
# 
# lnplot_norm_inc_median_semestral <- ggplot(data = bid_inc_summary_semestral,
#                                            mapping = aes(x = inicio_semestre, group = 1, y = -median)) + 
#   geom_line(size = 1, color = 'steelblue') +
#   # ggtitle('Evolucao do incremento normalizado: mediana') +
#   xlab('Tempo (semestres)') +
#   ylab('Incremento negativo normalizado') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - S1'))
# 
# gridExtra::grid.arrange(lnplot_norm_inc_mean_semestral, lnplot_norm_inc_median_semestral)
# 
# ggplot(data = bid_inc_summary_semestral_gathered %>% filter(statistic != 'percentile75'),
#        mapping = aes(x = inicio_semestre, group = statistic, color = statistic, y = -valor)) + 
#   geom_line(size = 1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   geom_vline(xintercept = c(data_20s, data_3s), color = 'darkred') +
#   scale_color_discrete(name = "Estatística", labels = c("Média", "Mediana", "Primeiro quartil", "Terceiro quartil")) +
#   labs(x = "Tempo", y = "Incremento negativo normalizado") +
#   scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, " - S1"))





#### O QUE ACONTECEU EM 2015??? ####
# 
# df_2015 <- BEC_bid_inc_unnested_clean_first %>% 
#   filter(ano == '2015')
# 
# df_2015 %>% 
#   group_by(inicio_mes) %>% 
#   summarise(mean = mean(normalized_negative_increment)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = inicio_mes, y = mean)) +
#   geom_point(alpha = 0.5)
# 
# df_low_increments <- df_2015 %>% 
#   filter(normalized_negative_increment < quantile(normalized_negative_increment, 0.25))
# 
# df_low_increment_2015 <- BEC_bid_inc_2015 %>% 
#   filter(id_item %in% df_low_increments$id_item)
# 
# df_2015 %>% filter(inicio_mes == as.Date('2015-07-01')) %>% View()
# 
# BEC_bid_inc_unnested_clean_first %>% 
#   filter(ano %in%  c('2014', '2015', '2016')) %>% 
#   ggplot(aes(x = as.factor(inicio_mes), y = normalized_negative_increment)) +
#   geom_boxplot()
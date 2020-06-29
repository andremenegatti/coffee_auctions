library(tidyverse)

df_cnet_original <- readRDS('data/cnet_cafe_dd.rds')

df_lances_completo <- readRDS('data/cnet_lances.rds')

df_lances_nested <- df_lances_completo %>%
  select(id_item, data_hora, valor_lance:incremento_proprio_norm) %>%
  nest(bid_data = data_hora:incremento_proprio_norm)

df_cnet <- df_cnet_original %>%
  select(id_item, unidade_compradora, sigla_uf, municipio,
         abertura_lances, iminencia_encerramento,
         inicio_fase_aleatoria, encerramento_lances,
         regime_juridico, lances_clean, propostas,
         kg_por_unid, quantidade,
         win_bid = win_bid_kg, reserve = reserve_kg,
         inicio_mes, inicio_bimestre, inicio_trimestre, inicio_semestre,
         avg_bids_per_bidder, median_bids_per_bidder, num_forn_lances,
         margem_preferencia, tratamento_diferenciado,
         futuro_defl, arab_defl, qualidade,
         marca_vencedor_principais, marca_vencedor)

df_cnet <- df_cnet %>%
  mutate(n_lances = map_dbl(.x = lances_clean,
                        .f = possibly(.f = function(x) nrow(x), otherwise = 0L))) %>%
  mutate(valor_contrato = kg_por_unid * quantidade * win_bid)

df_cnet <- df_cnet %>%
  left_join(df_lances_nested, by = 'id_item')

df_cnet2 <- df_cnet %>%
  filter(!map_lgl(bid_data, is.null)) %>%
  mutate(bid_data = map(.x = bid_data, .f = ~ arrange(.x, data_hora))) %>%
  mutate(hora_ultimo_lance = map_dbl(.x = bid_data,
                                     .f = ~ last(.x$data_hora) %>%
                                       as.numeric())) %>%
  mutate(int_ult_lance_fim =
           as.numeric(encerramento_lances) - hora_ultimo_lance) %>%
  mutate(last_1min = ifelse(int_ult_lance_fim <= 60, 1, 0)) %>%
  mutate(last_20sec = ifelse(int_ult_lance_fim <= 20, 1, 0))

df_cnet2 %>%
  group_by(last_20sec) %>%
  summarise(median_n_lances = median(n_lances, na.rm = TRUE),
            median_n_forn = median(num_forn_lances, na.rm = TRUE))


df_cnet_ate2015 <- df_cnet2 %>%
  filter(abertura_lances < '2016-01-01') %>%
  split(f = .$regime_juridico) %>%
  map(.f = ~ mutate(.x, ecd = ecdf(int_ult_lance_fim)(int_ult_lance_fim))) %>%
  bind_rows()

formatar_numero <- partial(formatC, digits = 2,
                           decimal.mark = ',', big.mark = '.')

# ECDF PLOT -------------------------------------------------------------------
ecdf_plot <- df_cnet_ate2015 %>% 
  mutate(regime_juridico = str_replace(regime_juridico, 'mini', 'míni') %>% 
           fct_relevel('Sem intervalo mínimo', 'Regra 20s', 'Regra 20s + Regra 3s')) %>% 
  ggplot() +
  geom_line(aes(x =  int_ult_lance_fim, y = ecd,
                linetype = regime_juridico, group = regime_juridico),
            size = 1, alpha = 0.7) +
  scale_linetype_manual(name = 'Regime Jurídico',
                     values = c('solid', 'dashed', 'dotted')) +
  scale_y_continuous(labels = formatar_numero) +
  coord_cartesian(xlim = c(0, 300)) +
  labs(
    x = 'Segundos entre o último lance e o fim do pregão',
    y = 'Probabilidade acumulada',
    title = 'Efeitos das regras de intervalo mínimo sobre lances finais',
    subtitle = 'ECDF do tempo entre o último lance e o encerramento do pregão',
    caption = '
    Nota: Para melhor visualização, o eixo horizontal é limitado a 5 minutos.'
  ) +
  theme(legend.text = element_text(size = 10)) ; ecdf_plot

ggsave(plot = ecdf_plot,
       filename = '~/Documents/dissertacao/images/ecdf_int_ult_lance_fim.png',
       width = 6.5, height = 7)


# Checking specific values
ecdfs = df_cnet2 %>%
  filter(abertura_lances < '2016-01-01') %>%
  split(f = .$regime_juridico) %>% 
  map(.f = ~ ecdf(.x$int_ult_lance_fim))

ecdfs$`Sem intervalo minimo`(75)
ecdfs$`Regra 20s`(90)
ecdfs$`Regra 20s + Regra 3s`(110)

# DENSITY PLOT ----------------------------------------------------------------
epdf_plot <- ggplot(df_cnet_ate2015) +
  stat_density(aes(x =  int_ult_lance_fim, col = regime_juridico,),
            size = 1, alpha = 0.7, position = 'identity', geom = 'line') +
  scale_color_manual(name = 'Regime Jurídico',
                     values = c('black', 'blue', 'red')) +
  scale_y_continuous(labels = formatar_numero) +
  coord_cartesian(xlim = c(0, 300)) +
  labs(
    x = 'Segundos entre o último lance e o encerramento do pregão',
    y = 'Densidade de probabilidade',
    title = 'Efeitos das regras de intervalo mínimo sobre lances finais',
    subtitle = 'EPDF do tempo entre o último lance e o encerramento do pregão',
    caption = '
    Nota: Para melhor visualização, o eixo horizontal é limitado a 5 minutos.'
  ) +
  theme(legend.text = element_text(size = 10))

# ggsave(plot = epdf_plot, filename = 'plots/epdf_int_ult_lance_fim.png', width = 6.5, height = 7)

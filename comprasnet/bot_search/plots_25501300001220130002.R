library(tidyverse)

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < '2016-01-01')

auction_to_check <- '25501300001220130002'

cnpjs_to_filter <- df_lances %>% filter(id_item == auction_to_check) %>% 
  count(CNPJ_CPF) %>% arrange(desc(n)) %>% select(CNPJ_CPF) %>% 
  slice(1:2) %>% unlist()

data_leilao <- df_lances %>% filter(id_item == auction_to_check) %>% 
  distinct(abertura_lances) %>% 
  mutate(ano = lubridate::year(abertura_lances),
         mes = lubridate::month(abertura_lances) %>% 
           str_pad(width = 2, side = 'left', pad = '0'),
         dia = lubridate::day(abertura_lances) %>% 
           str_pad(width = 2, side = 'left', pad = '0'),
         data = str_c(ano, mes, dia, sep = '-')) %>% 
  select(data) %>% unlist()

subtitle_plot <- str_c('Fase aleatória do leilão ', auction_to_check,
                       ', realizado em ', data_leilao)

# Data wrangling --------------------------------------------------------------
df_bids <- df_lances %>% 
  filter(id_item == auction_to_check,
         data_hora > abertura_lances,
         CNPJ_CPF %in% cnpjs_to_filter)

df_intervalos <- df_bids %>% 
  select(CNPJ_CPF, incremento_menor_bruto, data_hora, intervalo_anterior,
         intervalo_menor, intervalo_proprio) %>% 
  pivot_longer(cols = contains('intervalo_'), names_prefix = 'intervalo_',
               names_to = 'tipo_intervalo', values_to = 'segundos') %>% 
  mutate(tipo_intervalo = 
           case_when(tipo_intervalo == 'menor' ~ 'Menor Lance',
                     tipo_intervalo == 'anterior' ~ 'Lance Anterior',
                     TRUE ~ 'Lance Próprio') %>% 
           fct_relevel('Lance Anterior', 'Menor Lance', 'Lance Próprio')
  )

# Evolucao intervalos ---------------------------------------------------------
plot_int <- df_intervalos %>% 
  filter(incremento_menor_bruto < 0) %>% # <<<< 
  filter(tipo_intervalo != 'Lance Anterior') %>% 
  filter(data_hora > lubridate::ymd_hms('2013-06-12 10:49:00')) %>%
  # filter(segundos < 40) %>% # <<<<
  ggplot(aes(x = data_hora, y = segundos,
             col = CNPJ_CPF, shape = CNPJ_CPF)) + 
  geom_point(alpha = 0.8) +
  geom_hline(yintercept = c(3, 20), linetype = 'dotted') +
  scale_y_continuous(breaks = c(3, seq(10, 40, by = 10))) +
  scale_shape_manual(values = c(16, 4), name = 'CNPJ') +
  scale_color_manual(values = c('red', 'blue'), name = 'CNPJ') +
  guides(shape = FALSE, col = FALSE) +
  labs(
    x = 'Hora',
    y = 'Segundos transcorridos desde o último lance',
    title = 'Evolução de intervalos entre lances ao longo de um pregão',
    subtitle = subtitle_plot,
    caption =
      'Notas:
    i) Apenas lances registrados na fase aleatória pelos dois participantes mais ativos;
    ii) Para melhor visualização, são exibidos apenas lances registrados após às 10:50.'
  ) +
  theme(legend.position = 'bottom') +
  facet_grid(tipo_intervalo ~ CNPJ_CPF, scales = 'free') ; plot_int

ggsave(plot = plot_int, height = 7, width = 10,
       filename = str_c(auction_to_check, '_evolucao_intervalos.png'))
# plotly::ggplotly(plot_int)

# Evolucao  Descontos ---------------------------------------------------------
plot_desc <- df_bids %>% 
  filter(incremento_menor_bruto < 0) %>% # <<<<
  filter(data_hora > lubridate::ymd_hms('2013-06-12 10:49:00')) %>%
  ggplot(aes(x = data_hora, y = incremento_menor_bruto,
             col = CNPJ_CPF, shape = CNPJ_CPF)) + 
  geom_point(alpha = 0.9) +
  scale_shape_manual(values = c(16, 4), name = 'CNPJ') +
  scale_color_manual(values = c('red', 'blue'), name = 'CNPJ') +
  scale_y_continuous(breaks = c(-0.01, -(1:5)), labels = 
                       function(x) formatC(x, digits = 3, big.mark = '.',
                                           decimal.mark = ',')) +
  labs(
    x = 'Hora',
    y = 'Diferença de preço entre os lances em reais',
    title = 'Evolução de descontos ao longo de um pregão',
    subtitle = subtitle_plot,
    caption = 'Notas: 
    i) Apenas lances de menor valor submetidos na fase aleatória pelos dois participantes mais ativos;
    ii) Para melhor visualização, são exibidos apenas lances registrados após às 10:50.'
  )  +
  theme(legend.position = 'bottom') ; plot_desc

ggsave(plot = plot_desc, height = 6, width = 7,
       filename = str_c(auction_to_check, '_evolucao_descontos.png'))
# plotly::ggplotly(plot_desc)

# Lances -----------------------------------------------------------------
plot_bids <- df_bids %>% 
  filter(data_hora > lubridate::ymd_hms('2013-06-12 10:49:00')) %>%
  ggplot() +
  geom_point(aes(x = data_hora, y = valor_lance / 1000,
                 shape = CNPJ_CPF, col = CNPJ_CPF),
             alpha = 0.8) +
  scale_shape_manual(values = c(16, 4), name = 'CNPJ') +
  scale_color_manual(values = c('red', 'blue'), name = 'CNPJ') +
  scale_y_continuous(labels = 
                       function(x) formatC(x, digits = 3, big.mark = '.',
                                           decimal.mark = ',')) +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Hora',
    y = 'Valor do lance (milhares de reais)',
    title = 'Exemplo de leilão com grande número de lances',
    subtitle = subtitle_plot,
    caption =  'Nota:
    i) Dados de lances de menor valor submetidos na fase aleatória pelos dois participantes mais ativos;
    ii) Para melhor visualização, são exibidos apenas lances registrados após às 10:50;
    iii) Nesse leilão, os lances correspondem ao valor total do contrato.'
  ) ; plot_bids

ggsave(plot = plot_bids, height = 6, width = 7,
       filename = str_c(auction_to_check, '_evolucao_lances.png'))
# plotly::ggplotly(plot_bids)
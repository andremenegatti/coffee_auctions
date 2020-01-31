library(PregoesBR)

df_bid_inc_unnested <- readRDS("comprasnet/cnet_df_bid_inc_unnested.rds")

# Grande concentracao de incrementos em valores proximos de zero
ggplot(df_bid_inc_unnested, aes(x = -norm_inc_first)) +
  geom_histogram(fill = 'gray', color = 'white', size = 0.1,
                 alpha = 0.8, binwidth = 0.0005) +
  xlim(c(0, 0.1)) +
  labs(x = "Incremento negativo normalizado", y = "Número de obseravações")

# Removendo outliers (5% mais baixo de cada ano) e incrementos positivos
df_inc <- df_bid_inc_unnested %>%
  # Garantindo que temos apenas incrementos negativos
  filter(norm_inc_first < 0) %>%
  # Trimming (5% de cada ano)
  trim_df_grouped(vars = "norm_inc_first",
                  group = 'inicio_ano', tail = "lower") %>%
  # Nova variavel
  mutate(norm_neg_inc = -norm_inc_first)

# ESTATÍSTICAS DESCRITIVAS ----------------------------------------------------
# Calculando incremento medio na amostra limpa de outliers
## - Valores proximos aos reportados por Szerman
df_inc %>%
  get_summary_stats(norm_neg_inc * 100)

# Por periodo de interesse
df_inc %>%
  get_summary_stats(norm_neg_inc * 100, regime_juridico_3s)

# HISTOGRAMA ------------------------------------------------------------------
histogram <- ggplot(df_inc, aes(x = norm_neg_inc * 100)) +
  geom_histogram(alpha = 0.8,
                 col = 'white',
                 fill = 'gray',
                 size = .1,
                 bins = 50) +
  labs(x = "Redução de preço (como % do primeiro lance)",
       y = "Número de obseravações",
       title = 'Distribuição dos incrementos entre lances',
       subtitle = 'Valores normalizados pelo primeiro lance do leilão',
       caption = 'Nota: foram considerados apenas lances que representaram redução de preço em relação ao melhor lance anteriormente registrado.') +
  facet_wrap(~ regime_juridico_3s)

histogram
# ggsave('plots/histograma_incremento_first_3s.png', width = 9, height = 7)

# DENSIDADE -------------------------------------------------------------------
density_plot <- ggplot(df_inc, aes(x = norm_neg_inc * 100)) +
  stat_density(col = 'black', geom = 'line', position = 'identity',
               aes(linetype = regime_juridico_3s)) +
  scale_x_continuous(labels =
                       function(x) str_replace(paste0(x, '%'), '\\.', ',')) +
  scale_linetype_discrete(name = 'Vigência Regra 3s') +
  labs(x = "Redução de preço como % do primeiro lance",
       y = "Densidade",
       title = 'Impacto da Regra dos 3s sobre os descontos entre lances',
       subtitle = 'Densidade de probabilidade dos descontos de lances de cobertura',
       caption = 'Notas:
       1. Foram considerados apenas lances que representaram redução de preço em relação ao melhor lance anteriormente registrado.
       2. Ambas as distribuições possuem uma longa cauda à direita, não totalmente exibida para melhor visualização.') +
  my_theme() +
  coord_cartesian(xlim = c(0, 1))

density_plot
# ggsave('plots/densidade_incremento_first_3s.png', width = 9, height = 7)

# ECDF ------------------------------------------------------------------------
df_ecdf <- df_inc %>%
  filter(abertura_lances < '2016-01-01') %>%
  split(f = .$regime_juridico_3s) %>%
  map(.f = ~ mutate(.x, cum_prob = ecdf(norm_neg_inc)(norm_neg_inc))) %>%
  bind_rows()

ecdf_plot <- ggplot(df_ecdf) +
  geom_line(aes(x = norm_neg_inc * 100, y = cum_prob,
                group = regime_juridico_3s, linetype = regime_juridico_3s)) +
  labs(
    x = 'Redução do preço como % do primeiro lance',
    y = 'Probabilidade acumulada',
    title = 'Impacto da Regra dos 3s sobre os descontos entre lances',
    subtitle = 'ECDF dos descontos dos lances de cobertura, por regime juridico'
    ) +
  scale_linetype_discrete(name = 'Vigência Regra 3s') +
  scale_y_continuous(labels = formatar_numero) +
  scale_x_continuous(labels = function(x) paste0(x, '%'))

ecdf_plot
# ggsave(filename = 'plots/ecdf_incremento_first_3s.png', width = 9, height = 7)

# SMOOTH ----------------------------------------------------------------------
smooth_plot <- ggplot(df_inc) +
  geom_smooth(aes(x = as.Date(abertura_lances), y = norm_neg_inc * 100),
              col = 'black') +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  geom_label(data = tibble(data = c(data_20s, data_3s),
                           texto = c('Regra 20s', 'Regra 3s')),
             aes(x = data, y = 0.5, label = texto), family = 'serif') +
  scale_y_continuous(labels = function(x) str_replace(paste0(x, '%'), '\\.', ',')) +
  scale_x_date(breaks = 'year', labels = 2011:2018) +
  coord_cartesian(xlim = c('2011-03-01', '2015-12-31') %>% as.Date() ) +
  labs(x = 'Data',
       y = 'Redução de preço como % do primeiro lance',
       title = 'Evolução do desconto dos lances de cobertura ao longo do tempo',
       subtitle = 'Regressão não-paramétrica (GAM)')

smooth_plot
# ggsave(filename = 'plots/smooth_incremento_first_3s.png', width = 9, height = 7)

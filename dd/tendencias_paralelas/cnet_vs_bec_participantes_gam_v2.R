library(PregoesBR)

BEC_cafe_full <- readRDS('BEC/BEC_cafe_etapa4.rds')
cnet_cafe_full <- readRDS('Comprasnet/cnet_cafe_01_v4.rds')

BEC_cafe <- BEC_cafe_full %>%
  # filter(abertura_lances >= as.Date('2011-03-01')) %>%
  # filter(abertura_lances < as.Date('2016-01-01')) %>%
  mutate(abertura_lances = as.Date(abertura_lances)) %>%
  mutate(sigla_uf = 'SP')

cnet_cafe <- cnet_cafe_full %>%
  # filter(abertura_lances >= as.Date('2011-03-01')) %>%
  # filter(abertura_lances < as.Date('2016-01-01')) %>%
  mutate(abertura_lances = as.Date(abertura_lances)) %>%
  mutate(win_bid = win_bid_kg)

# Filtrando Comprasnet: apenas SP
sp_cnet_cafe <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

# Removendo outliers (verificar se eh realmente necessario, a depender da variavel)
# cnet_cafe <- cnet_cafe %>%
#   trim_df('win_bid')
# sp_cnet_cafe <- sp_cnet_cafe %>%
#   trim_df('win_bid')
# BEC_cafe <- BEC_cafe %>%
#   trim_df('win_bid')

plot_list <- map(.x = list(cnet_cafe, sp_cnet_cafe),
                 .f = ~ .x %>%
                   select(abertura_lances, win_bid, num_forn_lances) %>%
                   mutate(Plataforma = "Comprasnet") %>%
                   bind_rows(BEC_cafe %>%
                               select(abertura_lances, win_bid, num_forn_lances) %>%
                               mutate(Plataforma = "BEC")) %>%
                   mutate(Plataforma = factor(Plataforma, levels = c("Comprasnet", "BEC")))
) %>%
  set_names(c('full', 'sp'))

all_three_lines <-
  bind_rows(
    plot_list$full %>% mutate(amostra = "Amostra completa"),
    plot_list$sp %>% mutate(amostra = "Apenas Estado de São Paulo")
  ) %>%
  mutate(
    regime_juridico = case_when(
      abertura_lances < data_20s ~ 1,
      abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
      abertura_lances >= data_3s ~ 3
    ),
    plataforma_X_regime = str_c(Plataforma, regime_juridico, sep = '_')
  )

all_three_lines %>%
  # filter(abertura_lances >= as.Date('2011-03-01')) %>%
  # filter(abertura_lances < as.Date('2016-01-01')) %>%
  ggplot(aes(x = abertura_lances, y = num_forn_lances, group = Plataforma, linetype = Plataforma)) +
  geom_smooth(method = 'loess',
              color = 'black') +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-09-01'))) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2012:2015, '-01-01')), labels = 2012:2015) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet",
       caption = "Nota: as linhas verticais indicam o início da vigência das regras dos 20s (esquerda) e dos 3s (direita), nos pregões do Comprasnet.") +
  facet_wrap(~ amostra, nrow = 2, scales = "free_y")
ggsave('cnet_vs_bec_participantes_loess_faceted.png', width = 8.5, height = 7)

# cut: corte nos dados, e nao em coord_cartesian
# split: group = plataforma_X_regime, e nao simplesmente Plataforma
# Loess vs. GAM

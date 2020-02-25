library(PregoesBR)

df_bid_inc_unnested <- 
  readRDS("data/cnet_negative_bid_increments_unnested.rds") %>% 
  left_join(readRDS('data/cnet_cafe.rds') %>% 
              select(id_item, inicio_fase_aleatoria),
            by = 'id_item') %>% 
  # Apenas lances de cobertura da fase aleatoria
  filter(data_hora >= inicio_fase_aleatoria,
         norm_inc_first < 0)

# Trimming (2.5% de cada ano)
df_inc <- df_bid_inc_unnested %>%
  create_time_variables() %>% 
  trim_df_grouped(vars = "norm_inc_first", perc = 2.5,
                  group = 'inicio_ano', tail = "both") 


for (i in c(10, 15, 20, 30, 40, 50, 75, 100)) {
  
  ranking_lances <- df_inc %>% 
    count(CNPJ_CPF, inicio_semestre) %>% 
    split(.$inicio_semestre) %>% 
    map(.f = ~ arrange(.x, desc(n)) %>%
          mutate(share_lances = n / sum(n),
                 share_acumulado = cumsum(share_lances)))
  
  df_inc <- df_inc %>% 
    split(.$inicio_semestre) %>% 
    map2(.y = map(.x = ranking_lances, .f = ~ slice(.x, 1:i)),
         .f = ~ .x %>% 
           mutate(mais_ativos = ifelse(CNPJ_CPF %in% .y$CNPJ_CPF,
                                       'Mais ativos', 'Outros'))) %>% 
    bind_rows()
  
  
  myplt <- df_inc %>% 
    filter(abertura_lances < '2016-01-01') %>% 
    ggplot() +
    geom_density(aes(x = intervalo_menor, fill = mais_ativos),
                 position = 'identity', alpha = 0.5) +
    scale_x_log10(breaks = c(0.1, 1, 3, 10, 20, 100, 1000, 10000),
                  labels = c('0,1', '1', '3', '10', '20', '100', '1000', '10 mil')) +
    geom_vline(xintercept = 3, col = 'darkred', linetype = 'dotted', alpha = 0.7) +
    coord_cartesian(xlim = c(1, 500)) +
    labs(x = 'Segundos desde o menor lance (escala logarítmica)',
         y = 'Número de lances') +
    facet_grid(inicio_semestre ~ ., scales = 'free')
  
  ggsave(plot = myplt,
         filename = str_c('waves_top', i, 'bidders.png'),
                          height = 14, width = 5)
  
}

# Densidade -------------------------------------------------------------------
library(ggridges)

density_plot <- df_inc %>% 
  filter(abertura_lances < '2016-01-01',
         intervalo_menor > 0) %>%
  ggplot(aes(x = intervalo_menor)) +
  geom_density_ridges(aes(y = inicio_ano, group = inicio_ano)) +
  scale_y_date(breaks = as.Date(str_c(2011:2015, '-01-01')),
               labels = 2011:2015) +
  scale_x_log10(breaks = c(1, 3, 10, 20, 100, 1000, 10000),
                labels = c('1', '3', '10', '20', '100', '1000', '10 mil')) +
  geom_vline(xintercept = 3, col = 'darkred', linetype = 'dotted', alpha = 0.7) +
  coord_cartesian(xlim = c(1, 1e+4)) +
  labs(x = 'Segundos desde o menor lance (escala logarítmica)',
       y = 'Ano'); density_plot


# SMOOTH ----------------------------------------------------------------------
# Continuous
smooth_plot <- df_inc %>% 
  filter(abertura_lances < '2015-01-01') %>% 
  filter(mais_ativos == 'Mais ativos') %>% 
  ggplot() +
  geom_smooth(aes(x = as.Date(abertura_lances), y = intervalo_menor),
              col = 'black') +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  scale_y_log10() ; smooth_plot
# ggsave(filename = 'plots/smooth_incremento_first_3s.png', width = 9, height = 7)

# Split
ggplot(df_inc) +
  geom_smooth(aes(x = as.Date(abertura_lances), y = norm_neg_inc * 100,
                  group = regime_juridico),
              col = 'black') +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  geom_label(data = tibble(data = c(data_20s, data_3s),
                           texto = c('Regra 20s', 'Regra 3s')),
             aes(x = data, y = 0.52, label = texto), family = 'serif') +
  scale_y_continuous(labels = function(x) str_replace(paste0(x, '%'), '\\.', ',')) +
  # scale_x_date(breaks = 'year', labels = 2011:2015) +
  coord_cartesian(xlim = c('2011-03-01', '2015-08-31') %>% as.Date() ) +
  labs(x = 'Data',
       y = 'Redução de preço como % do primeiro lance',
       title = 'Evolução do desconto dos lances de cobertura ao longo do tempo',
       subtitle = 'Regressão não-paramétrica (GAM)')
# ggsave(filename = 'plots/smooth_incremento_first_3s_gam_split.png', width = 9, height = 7)

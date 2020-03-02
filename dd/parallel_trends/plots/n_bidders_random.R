library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = readRDS) %>% set_names(c('dd_brasil', 'dd_sp'))

cnet_lances <- readRDS('data/cnet_lances.rds')
cnet_cafe <- readRDS('data/cnet_cafe.rds')

# Participantes na fase aleatória ---------------------------------------------
# SP
df_n_bidders_random_sp <- cnet_lances %>% 
  semi_join(dd_data_list$dd_sp, by = 'id_item') %>% 
  left_join(cnet_cafe %>% select(id_item, inicio_fase_aleatoria),
            by = 'id_item') %>% 
  filter(data_hora >= inicio_fase_aleatoria) %>% 
  group_by(id_item) %>% 
  summarise(n_bidders_random = length(unique(CNPJ_CPF)))

dd_data_list$dd_sp <- dd_data_list$dd_sp %>% 
  left_join(df_n_bidders_random_sp, by = 'id_item') %>% 
  mutate(num_forn_lances = ifelse(!is.na(n_bidders_random),
                                  n_bidders_random,
                                  num_forn_lances))

# Brasil
df_n_bidders_random_brasil <- cnet_lances %>% 
  semi_join(dd_data_list$dd_brasil, by = 'id_item') %>% 
  left_join(cnet_cafe %>% select(id_item, inicio_fase_aleatoria),
            by = 'id_item') %>% 
  filter(data_hora >= inicio_fase_aleatoria) %>% 
  group_by(id_item) %>% 
  summarise(n_bidders_random = length(unique(CNPJ_CPF)))

dd_data_list$dd_brasil <- dd_data_list$dd_brasil %>% 
  left_join(df_n_bidders_random_brasil, by = 'id_item') %>% 
  mutate(num_forn_lances = ifelse(!is.na(n_bidders_random),
                                  n_bidders_random,
                                  num_forn_lances))

# Preparando base para o gráfico ----------------------------------------------
df_plot <- bind_rows(
  # Variavel que sera utilizada para faceting
  dd_data_list$dd_brasil %>% mutate(amostra = 'Amostra completa'),
  dd_data_list$dd_sp %>% mutate(amostra = "Apenas Estado de São Paulo")
) %>% 
  mutate(
    Plataforma = ifelse(comprasnet == 1, 'Comprasnet', 'BEC') %>%
      fct_relevel('Comprasnet', 'BEC'),
    # Variavel que identifica regime juridico e plataforma (6 valores)
    grupo =
      case_when(
        Plataforma == 'Comprasnet' & abertura_lances < data_20s ~ 'cnet0',
        Plataforma == 'Comprasnet' & abertura_lances < data_3s ~ 'cnet1',
        Plataforma == 'Comprasnet' ~ 'cnet3',
        Plataforma == 'BEC' & abertura_lances < data_20s ~ 'bec0',
        Plataforma == 'BEC' & abertura_lances < data_3s ~ 'bec1',
        Plataforma == 'BEC' ~ 'bec3'
      )
  )

# DF para usar em geom_label --------------------------------------------------
df_label <- 
  tibble(abertura_lances = rep(c(data_20s, data_3s), 2),
         label = rep(c('Regra 20s', 'Regra 3s'), 2),
         amostra = rep(c('Amostra completa', 'Apenas Estado de São Paulo'),
                       each = 2))

# Gráfico ---------------------------------------------------------------------
ggplot(df_plot) +
  stat_smooth(aes(x = abertura_lances, y = num_forn_lances,
                  group = grupo, linetype = Plataforma),
              geom = 'smooth', color = 'black',
              method = "lm") +
  geom_vline(xintercept = c(data_20s, data_3s),
             linetype = 'dotted') +
  geom_label(data = df_label,
             aes(x = abertura_lances, label = label), y = 10,
             size = 2.5, family = 'serif') +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')),
               labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet") +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  facet_wrap(~ amostra, nrow = 2)

ggsave('plots/parallel_trends/parallel_trends_participantes.png',
       height = 6, width = 6)

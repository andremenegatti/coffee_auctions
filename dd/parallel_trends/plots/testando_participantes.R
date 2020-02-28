library(tidyverse)
library(lubridate)

# Pasta onde os gráficos serão salvos
plot_folder <- 'plots/parallel_trends/participantes/'

# Importando dados ------------------------------------------------------------
bec <- readRDS('data/bec_cafe.rds') %>% 
  mutate(abertura_lances = as.Date(dt_inicio), Plataforma = 'BEC') %>%
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances, Plataforma)

cnet <- readRDS('data/cnet_cafe.rds') %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances, sigla_uf) %>% 
  mutate(abertura_lances = as.Date(abertura_lances), Plataforma = 'Comprasnet')

cnet_sp <- filter(cnet, sigla_uf == 'SP')

# Preparando dados ------------------------------------------------------------
plot_list <- map(
  .x = list(cnet, cnet_sp),
  .f = ~ select(.x, -sigla_uf) %>% 
    mutate(Plataforma = 'Comprasnet') %>% 
    PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>% # <<<<
    bind_rows(bec %>% PregoesBR::trim_df('win_bid_kg', perc = 2.5)) # <<<<
) %>% set_names(c('brasil', 'sp'))

df_plot <- bind_rows(
  # Variavel que sera utilizada para faceting
  plot_list$brasil %>% mutate(amostra = 'Amostra completa'),
  plot_list$sp %>% mutate(amostra = "Apenas Estado de São Paulo")
) %>% 
  mutate(
    Plataforma = factor(Plataforma, levels = c("Comprasnet", "BEC")),
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

# Plotting: "zoom" na amostra completa ----------------------------------------
base_plot_zoom <- df_plot %>%
  ggplot(aes(x = abertura_lances, linetype = Plataforma)) +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')), labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet",
       caption = "Nota: as linhas verticais indicam o início da vigência das regras dos 20s (esquerda) e dos 3s (direita).") +
  facet_wrap(~ amostra, nrow = 2, scales = "free_y")

# GAM, sem quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma), method = "gam",
              formula = y ~ s(x, bs = "cs"), color = 'black')
ggsave(str_c(plot_folder, 'continuous/gam_zoom.png'), width = 8.5, height = 7)

# GAM, com quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = grupo), method = "gam",
              formula = y ~ s(x, bs = "cs"), color = 'black')
ggsave(str_c(plot_folder, 'split/gam_zoom.png'), width = 8.5, height = 7)

# Loess, sem quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma),
              method = "loess", color = 'black')
ggsave(str_c(plot_folder, 'continuous/loess_zoom.png'), width = 8.5, height = 7)

# Loess, com quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = grupo),
              method = "loess", color = 'black')
ggsave(str_c(plot_folder, 'split/loess_zoom.png'), width = 8.5, height = 7)

# Linear, sem quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma),
              method = "lm", color = 'black')
ggsave(str_c(plot_folder, 'continuous/lm_zoom.png'), width = 8.5, height = 7)

# Linear, com quebra
base_plot_zoom +
  geom_smooth(aes(y = num_forn_lances, group = grupo),
              method = "lm", color = 'black')
ggsave(str_c(plot_folder, 'split/lm_zoom.png'), width = 8.5, height = 7)

# Plotting: usando apenas a amostra do DD -------------------------------------
base_plot_restrita <- df_plot %>%
  filter(abertura_lances >= '2011-03-01',
         abertura_lances < '2016-01-01') %>% 
  ggplot(aes(x = abertura_lances, linetype = Plataforma)) +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')), labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet",
       caption = "Nota: as linhas verticais indicam o início da vigência das regras dos 20s (esquerda) e dos 3s (direita).") +
  facet_wrap(~ amostra, nrow = 2, scales = "free_y")

# GAM, sem quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma), method = "gam",
              formula = y ~ s(x, bs = "cs"), color = 'black')
ggsave(str_c(plot_folder, 'continuous/gam_restrita.png'), width = 8.5, height = 7)

# GAM, com quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = grupo), method = "gam",
              formula = y ~ s(x, bs = "cs"), color = 'black')
ggsave(str_c(plot_folder, 'split/gam_restrita.png'), width = 8.5, height = 7)

# Loess, sem quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma),
              method = "loess", color = 'black')
ggsave(str_c(plot_folder, 'continuous/loess_restrita.png'), width = 8.5, height = 7)

# Loess, com quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = grupo),
              method = "loess", color = 'black')
ggsave(str_c(plot_folder, 'split/loess_restrita.png'), width = 8.5, height = 7)

# Linear, sem quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = Plataforma),
              method = "lm", color = 'black')
ggsave(str_c(plot_folder, 'continuous/lm_restrita.png'), width = 8.5, height = 7)

# Linear, com quebra
base_plot_restrita +
  geom_smooth(aes(y = num_forn_lances, group = grupo),
              method = "lm", color = 'black')
ggsave(str_c(plot_folder, 'split/lm_restrita.png'), width = 8.5, height = 7)
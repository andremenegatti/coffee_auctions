library(tidyverse)
library(lubridate)

# Importando dados ------------------------------------------------------------
bec <- readRDS('data/bec_cafe.rds') %>% 
  mutate(abertura_lances = as.Date(dt_inicio), Plataforma = 'BEC') %>%
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances,
         Plataforma, kg_fornecidos) %>% 
  filter(abertura_lances > '2011-01-01', abertura_lances < '2016-02-01') %>% 
  filter(win_bid_kg < 80)

cnet <- readRDS('data/cnet_cafe.rds') %>% 
  mutate(abertura_lances = as.Date(abertura_lances)) %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances,
         sigla_uf, kg_fornecidos) %>% 
  filter(abertura_lances > '2011-01-01', abertura_lances < '2016-02-01')

cnet_sp <- filter(cnet, sigla_uf == 'SP')

cnet <- PregoesBR::trim_df(cnet, 'win_bid_kg', perc = 1.5)

# Preparando dados ------------------------------------------------------------
plot_list <- map(
  .x = list(cnet, cnet_sp),
  .f = ~ select(.x, -sigla_uf) %>% 
    mutate(Plataforma = 'Comprasnet') %>% 
    PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>% # <<<<
    bind_rows(bec %>% PregoesBR::trim_df('win_bid_kg', perc = 2.5)) %>% # <<<<
    mutate(Plataforma = fct_relevel(Plataforma, 'Comprasnet', 'BEC')) 
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

# Não-ponderado ---------------------------------------------------------------
p1 <- df_plot %>% 
  ggplot() +
  stat_smooth(aes(x = abertura_lances, y = num_forn_lances,
                  linetype = Plataforma),
              geom = 'smooth', se = TRUE, col = 'black',
              method = 'loess', span = 0.6) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  geom_label(data = tibble(abertura_lances = c(data_20s, data_3s),
                           label = c('Regra 20s', 'Regra 3s')),
             aes(x = abertura_lances, label = label),
             y = 9.75, size = 3, family = 'serif') +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')),
               labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes",
       subtitle = "Comparação entre BEC e Comprasnet") +
  facet_wrap(~ amostra, nrow = 2) +
  theme(legend.position = 'bottom')

ggsave(plot = p1, width = 5, height = 6,
       filename = 'plots/descricao_amostra/smooth_evolucao_participantes.png')

# Salvando versao sem título para a dissertação
p2 <- p1 +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())

ggsave(plot = p2, width = 5, height = 6,
       filename = 
         '~/Documents/dissertacao/images/smooth_evolucao_participantes.png')

# Ponderado -------------------------------------------------------------------
df_plot %>% 
  ggplot() +
  stat_smooth(aes(x = abertura_lances, y = num_forn_lances,
                  linetype = Plataforma, weight = kg_fornecidos),
              geom = 'smooth', se = TRUE, col = 'black',
              method = 'loess', span = 0.6) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  geom_label(data = tibble(abertura_lances = c(data_20s, data_3s),
                           label = c('Regra 20s', 'Regra 3s')),
             aes(x = abertura_lances, label = label),
             y = 9.75, size = 2.5, family = 'serif') +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')),
               labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Evolução do número de participantes nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet") +
  facet_wrap(~ amostra, nrow = 2)

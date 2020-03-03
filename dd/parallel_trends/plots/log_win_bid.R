library(tidyverse)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = readRDS) %>% set_names(c('dd_brasil', 'dd_sp'))

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
  stat_smooth(aes(x = abertura_lances, y = log(win_bid_kg),
                  group = grupo, linetype = Plataforma),
              geom = 'smooth', color = 'black',
              method = "lm") +
  geom_vline(xintercept = c(data_20s, data_3s),
             linetype = 'dotted') +
  geom_label(data = df_label,
             aes(x = abertura_lances, label = label), y = 2.95,
             size = 2.5, family = 'serif') +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')),
               labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Log. do melhor lance",
       title = "Evolução dos preços nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet") +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01')),
                  ylim = c(2, 3)) +
  facet_wrap(~ amostra, nrow = 2)

ggsave('plots/parallel_trends/parallel_trends_log_win_bid.png',
       height = 6, width = 6)

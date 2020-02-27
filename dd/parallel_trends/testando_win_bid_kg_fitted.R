library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
# Principais
bec_cafe <- readRDS('data/bec_cafe_dd.rds')
cnet_cafe <- readRDS('data/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        # Removendo outliers: 2.5% de cada lado # <<<<
        PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>%
        # Selecionando apenas variaveis relevantes
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_por_unid, num_forn_lances,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2
        ) %>%
        # Coercing to factor to avoid warnings when joining dataframes
        mutate_if(is.factor, as.character)
  ) %>% 
  # Dando nomes aos DFs
  set_names(c('bec', 'cnet', 'cnet_sp'))

# Montando bases DD em uma lista ----------------------------------------------
dd_data_list <- list(data_list$cnet, data_list$cnet_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$bec) %>%
        PregoesBR::build_dd_df()
  ) %>% set_names(c('dd_brasil', 'dd_sp'))


# Modelos para obter resíduos e fitted values ---------------------------------
bec_cafe2 <- dd_data_list$dd_sp %>% 
  filter(comprasnet == 0)

cnet_cafe2 <- dd_data_list$dd_brasil %>% 
  filter(comprasnet == 1)

cnet_cafe_sp2 <- dd_data_list$dd_sp %>% 
  filter(comprasnet == 1)

mod_cnet_sp <- felm(log_win_bid ~ qualidade + kg_por_unid + arab_defl | 
                      bimestre + unidade_compradora +  municipio + 
                      marca_vencedor_principais,
                    data = cnet_cafe_sp2)

mod_cnet_br <- felm(log_win_bid ~ qualidade + kg_por_unid + arab_defl | 
                      bimestre + sigla_uf:bimestre + municipio + 
                      unidade_compradora,
                    data = cnet_cafe2)

mod_bec <- felm(log_win_bid ~ qualidade + kg_por_unid + arab_defl | 
                     bimestre + unidade_compradora +  municipio + 
                     marca_vencedor_principais,
                   data = bec_cafe2)

bec_cafe2 <- bec_cafe2 %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances) %>% 
  mutate(Plataforma = 'BEC',
         residuals = exp(mod_bec$residuals[, 1]),
         fitted = exp(mod_bec$fitted.values[, 1]))

cnet_cafe2 <- cnet_cafe2 %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances, sigla_uf) %>% 
  mutate(Plataforma = 'Comprasnet',
         residuals = exp(mod_cnet_br$residuals[, 1]),
         fitted = exp(mod_cnet_br$fitted.values[, 1]))

cnet_cafe_sp2 <- cnet_cafe_sp2 %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances, sigla_uf) %>% 
  mutate(Plataforma = 'Comprasnet',
         residuals = exp(mod_cnet_sp$residuals[, 1]),
         fitted = exp(mod_cnet_sp$fitted.values[, 1]))

# Preparando dados ------------------------------------------------------------
plot_list <- map(
  .x = list(cnet_cafe2, cnet_cafe_sp2),
  .f = ~ select(.x, -sigla_uf) %>% 
    mutate(Plataforma = 'Comprasnet') %>% 
    bind_rows(bec_cafe2)
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

# Plotting: usando apenas a amostra do DD -------------------------------------
base_plot_restrita <- df_plot %>%
  ggplot(aes(x = abertura_lances, linetype = Plataforma)) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')), labels = 2011:2017) +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = "Data do pregão",
       y = "Melhor lance (reais)",
       title = "Evolução dos preços nos pregões de café",
       subtitle = "Comparação entre BEC e Comprasnet",
       caption = "Nota: as linhas verticais indicam o início da vigência das regras dos 20s (esquerda) e dos 3s (direita).") +
  facet_wrap(~ amostra, nrow = 2, scales = "free_y")

# GAM, sem quebra
base_plot_restrita + 
  geom_point(aes(y = fitted, col = Plataforma),
                                alpha = 0.08, shape = 1) +
  geom_smooth(aes(y = fitted, group = Plataforma),
              method = "gam", formula = y ~ s(x, bs = "cs"), color = 'black') +
  coord_cartesian(ylim = c(5, 18), xlim = as.Date(c('2011-03-01', '2015-11-01')))
# ggsave(str_c(plot_folder, 'continuous/gam_restrita.png'), width = 8.5, height = 7)

# GAM, com quebra
base_plot_restrita +
  geom_point(aes(y = fitted, col = Plataforma),
             alpha = 0.08, shape = 1) +
  geom_smooth(aes(y = fitted, group = grupo),
              method = "gam", formula = y ~ s(x, bs = "cs"), color = 'black') +
  coord_cartesian(ylim = c(5, 18), xlim = as.Date(c('2011-03-01', '2015-11-01')))
# ggsave(str_c(plot_folder, 'split/gam_restrita.png'), width = 8.5, height = 7)

# Loess, sem quebra
base_plot_restrita +
  geom_point(aes(y = fitted, col = Plataforma),
             alpha = 0.08, shape = 1) +
  geom_smooth(aes(y = fitted, group = Plataforma),
              method = "loess", color = 'black') +
  coord_cartesian(ylim = c(5, 18), xlim = as.Date(c('2011-03-01', '2015-11-01')))
# ggsave(str_c(plot_folder, 'continuous/loess_restrita.png'), width = 8.5, height = 7)

# Loess, com quebra
base_plot_restrita +
  geom_point(aes(y = fitted, col = Plataforma),
             alpha = 0.08, shape = 1) +
  geom_smooth(aes(y = fitted, group = grupo),
              method = "loess", color = 'black') +
  coord_cartesian(ylim = c(5, 18), xlim = as.Date(c('2011-03-01', '2015-11-01')))
# ggsave(str_c(plot_folder, 'split/loess_restrita.png'), width = 8.5, height = 7)
library(stargazer)
library(tidyverse)
library(PregoesBR)
library(plotly)
library(viridis)

#### ABRINDO BASES ####
df_ipca <- read_csv('Dados/ipca.csv') %>%
  mutate(inicio_mes = lubridate::ymd(inicio_mes))

# Precos cafe ESALQ e futuros BLOOMBERG
controles <- readRDS('controles_futuros_arab_rob.rds') %>%
  mutate(arab_rob = (arab + rob)/2) %>%
  create_time_variables() %>%
  left_join(df_ipca, by = 'inicio_mes') %>%
  mutate(rob_defl = deflacionar(rob, indice_no_periodo = ipca),
         arab_defl = deflacionar(arab, indice_no_periodo = ipca),
         arab_rob_defl = deflacionar(arab_rob, indice_no_periodo = ipca),
         futuro_defl = deflacionar(futuro, indice_no_periodo = ipca))

controles <- controles %>%
  select(abertura_lances, arab, arab_defl, rob, rob_defl, arab_rob, arab_rob_defl, futuro, futuro_defl)

# Marcas
marcas <- readRDS('Marcas/marcas_full.rds') %>%
  select(id_item, marca_vencedor = marca_vencedor_clean_final, marca_vencedor_principais)

# Principais
BEC_cafe <- readRDS('BEC/BEC_cafe_etapa4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  filter(abertura_lances < '2016-01-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 0) %>%
  rename(unidade_compradora = UNIDADE_COMPRADORA) %>%
  left_join(controles, by = 'abertura_lances') %>%
  left_join(marcas, by = 'id_item')

cnet_cafe <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  filter(abertura_lances < '2016-01-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))  %>%
  left_join(controles, by = 'abertura_lances') %>%
  left_join(marcas, by = 'id_item') %>%
  # Atencao! Verificar pq ha NAs.
  # Resposta: sao os casos em que o campo 'situacao' esta como 'em analise' ou 'cancelado na aceitacao'
  # Acho que podemos excluir de todas as especificacoes para win_bid
  filter(!is.na(marca_vencedor))

#### PREPARANDO DADOS PARA ANALISE ####
# Renomeando variavel de preco
cnet_cafe <- cnet_cafe %>%
  mutate(win_bid = win_bid_kg)

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ trim_df(.x, 'win_bid') %>%
        select(id_item, abertura_lances,
               inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               win_bid, unidade_compradora, comprasnet,
               futuro, arab_rob, arab, rob,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               marca_vencedor_principais)) %>%
  set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

#### MONTANDO BASES DD ####
dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$BEC_cafe) %>%
        mutate(bimestre = factor(inicio_bimestre), mes = factor(inicio_mes), semana = factor(inicio_semana),
               unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
               marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais))
        )
  ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

# Especificacao DD
dd_formula <- 'win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + arab_rob'

#### PLACEBO GRID ####
bandwidth <- -60:60
placebo_cross <- crossing(data_20s_alt = bandwidth + data_20s,
                          data_3s_alt = bandwidth + data_3s)

placebo_cross <- placebo_cross %>%
  mutate(sp_cafe_dd = map2(.x = data_20s_alt, .y = data_3s_alt,
                           .f = ~ dd_data_list$sp_cafe_dd %>%
                             mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= .x & abertura_lances < .y, 1, 0),
                                    treat2 = if_else(comprasnet == 1 & abertura_lances > .y, 1, 0))))

placebo_cross <- placebo_cross %>%
  mutate(model_summary = map(.x = sp_cafe_dd,
                             .f = ~ lm(dd_formula, data = .x) %>%
                               broom::tidy()))

placebo_cross <- placebo_cross %>%
  mutate(treat1_est = map_dbl(.x = model_summary,
                              .f = ~ .x %>% filter(term == 'treat1') %>% select(estimate) %>% unlist())) %>%
  mutate(treat1_std = map_dbl(.x = model_summary,
                              .f = ~ .x %>% filter(term == 'treat1') %>% select(std.error) %>% unlist())) %>%
  mutate(treat1_upper = treat1_est + 2*treat1_std,
         treat1_lower = treat1_est - 2*treat1_std) %>%
  mutate(treat2_est = map_dbl(.x = model_summary,
                              .f = ~ .x %>% filter(term == 'treat2') %>% select(estimate) %>% unlist())) %>%
  mutate(treat2_std = map_dbl(.x = model_summary,
                              .f = ~ .x %>% filter(term == 'treat2') %>% select(std.error) %>% unlist())) %>%
  mutate(treat2_upper = treat2_est + 2*treat2_std,
         treat2_lower = treat2_est - 2*treat2_std)

# Heatmaps/tiles
library(viridis)

tiles_treat1 <- ggplot(placebo_cross) +
  geom_tile(aes(x = data_20s_alt, y = data_3s_alt, fill = treat1_est)) +
  scale_fill_viridis(option = 'C') +
  geom_vline(xintercept = data_20s, color = 'black', linetype = 'dotted') +
  geom_hline(yintercept = data_3s, color = 'black', linetype = 'dotted') +
  ggtitle('Placebo - Efeito da regra dos 20s')

tiles_treat2 <- ggplot(placebo_cross) +
  geom_tile(aes(x = data_20s_alt, y = data_3s_alt, fill = treat2_est)) +
  scale_fill_viridis(option = 'C') +
  geom_vline(xintercept = data_20s, color = 'black', linetype = 'dotted') +
  geom_hline(yintercept = data_3s, color = 'black', linetype = 'dotted') +
  ggtitle('Placebo - Efeito da regra dos 3s')

tiles_treat1 %>% ggsave('C:/Users/Dell/Desktop/placebo_20s_heat.png')
tiles_treat2 %>% ggsave('C:/Users/Dell/Desktop/placebo_3s_heat.png')


# 3D plots
library(plotly)

treat1_matrix <- matrix(placebo_cross$treat1_est, nrow = 121, ncol = 121, byrow = TRUE)
treat1_lower_matrix <- matrix(placebo_cross$treat1_lower, nrow = 121, ncol = 121, byrow = TRUE)
treat1_upper_matrix <- matrix(placebo_cross$treat1_upper, nrow = 121, ncol = 121, byrow = TRUE)

treat2_matrix <- matrix(placebo_cross$treat2_est, nrow = 121)
treat2_lower_matrix <- matrix(placebo_cross$treat2_lower, nrow = 121)
treat2_upper_matrix <- matrix(placebo_cross$treat2_upper, nrow = 121)

p_3s <- plot_ly() %>%
  add_surface(x = ~bandwidth,
              y = ~bandwidth,
              z = ~treat2_matrix,
              name = 'Estimativa pontual',
              showlegend = TRUE,
              colorbar = list(title = 'Coeficiente estimado')
  ) %>%
  add_surface(x = ~bandwidth, y = ~bandwidth, z = ~treat2_lower_matrix,
              opacity = 0.3, name = 'Limite inferior - Int. Conf. 95%', showscale = FALSE, colorscale = 'Hot') %>%
  add_surface(x = ~bandwidth, y = ~bandwidth, z = ~ treat2_upper_matrix,
              opacity = 0.3, name = 'Limite superior - Int. Conf. 95%', showscale = FALSE, colorscale = 'Hot') %>%
  layout( title = 'Placebo - Efeito da regra dos 3s',
          scene = list(
            xaxis = list(title = "Data Regra 20s"),
            yaxis = list(title = "Data Regra 3s"),
            zaxis = list(title = "Efeito estimado Regra 3s")
          )
  )

htmlwidgets::saveWidget(p_3s, 'C:/Users/Dell/Desktop/placebo_3s_surface.html')


p_20s <- plot_ly() %>%
  add_surface(x = ~bandwidth,
              y = ~bandwidth,
              z = ~treat1_matrix,
              name = 'Estimativa pontual',
              showlegend = TRUE,
              colorbar = list(title = 'Coeficiente estimado')
  ) %>%
  add_surface(x = ~bandwidth, y = ~bandwidth, z = ~treat1_lower_matrix,
              opacity = 0.3, name = 'Limite inferior - Int. Conf. 95%', showscale = FALSE, colorscale = 'Hot') %>%
  add_surface(x = ~bandwidth, y = ~bandwidth, z = ~ treat1_upper_matrix,
              opacity = 0.3, name = 'Limite superior - Int. Conf. 95%', showscale = FALSE, colorscale = 'Hot') %>%
  layout( title = 'Placebo - Efeito da regra dos 20s',
          scene = list(
            xaxis = list(title = "Data Regra 20s"),
            yaxis = list(title = "Data Regra 3s"),
            zaxis = list(title = "Efeito estimado Regra 20s")
          )
  )

htmlwidgets::saveWidget(p_20s, 'C:/Users/Dell/Desktop/placebo_20s_surface.html')


#### CAETERIS PARIBUS ####
# Alterando data da regra dos 3s, mantendo 20s constante
placebo_df_3s <- tibble(data_3s_alt = (-120:120) + data_3s)

placebo_df_3s <- placebo_df_3s %>%
  mutate(sp_cafe_dd = map(.x = data_3s_alt,
                          .f = ~ dd_data_list$sp_cafe_dd %>%
                            mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x, 1, 0),
                                   treat2 = if_else(comprasnet == 1 & abertura_lances > .x, 1, 0)))) %>%
  mutate(dd_model = map(.x = sp_cafe_dd,
                        .f = ~ lm(dd_formula, data = .x))) %>%
  mutate(tidy_summary = map(.x = dd_model,
                            .f = ~ broom::tidy(.x))) %>%
  mutate(treat2_est = map_dbl(.x = tidy_summary,
                              .f = ~ .x %>%
                                filter(term == 'treat2') %>%
                                select(estimate) %>%
                                unlist()),
         treat2_std = map_dbl(.x = tidy_summary,
                              .f = ~ .x %>%
                                filter(term == 'treat2') %>%
                                select(std.error) %>%
                                unlist())) %>%
  mutate(upper_bound = treat2_est + 2*treat2_std,
         lower_bound = treat2_est - 2*treat2_std)

# Grafico
ggplot(data = placebo_df_3s) +
  geom_line(aes(x = data_3s_alt, y = treat2_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_3s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_3s_alt, y = upper_bound), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_3s_alt, y = lower_bound), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0) +
  scale_x_date(breaks = as.Date(c('2013-10-01', '2014-01-02', '2014-04-01')),
               labels = c('Out./2013')) +
  labs(title = 'Teste Placebo',
       subtitle = 'Efeito da regra dos 3s',
       caption = 'Notas:,
       1) Utilizou-se janela de 360 dias ao redor da verdadeira data da regra dos 3s;
       2) A data da regra dos 20s foi mantida constante;
       3) A linha vertical pontilhada indica a verdadeira data da regra dos 3s;
       4) As linhas hachuradas indicam o intervalo de confiança de 95%.',
       x = 'Data da regra dos 3s',
       y = 'Estimativa do efeito (coeficiente)')

ggsave('C:/Users/Dell/Desktop/placebo_3s_lineplot.png', height = 5, width = 8)

# Alterando data da regra dos 20s, mantendo 3s constante
placebo_df_20s <- tibble(data_20s_alt = (-180:180) + data_20s) %>%
  mutate(sp_cafe_dd = map(.x = data_20s_alt,
                          .f = ~ dd_data_list$sp_cafe_dd %>%
                            mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= .x & abertura_lances < data_3s, 1, 0),
                                   treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0)))) %>%
  mutate(dd_model = map(.x = sp_cafe_dd,
                        .f = ~ lm(dd_formula,
                                  data = .x))) %>%
  mutate(tidy_summary = map(.x = dd_model,
                            .f = ~ broom::tidy(.x))) %>%
  mutate(treat1_est = map_dbl(.x = tidy_summary,
                              .f = ~ .x %>%
                                filter(term == 'treat1') %>%
                                select(estimate) %>%
                                unlist()),
         treat1_std = map_dbl(.x = tidy_summary,
                              .f = ~ .x %>%
                                filter(term == 'treat1') %>%
                                select(std.error) %>%
                                unlist())) %>%
  mutate(upper_bound = treat1_est + 2*treat1_std,
         lower_bound = treat1_est - 2*treat1_std)

ggplot(data = placebo_df_20s) +
  geom_line(aes(x = data_20s_alt, y = treat1_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_20s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_20s_alt, y = upper_bound), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_20s_alt, y = lower_bound), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0)

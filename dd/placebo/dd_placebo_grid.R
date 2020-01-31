library(tidyverse)
library(plotly)
# Outros pacotes necessarios: broom, lfe, viridis, htmlwidgets

# Executando script auxiliar
source('helpers_placebo.R')

# Carregando base de dados
dd_sp <- readRDS('base_dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Opcional: Definindo tema para graficos mais bonitos
theme_set(my_theme())

# ----------------------------- #
#### RODANDO MODELOS PLACEBO ####
# ----------------------------- #

# Janela a ser aberta ao redor de cada data
bandwidth <- -60:60 # 60 de cada lado
## Nota:
## Para deixar computacionalmente mais leve, experimentar colocar intervalos entre as datas

# Cruzando datas: todas 121x121 = 14631 combinacoes
placebo_cross <-
  crossing(data_20s_alt = bandwidth + data_20s,
           data_3s_alt = bandwidth + data_3s) %>%
  # Criando colunas com dummies de tratamento alternativas (cada celula contem uma lista)
  mutate(
    treat1 = map2(
      .x = data_20s_alt,
      .y = data_3s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances >= .x & abertura_lances < .y, 0 , 1)
      ),
    treat2 = map(
      .x = data_3s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances > .x, 1, 0)
      )
    )

# Rodando modelos para todas as combinacoes e salvando resultados na coluna 'model_summary'
# Pode demorar...
placebo_cross <- placebo_cross %>%
  mutate(
    model_summary = map2(
      .x = treat1,
      .y = treat2,
      .f = ~ lfe::felm(
        log_win_bid ~ .x + .y + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais
        ) %>%
        # Extraindo resultados de forma limpa e organizada usando pacote broom
        broom::tidy() %>%
        # Substituindo '.x' por 'treat1' e '.y' por 'treat2' nos DFs de resultados
        mutate(
          term = case_when(
            term == '.x' ~ 'treat1',
            term == '.y' ~ 'treat2',
            !term %in% c('.x', '.y') ~ term
            )
          )
      )
    ) %>%
  # Usando funcao definida no script auxiliar para extrair dados dos resultados
  # e criar colunas com coeficientes e erros padrao
  get_tidy_estimates()

# ---------------------------- #
####    HEATMAPS / TILES    ####
# ---------------------------- #

# Grafico Placebo 20s
tiles_treat1 <- ggplot(placebo_cross) +
  geom_tile(aes(x = data_20s_alt, y = data_3s_alt, fill = treat1_est)) +
  viridis::scale_fill_viridis(option = 'C', name = 'Coeficiente\nEfeito Regra 20s') +
  geom_vline(xintercept = data_20s, color = 'black', linetype = 'dotted') +
  geom_hline(yintercept = data_3s, color = 'black', linetype = 'dotted') +
  labs(
    title = 'Placebo - Efeito da regra dos 20s',
    subtitle = 'Grade com datas de implementação alternativas para ambas as regras',
    x = 'Data da Regra dos 20s',
    y = 'Data Regra dos 3s',
    caption = "Notas:
    1) Foi considerada uma janela de 120 dias ao redor da implementação de cada regra;
    2) As linhas hachuradas indicam as verdadeiras datas de implementação;
    3) O centro do gráfico representa o efeito estimado 'verdadeiro' "
  )

tiles_treat1

# Grafico Placebo 3s
tiles_treat2 <- ggplot(placebo_cross) +
  geom_tile(aes(x = data_20s_alt, y = data_3s_alt, fill = treat2_est)) +
  viridis::scale_fill_viridis(option = 'C', name = 'Coeficiente') +
  geom_vline(xintercept = data_20s, color = 'black', linetype = 'dotted') +
  geom_hline(yintercept = data_3s, color = 'black', linetype = 'dotted') +
  labs(
    title = 'Placebo - Efeito da regra dos 3s',
    subtitle = 'Grade com datas de implementação alternativas para ambas as regras',
    x = 'Data da Regra dos 20s',
    y = 'Data Regra dos 3s',
    caption = "Notas:
    1) Foi considerada uma janela de 120 dias ao redor da implementação de cada regra;
    2) As linhas hachuradas indicam as verdadeiras datas de implementação;
    3) O centro do gráfico representa o efeito estimado 'verdadeiro' "
    )

tiles_treat2

# ------------------------ #
####      3D PLOTS      ####
# ------------------------ #

# Montando matrizes necessarias para o grafico - Regra 3s
treat2_matrix <- matrix(placebo_cross$treat2_est, nrow = 121)
treat2_lower_matrix <- matrix(placebo_cross$treat2_lower, nrow = 121)
treat2_upper_matrix <- matrix(placebo_cross$treat2_upper, nrow = 121)

# Montando grafico - Regra 3s
p_3s <- plot_ly() %>%
  add_surface(x = ~ bandwidth,
              y = ~ bandwidth,
              z = ~ treat2_matrix,
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

# Salvando como pagina html
htmlwidgets::saveWidget(p_3s, 'placebo_3s_surface.html')

# Montando matrizes necessarias para o grafico - Regra 20s
treat1_matrix <- matrix(placebo_cross$treat1_est, nrow = 121, ncol = 121, byrow = TRUE)
treat1_lower_matrix <- matrix(placebo_cross$treat1_lower, nrow = 121, ncol = 121, byrow = TRUE)
treat1_upper_matrix <- matrix(placebo_cross$treat1_upper, nrow = 121, ncol = 121, byrow = TRUE)

# Montando gráfico - Regra 20s
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

# Salvando como pagina html - Regra 20s
htmlwidgets::saveWidget(p_20s, 'placebo_20s_surface.html')


library(stargazer)
library(tidyverse)
library(PregoesBR)

# Loading data ----------------------------------------------------------------
bec <- readRDS('data/bec_cafe_dd.rds')
cnet <- readRDS('data/cnet_cafe_dd.rds')
cnet_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec, cnet, cnet_sp) %>%
  map(.f = ~ trim_df(.x, 'win_bid_kg', perc = 2.5) %>% # <<<<
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_por_unid, num_forn_lances,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_defl,
               qualidade, qualidade2
        ) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(regime_juridico = case_when(
          abertura_lances < data_20s ~ 1,
          abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
          abertura_lances >= data_3s ~ 3
        ) %>% as.factor())
  ) %>%
  # Dando nomes aos DFs
  set_names(c('bec', 'cnet', 'cnet_sp'))

# Summary statistics ----------------------------------------------------------
data_list$bec %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(data_list$cnet %>% mutate(Grupo = "Tratamento")) %>%
  bind_rows(data_list$cnet_sp %>% mutate(Grupo = "Tratamento - SP")) %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(num_forn_lances, na.rm = TRUE),
            Mediana = median(num_forn_lances, na.rm = TRUE),
            SD = sd(num_forn_lances, na.rm = TRUE)) %>%
  write.csv2('dd/descricao_amostra/tabela_descritiva_num_forn_lances.csv',
             row.names = FALSE)

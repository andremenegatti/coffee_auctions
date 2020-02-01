library(stargazer)
library(tidyverse)
library(PregoesBR)

#### ABRINDO BASES ####
# Principais
BEC_cafe <- readRDS('DD/bec_cafe_dd.rds')
cnet_cafe <- readRDS('DD/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('DD/cnet_cafe_sp_dd.rds')

#### PREPARANDO BASES ####
data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        # EXCLUINDO OBSERVACOES POSTERIORES A 2016 ## <----------
      filter(abertura_lances < '2016-01-01') %>%
        # TRIMMING: 2.5% de cada lado ## <----------------------
      # trim_df('win_bid', perc = 5) %>%
        # Selecionando apenas variaveis relevantes
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid,
               num_forn_lances,
               comprasnet,
               sigla_uf, municipio,
               unidade_compradora,
               marca_vencedor_principais, kg_por_unid,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2
        ) %>%
        # Transformando todos os factors em caracteres para evitar warnings ao juntar bases
        mutate_if(is.factor, as.character) %>%
        # filter(kg_por_unid != 0.25) %>%
        # Incluindo residuos de 1o estagio: win_bid ~ unidade_compradora
        # add_1st_stage_residuals(formula = 'win_bid ~ unidade_compradora', residuals_column_name = 'res_EF_uasgs') %>%
        # Criando variavel de regime juridico
        mutate(regime_juridico = case_when(
          abertura_lances < data_20s ~ 1,
          abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
          abertura_lances >= data_3s ~ 3
        ) %>%
          as.factor()
        )
  ) %>%
  # Dando nomes aos DFs
  set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))


data_list$BEC_cafe %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(data_list$cnet_cafe %>% mutate(Grupo = "Tratamento")) %>%
  bind_rows(data_list$cnet_cafe_sp %>% mutate(Grupo = "Tratamento - SP")) %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(num_forn_lances, na.rm = TRUE),
            Mediana = median(num_forn_lances, na.rm = TRUE),
            SD = sd(num_forn_lances, na.rm = TRUE)) %>%
  write.csv2('tabela_descritiva_num_forn_lances.csv', row.names = FALSE)

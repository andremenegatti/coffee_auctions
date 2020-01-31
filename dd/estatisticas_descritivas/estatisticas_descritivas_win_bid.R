library(stargazer)
library(tidyverse)
library(PregoesBR)

data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

#### ABRINDO BASES ####
futuros <- readRDS('Dados/futuros.rds') %>%
  select(inicio_mes, futuro_media_mensal) %>%
  distinct()

precos_cafe_cepea <- readRDS('Dados/precos_cafe_cepea.rds') %>%
  select(inicio_mes, pr_mensal_arab, pr_mensal_rob) %>%
  distinct() %>%
  mutate(media_arab_rob = (pr_mensal_arab + pr_mensal_rob)/2)

# Marcas
marcas <- readRDS('Marcas/marcas_full.rds') %>%
  select(id_item, marca_vencedor = marca_vencedor_clean2, marca_vencedor_principais)

# Principais
BEC_cafe <- readRDS('BEC/BEC_cafe_etapa4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances), comprasnet = 0) %>%
  rename(unidade_compradora = UNIDADE_COMPRADORA) %>%
  left_join(precos_cafe_cepea, by = 'inicio_mes') %>%
  left_join(futuros, by = 'inicio_mes') %>%
  left_join(marcas, by = 'id_item')

cnet_cafe <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))  %>%
  left_join(precos_cafe_cepea, by = 'inicio_mes') %>%
  left_join(futuros, by = 'inicio_mes') %>%
  left_join(marcas, by = 'id_item') %>%
  filter(!is.na(marca_vencedor)) #### <<<<<------ Isso eh um problema???

#### PREPARANDO DADOS PARA ANALISE ####

# Renomeando variavel de preco
cnet_cafe <- cnet_cafe %>%
  mutate(win_bid = win_bid_kg)

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')


data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        trim_df('win_bid') %>%
        select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               win_bid, unidade_compradora, comprasnet,
               futuro_media_mensal, media_arab_rob,
               marca_vencedor, marca_vencedor_principais) %>%
        mutate(regime_juridico = case_when(abertura_lances < data_20s ~ 1,
                                           abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
                                           abertura_lances >= data_3s ~ 3) %>%
                 factor(labels = c('Sem intervalo minimo', 'Regra 20s', 'Regra 20s + Regra 3s'))) %>%
        filter(!is.na(win_bid))
  ) %>%
  set_names(c('BEC_cafe_1st_stage', 'cnet_cafe_1st_stage', 'sp_cnet_cafe_1st_stage'))

attach(data_list)

BEC_cafe_1st_stage %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(cnet_cafe_1st_stage %>%
              mutate(Grupo = "Tratamento")) %>%
  bind_rows(sp_cnet_cafe_1st_stage %>%
              mutate(Grupo = "Tratamento - SP")) %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(win_bid, na.rm = TRUE),
            SD = sd(win_bid, na.rm = TRUE)) %>%
  write.csv2('tabela_descritiva_win_bid.csv', row.names = FALSE)

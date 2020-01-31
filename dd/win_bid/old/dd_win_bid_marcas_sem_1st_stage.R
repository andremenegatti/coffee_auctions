library(stargazer)
library(tidyverse)
library(PregoesBR)

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
  filter(!is.na(marca_vencedor))

#### PREPARANDO DADOS PARA ANALISE ####

# Renomeando variavel de preco
cnet_cafe <- cnet_cafe %>%
  mutate(win_bid = win_bid_kg)

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

# Trimming (NECESSARIO APENAS EM WIN_BID)
# BEC_cafe <- trim_df(BEC_cafe, 'win_bid')
# cnet_cafe <- trim_df(cnet_cafe, 'win_bid')
# cnet_cafe_sp <- trim_df(cnet_cafe_sp, 'win_bid')

data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ trim_df(.x, 'win_bid') %>%
        select(id_item, abertura_lances,
               inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               win_bid, unidade_compradora, comprasnet,
               futuro_media_mensal, media_arab_rob,
               marca_vencedor_principais)) %>%
  set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

#### MONTANDO BASES DD ####

dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$BEC_cafe) %>%
         mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
                treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
                bimestre = factor(inicio_bimestre), mes = factor(inicio_mes), semana = factor(inicio_semana),
                unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
                marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais))
                )
       ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

attach(dd_data_list)

#### DD ####
attach(full_cafe_dd)

# Dummies de bimestre
dd_win_bid <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2)
# dd_win_bid_marca <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais)
dd_win_bid_uasg <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + unidade_compradora)
dd_win_bid_marca_uasg <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + unidade_compradora)
# dd_win_bid_marca_uasg_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais:comprasnet + unidade_compradora)
dd_win_bid_marca_uasg_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora)
dd_win_bid_marca_uasg_futuro <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + unidade_compradora + futuro_media_mensal)
# dd_win_bid_marca_uasg_futuro_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais:comprasnet + unidade_compradora + futuro_media_mensal)
dd_win_bid_marca_uasg_futuro_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro_media_mensal)
dd_win_bid_marca_uasg_cepea <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + unidade_compradora + media_arab_rob)
dd_win_bid_marca_uasg_cepea_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + media_arab_rob)
dd_win_bid_full <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + unidade_compradora + futuro_media_mensal + media_arab_rob)
dd_win_bid_full_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro_media_mensal + media_arab_rob)

detach(full_cafe_dd)

stargazer(dd_win_bid,
          dd_win_bid_uasg,
          dd_win_bid_marca_uasg,
          dd_win_bid_marca_uasg_int,
          dd_win_bid_marca_uasg_futuro_int,
          dd_win_bid_marca_uasg_cepea_int,
          # dd_win_bid_full,
          dd_win_bid_full_int,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_", "comprasnet:"),
          omit.stat = c("ser", "adj.rsq", "f"),
          # type = 'text',
          decimal.mark = ",",
          digit.separator = ".")

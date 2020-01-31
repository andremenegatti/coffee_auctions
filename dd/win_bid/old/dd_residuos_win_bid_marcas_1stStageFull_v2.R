library(stargazer)
library(tidyverse)
library(PregoesBR)

#### ABRINDO BASES ####
# Futuros Bloomberg
futuros <- readRDS('Dados/futuros.rds') %>%
  select(inicio_mes, futuro_media_mensal) %>%
  distinct()

# Precos cafe ESALQ/CEPEA
precos_cafe_cepea <- readRDS('Dados/precos_cafe_cepea.rds') %>%
  select(inicio_mes, pr_mensal_arab, pr_mensal_rob) %>%
  distinct() %>%
  mutate(media_arab_rob = (pr_mensal_arab + pr_mensal_rob)/2)

# Marcas dos vencedores
marcas <- readRDS('Marcas/marcas_full.rds') %>%
  select(id_item, marca_vencedor = marca_vencedor_clean2, marca_vencedor_principais)

# BEC
BEC_cafe <- readRDS('BEC/BEC_cafe_etapa4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances), comprasnet = 0) %>%
  left_join(precos_cafe_cepea, by = 'inicio_mes') %>%
  left_join(futuros, by = 'inicio_mes') %>%
  left_join(marcas, by = 'id_item')

# Comprasnet
cnet_cafe <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))  %>%
  left_join(precos_cafe_cepea, by = 'inicio_mes') %>%
  left_join(futuros, by = 'inicio_mes') %>%
  left_join(marcas, by = 'id_item') %>%
  filter(!is.na(marca_vencedor))

#### ORGANIZANDO BASES ####

# Renomeando variavel de preco (queremos usar a corrigida com dados de propostas)
cnet_cafe <- cnet_cafe %>%
  mutate(win_bid = win_bid_kg)

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

# Trimming (NECESSARIO APENAS EM WIN_BID)
BEC_cafe <- trim_df(BEC_cafe, 'win_bid')
cnet_cafe <- trim_df(cnet_cafe, 'win_bid')
cnet_cafe_sp <- trim_df(cnet_cafe_sp, 'win_bid')

#### CONSTRUINDO BASES PARA O PRIMEIRO ESTAGIO ####

# BEC
BEC_cafe_1st_stage <- BEC_cafe %>%
  select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
         win_bid, unidade_compradora = UNIDADE_COMPRADORA, comprasnet,
         futuro_media_mensal, media_arab_rob,
         marca_vencedor, marca_vencedor_principais
  )

# Comprasnet: todas as UASGs
cnet_cafe_1st_stage <- cnet_cafe %>%
  select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
         win_bid, unidade_compradora, comprasnet,
         futuro_media_mensal, media_arab_rob,
         marca_vencedor, marca_vencedor_principais
  )

# Comprasnet: apenas SP
sp_cnet_cafe_1st_stage <- cnet_cafe_sp %>%
  select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
         win_bid, unidade_compradora, comprasnet,
         futuro_media_mensal, media_arab_rob,
         marca_vencedor, marca_vencedor_principais
  )

# Bases completas
df_1st_stage_full <- bind_rows(cnet_cafe_1st_stage, BEC_cafe_1st_stage)
df_1st_stage_sp <- bind_rows(sp_cnet_cafe_1st_stage, BEC_cafe_1st_stage)


#### RODANDO REGRESSOES PRIMEIRO ESTAGIO ####

# 4) EF marca + comprasnet, com interacoes
m4 <- lm(win_bid ~ marca_vencedor_principais + marca_vencedor_principais:comprasnet, data = df_1st_stage_full)
df_1st_stage_full$res_m4 <- m4$residuals

m4_sp <- lm(win_bid ~ marca_vencedor_principais + marca_vencedor_principais:comprasnet, data = df_1st_stage_sp)
df_1st_stage_sp$res_m4 <- m4_sp$residuals


# 4) EF marca + comprasnet, com interacoes + EF unidade_compradora
m5 <- lm(win_bid ~ marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora, data = df_1st_stage_full)
df_1st_stage_full$res_m5 <- m5$residuals

m5_sp <- lm(win_bid ~ marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora, data = df_1st_stage_sp)
df_1st_stage_sp$res_m5 <- m5_sp$residuals


#### MONTANDO BASES DD COM RESIDUOS ####

# Toda a amostra
full_cafe_dd <- df_1st_stage_full %>%
  mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
         treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
         bimestre = factor(inicio_bimestre),
         mes = factor(inicio_mes),
         semana = factor(inicio_semana),
         unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
         marca_vencedor = as.factor(str_c('marca', marca_vencedor_principais)))

# Apenas SP
sp_cafe_dd <- df_1st_stage_sp %>%
  mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
         treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
         bimestre = factor(inicio_bimestre),
         mes = factor(inicio_mes),
         semana = factor(inicio_semana),
         unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
         marca_vencedor = as.factor(str_c('marca', marca_vencedor_principais)))


attach(sp_cafe_dd)

dd_win_bid_bimestre <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2)

dd_res_m4_bimestre <- lm(res_m4 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m4_bimestre_futuro <- lm(res_m4 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m4_bimestre_cepea <- lm(res_m4 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m4_bimestre_full <- lm(res_m4 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

dd_res_m5_bimestre <- lm(res_m5 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m5_bimestre_futuro <- lm(res_m5 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m5_bimestre_cepea <- lm(res_m5 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m5_bimestre_full <- lm(res_m5 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

detach(sp_cafe_dd)

stargazer(dd_win_bid_bimestre,
          # dd_res_m5_bimestre,
          # dd_res_m5_bimestre_futuro,
          # dd_res_m5_bimestre_cepea,
          # dd_res_m5_bimestre_full,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_"),
          omit.stat = c("ser", "adj.rsq", "f"),
          type = 'text',
          decimal.mark = ",",
          digit.separator = ".")

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
  filter(!is.na(marca_vencedor)) #### <<<<<------ Isso eh um problema???

#### PREPARANDO DADOS PARA ANALISE ####
# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

#### CONSTRUINDO BASES PARA O PRIMEIRO ESTAGIO ####

# Selecionando variaveis
data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        select(id_item, abertura_lances, inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               unidade_compradora, comprasnet, num_forn_lances,
               futuro_media_mensal, media_arab_rob,
               marca_vencedor, marca_vencedor_principais) %>%
        filter(!is.na(num_forn_lances))
  ) %>%
  set_names(c('BEC_cafe_1st_stage', 'cnet_cafe_1st_stage', 'sp_cnet_cafe_1st_stage'))

attach(data_list)


#### RODANDO REGRESSOES PRIMEIRO ESTAGIO ####

# 1) EF marca
m1_bec <- lm(num_forn_lances ~ marca_vencedor_principais, data = BEC_cafe_1st_stage)
BEC_cafe_1st_stage$res_m1 <- m1_bec$residuals

m1_cnet <- lm(num_forn_lances ~ marca_vencedor_principais, data = cnet_cafe_1st_stage)
cnet_cafe_1st_stage$res_m1 <- m1_cnet$residuals

m1_cnet_sp <- lm(num_forn_lances ~ marca_vencedor_principais, data = sp_cnet_cafe_1st_stage)
sp_cnet_cafe_1st_stage$res_m1 <- m1_cnet_sp$residuals

# 2) EF marca + EF unidade compradora
m2_bec <- lm(num_forn_lances ~ marca_vencedor_principais + unidade_compradora, data = BEC_cafe_1st_stage)
BEC_cafe_1st_stage$res_m2 <- m2_bec$residuals

m2_cnet <- lm(num_forn_lances ~ marca_vencedor_principais + unidade_compradora, data = cnet_cafe_1st_stage)
cnet_cafe_1st_stage$res_m2 <- m2_cnet$residuals

m2_cnet_sp <- lm(num_forn_lances ~ marca_vencedor_principais + unidade_compradora, data = sp_cnet_cafe_1st_stage)
sp_cnet_cafe_1st_stage$res_m2 <- m2_cnet_sp$residuals

# 3) EF unidade compradora
m3_bec <- lm(num_forn_lances ~ unidade_compradora, data = BEC_cafe_1st_stage)
BEC_cafe_1st_stage$res_m3 <- m3_bec$residuals

m3_cnet <- lm(num_forn_lances ~ unidade_compradora, data = cnet_cafe_1st_stage)
cnet_cafe_1st_stage$res_m3 <- m3_cnet$residuals

m3_cnet_sp <- lm(num_forn_lances ~ unidade_compradora, data = sp_cnet_cafe_1st_stage)
sp_cnet_cafe_1st_stage$res_m3 <- m3_cnet_sp$residuals


#### MONTANDO BASES DD COM RESIDUOS ####

# Toda a amostra
full_cafe_dd <- bind_rows(BEC_cafe_1st_stage, cnet_cafe_1st_stage) %>%
  build_dd_df()

# Apenas SP
sp_cafe_dd <- bind_rows(BEC_cafe_1st_stage, sp_cnet_cafe_1st_stage) %>%
  build_dd_df()


#### DD: TODA A AMOSTRA ####
attach(full_cafe_dd)

# Dummies de bimestre
dd_num_forn_lances_bimestre <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m1_bimestre <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m1_bimestre_futuro <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m1_bimestre_cepea <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m1_bimestre_full <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

dd_num_forn_lances_bimestre <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m2_bimestre <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m2_bimestre_futuro <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m2_bimestre_cepea <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m2_bimestre_full <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

dd_num_forn_lances_bimestre <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m3_bimestre <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m3_bimestre_futuro <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m3_bimestre_cepea <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m3_bimestre_full <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

detach(full_cafe_dd)

stargazer(dd_num_forn_lances_bimestre,
          dd_res_m3_bimestre,
          dd_res_m3_bimestre_futuro,
          dd_res_m3_bimestre_cepea,
          dd_res_m3_bimestre_full,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_"),
          omit.stat = c("ser", "adj.rsq"),
          type = 'text',
          decimal.mark = ",",
          digit.separator = ".")


#### DD: APENAS SP ####

attach(sp_cafe_dd)

# Dummies de bimestre
dd_num_forn_lances_bimestre_sp <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m1_bimestre_sp <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m1_bimestre_futuro_sp <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m1_bimestre_cepea_sp <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m1_bimestre_full_sp <- lm(res_m1 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

dd_num_forn_lances_bimestre_sp <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m2_bimestre_sp <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m2_bimestre_futuro_sp <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m2_bimestre_cepea_sp <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m2_bimestre_full_sp <- lm(res_m2 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

dd_num_forn_lances_bimestre_sp <- lm(num_forn_lances ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m3_bimestre_sp <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2)
dd_res_m3_bimestre_futuro_sp <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal)
dd_res_m3_bimestre_cepea_sp <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + media_arab_rob)
dd_res_m3_bimestre_full_sp <- lm(res_m3 ~ comprasnet + bimestre + treat1 + treat2 + futuro_media_mensal + media_arab_rob)

detach(sp_cafe_dd)

stargazer(dd_num_forn_lances_bimestre_sp,
          dd_res_m2_bimestre_sp,
          dd_res_m2_bimestre_futuro_sp,
          dd_res_m2_bimestre_cepea_sp,
          dd_res_m2_bimestre_full_sp,
          omit = c("bimestre", "semana", "mes", "unidade_"),
          omit.stat = c("ser", "adj.rsq"),
          type = 'text',
          decimal.mark = ",",
          digit.separator = ".")

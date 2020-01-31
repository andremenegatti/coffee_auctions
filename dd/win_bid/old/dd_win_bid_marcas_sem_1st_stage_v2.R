library(stargazer)
library(tidyverse)
library(PregoesBR)

#### ABRINDO BASES ####
df_ipca <- read_csv('Dados/ipca.csv') %>%
  mutate(inicio_mes = lubridate::ymd(inicio_mes))

controles <- readRDS('controles_futuros_arab_rob.rds') %>%
  mutate(arab_rob = (arab + rob)/2) %>%
  create_time_variables() %>%
  left_join(df_ipca, by = 'inicio_mes') %>%
  mutate(rob_defl = deflacionar(rob, indice_no_periodo = ipca),
         arab_defl = deflacionar(arab, indice_no_periodo = ipca),
         arab_rob_defl = deflacionar(arab_rob, indice_no_periodo = ipca),
         futuro_defl = deflacionar(futuro, indice_no_periodo = ipca))

# Visualizando evolucao precos cafe e futuros
# controles %>%
#   select(arab, rob, arab_rob, futuro, abertura_lances) %>%
#   gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
#   ggplot() +
#   geom_smooth(aes(x = abertura_lances, y = preco, group = variavel, color = variavel))

# Visualizando evolucao precos cafe e futuros DEFLACIONADOS
# controles %>%
#   select(arab_defl, rob_defl, arab_rob_defl, futuro_defl, abertura_lances) %>%
#   gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
#   ggplot() +
#   geom_smooth(aes(x = abertura_lances, y = preco, group = variavel, color = variavel))

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
         mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
                treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
                bimestre = factor(inicio_bimestre), mes = factor(inicio_mes), semana = factor(inicio_semana),
                unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
                marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais))
                )
       ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))

attach(dd_data_list)

#### DD ####
attach(sp_cafe_dd)

# Dummies de bimestre
bim_dd_win_bid <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2)
bim_dd_win_bid_uasg <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + unidade_compradora)
bim_dd_win_bid_marca_uasg <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + unidade_compradora)
bim_dd_win_bid_marca_uasg_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora)
bim_dd_win_bid_marca_uasg_int_futuro <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro)
bim_dd_win_bid_marca_uasg_int_cepea <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + arab_rob)
bim_dd_win_bid_full <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob)
bim_dd_win_bid_full_int <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob + futuro:comprasnet + arab_rob:comprasnet)

# Tendencias especificas para cada grupo
bim_dd_win_bid_marca_uasg_int_futuro_tend <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + bimestre:comprasnet)
bim_dd_win_bid_marca_uasg_int_cepea_tend <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + arab_rob + bimestre:comprasnet)
bim_dd_win_bid_full_int_tend <- lm(win_bid ~ comprasnet + bimestre + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob + futuro:comprasnet + arab_rob:comprasnet + bimestre:comprasnet)


# Dummies de mes
mes_dd_win_bid <- lm(win_bid ~ comprasnet + mes + treat1 + treat2)
mes_dd_win_bid_uasg <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + unidade_compradora)
mes_dd_win_bid_marca_uasg <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + unidade_compradora)
mes_dd_win_bid_marca_uasg_int <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora)
mes_dd_win_bid_marca_uasg_int_futuro <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro)
mes_dd_win_bid_marca_uasg_int_cepea <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + arab_rob)
mes_dd_win_bid_full <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob)
mes_dd_win_bid_full_int <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob + futuro:comprasnet + arab_rob:comprasnet)

# Tendencias especificas para cada grupo
mes_dd_win_bid_marca_uasg_int_futuro_tend <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + mes:comprasnet)
mes_dd_win_bid_marca_uasg_int_cepea_tend <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + arab_rob + mes:comprasnet)
mes_dd_win_bid_full_int_tend <- lm(win_bid ~ comprasnet + mes + treat1 + treat2 + marca_vencedor_principais + marca_vencedor_principais:comprasnet + unidade_compradora + futuro + arab_rob + futuro:comprasnet + arab_rob:comprasnet + mes:comprasnet)


detach(sp_cafe_dd)

stargazer(bim_dd_win_bid,
          bim_dd_win_bid_uasg,
          bim_dd_win_bid_marca_uasg,
          bim_dd_win_bid_marca_uasg_int,
          bim_dd_win_bid_marca_uasg_int_futuro,
          bim_dd_win_bid_marca_uasg_int_cepea,
          bim_dd_win_bid_full,
          bim_dd_win_bid_full_int,
          bim_dd_win_bid_marca_uasg_int_futuro_tend,
          bim_dd_win_bid_marca_uasg_int_cepea_tend,
          bim_dd_win_bid_full_int_tend,
          omit = c("bimestre", "semana", "mes", "unidade_", "marca_", "comprasnet:"),
          omit.stat = c("ser", "adj.rsq", "f"),
          type = 'text',
          decimal.mark = ",",
          digit.separator = ".")

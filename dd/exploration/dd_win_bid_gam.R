library(mgcv)
library(stargazer)
library(PregoesBR)

#### ABRINDO BASES ####
futuros <- readRDS('Dados/futuros.rds') %>%
  select(data, preco_futuros = valor, futuro_media_mensal)

precos_cafe_cepea <- readRDS('Dados/precos_cafe_cepea.rds') %>%
  select(data, pr_arab_reais, pr_rob_reais) %>%
  mutate(media_arab_rob = (pr_arab_reais + pr_rob_reais)/2)

marcas <- readRDS('Marcas/marcas_full.rds') %>%
  select(id_item, marca_vencedor = marca_vencedor_clean2, marca_vencedor_principais)

BEC_cafe <- readRDS('BEC/BEC_cafe_etapa4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances), comprasnet = 0) %>%
  rename(unidade_compradora = UNIDADE_COMPRADORA) %>%
  left_join(precos_cafe_cepea, by = c("abertura_lances" = "data")) %>%
  left_join(futuros, by = c("abertura_lances" = "data")) %>%
  left_join(marcas, by = 'id_item')

cnet_cafe <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))  %>%
  # Renomeando variavel de preco
  rename(win_bid = win_bid_kg) %>%
  left_join(precos_cafe_cepea, by = c("abertura_lances" = "data")) %>%
  left_join(futuros, by = c("abertura_lances" = "data")) %>%
  left_join(marcas, by = 'id_item') %>%
  filter(!is.na(marca_vencedor))

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP')

#### PREPARANDO DADOS PARA ANALISE ####
data_list <- list(BEC_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ trim_df(.x, 'win_bid') %>%
        select(id_item, abertura_lances,
               inicio_ano, inicio_bimestre, inicio_mes, inicio_semana,
               win_bid, unidade_compradora, comprasnet,
               preco_futuros, futuro_media_mensal,
               pr_arab_reais, pr_rob_reais, media_arab_rob,
               marca_vencedor_principais)) %>%
  set_names(c('BEC_cafe', 'cnet_cafe', 'cnet_cafe_sp'))

#### MONTANDO BASES DD ####
dd_data_list <- list(data_list$cnet_cafe, data_list$cnet_cafe_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$BEC_cafe) %>%
        mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
               treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
               bimestre = factor(inicio_bimestre), mes = factor(inicio_mes), semana = factor(inicio_semana),
               unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
               marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais)),
               comprasnet = factor(comprasnet, levels = c("0", "1"), labels = c("BEC", "Comprasnet")),
               treat1 = factor(treat1, levels = c("0", "1"), labels = c("0", "1")),
               treat2 = factor(treat2, levels = c("0", "1"), labels = c("0", "1"))
        )
  ) %>% set_names(c('full_cafe_dd', 'sp_cafe_dd'))


#### GAM DD ####
gam_mod1 <- gam(win_bid ~ bimestre + treat1 + treat2 + comprasnet*marca_vencedor_principais + unidade_compradora + s(media_arab_rob) + preco_futuros,
                data = dd_data_list$full_cafe_dd,
                method = "REML")

gam_mod2 <- gam(win_bid ~ mes + treat1 + treat2 + comprasnet*marca_vencedor_principais + unidade_compradora
                + s(media_arab_rob, by = comprasnet) + s(preco_futuros, by = comprasnet),
                data = dd_data_list$sp_cafe_dd,
                method = "REML")

summary(gam_mod1)
summary(gam_mod2)

# Partial plots
plot(gam_mod1, residuals = TRUE, shift = coef(gam_mod1)[1])


gam.check(gam_mod1)
concurvity(gam_mod1)

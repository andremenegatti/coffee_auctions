library(tidyverse)

# Controles -------------------------------------------------------------------
# Futuros Bloomberg, precos cafe ESALQ
futuros_arab_rob <- readRDS('data/controles_futuros_arab_rob.rds') %>%
  select(abertura_lances, arab_defl, rob_defl, arab_rob_defl, futuro_defl,
         arab_fitted, rob_fitted, arab_rob_fitted, futuro_fitted)

# Marcas
marcas <- readRDS('data/controles_marcas.rds') %>%
  select(id_item, marca_vencedor = marca_vencedor_clean_final,
         marca_vencedor_principais)

# Qualidade
qualidade <- readRDS('data/controles_qualidade.rds') %>%
  select(-descricao)

# Unidades compradoras selecionadas - LASSO
selected_uasgs_list <- 
  map(.x = str_c('data/', c('bec', 'cnet', 'cnet_sp'),
                 '_selected_uasgs_soft_trim.rds'),
      .f = readRDS) %>%
  map(.f = ~ select(.x, id_item, unidade_compradora_lasso = lasso) %>%
        mutate(unidade_compradora_lasso = 
                 as.character(unidade_compradora_lasso))) %>% 
  set_names(c('bec', 'cnet', 'cnet_sp'))

# Dados principais ------------------------------------------------------------
# BEC
bec_cafe <- readRDS('data/bec_cafe.rds') %>%
  rename(abertura_lances = dt_inicio) %>% # <<<<<
  filter(abertura_lances >= '2011-03-01') %>%
  filter(abertura_lances < '2016-01-01') %>% # <<<<
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 0) %>%
  left_join(futuros_arab_rob, by = 'abertura_lances') %>%
  left_join(marcas, by = 'id_item')  %>%
  left_join(qualidade, by = 'id_item') %>%
  left_join(selected_uasgs_list$bec, by = 'id_item') %>% 
  mutate(sigla_uf = 'SP')

# Comprasnet completa
cnet_cafe <- readRDS('data/cnet_cafe.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  filter(abertura_lances < '2016-01-01') %>% # <<<<
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1,
         unidade_compradora = str_extract(id_item, '\\d{6}'))  %>%
  left_join(futuros_arab_rob, by = 'abertura_lances') %>%
  left_join(qualidade, by = 'id_item') %>%
  left_join(marcas, by = 'id_item') %>% 
  left_join(selected_uasgs_list$cnet, by = 'id_item')

# Filtrando SP
cnet_cafe_sp <- cnet_cafe %>%
  filter(sigla_uf == 'SP') %>% 
  select(-unidade_compradora_lasso) %>% 
  left_join(selected_uasgs_list$cnet_sp, by = 'id_item')

# Salvando
saveRDS(bec_cafe, 'data/bec_cafe_dd.rds')
saveRDS(cnet_cafe, 'data/cnet_cafe_dd.rds')
saveRDS(cnet_cafe_sp, 'data/cnet_sp_cafe_dd.rds')
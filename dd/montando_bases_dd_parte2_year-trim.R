library(tidyverse)

# Abrindo bases ---------------------------------------------------------------
bec_cafe <- readRDS('data/bec_cafe_dd.rds') %>% 
  mutate(num_forn_aleat = num_forn_lances) %>% 
  filter(win_bid_kg < 80) # Removendo 2 observações (possíveis erros)

cnet_cafe <- readRDS('data/cnet_cafe_dd.rds') %>% 
  PregoesBR::trim_df('win_bid_kg', 1.5)

cnet_cafe_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_por_unid,
               num_forn_lances, num_forn_aleat,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2, cnpj_fornecedor
        ) %>%
        # Coercing to factor to avoid warnings when joining dataframes
        mutate_if(is.factor, as.character)
  ) %>% set_names(c('bec', 'cnet', 'cnet_sp'))

# Montando bases DD em uma lista ----------------------------------------------
dd_data_list <- list(data_list$cnet, data_list$cnet_sp) %>%
  map(.f = ~ bind_rows(.x, data_list$bec) %>%
        PregoesBR::build_dd_df()
  ) %>% set_names(c('dd_brasil', 'dd_sp'))

# Salvando --------------------------------------------------------------------
saveRDS(dd_data_list$dd_brasil, 'data/dd_brasil.rds')
saveRDS(dd_data_list$dd_sp, 'data/dd_sp.rds')
library(tidyverse)

dd_sp <- readRDS('data/dd_sp.rds') %>% 
  select(-treat1, -treat2, -c(trend_mes:treat2_trend_trimestre))


dd_br <- readRDS('data/dd_brasil.rds') %>% 
  select(-treat1, -treat2, -c(trend_mes:treat2_trend_trimestre))


dd_treat1 <- dd_br %>% 
  filter(abertura_lances < data_3s) %>% 
  mutate(bim = case_when(
    abertura_lances < '2012-03-17' ~ 1,
    abertura_lances < '2012-05-17' ~ 2,
    abertura_lances < '2012-07-17' ~ 3,
    abertura_lances < '2012-09-17' ~ 4,
    abertura_lances < '2012-11-17' ~ 5,
    abertura_lances < '2013-01-17' ~ 6,
    abertura_lances < '2013-03-17' ~ 7,
    abertura_lances < '2013-05-17' ~ 8,
    abertura_lances < '2013-07-17' ~ 9,
    abertura_lances < '2013-09-17' ~ 10,
    abertura_lances < '2013-11-17' ~ 11,
    abertura_lances < data_3s ~ 12,
    )) %>% 
  mutate(trim = case_when(
    abertura_lances < '2012-03-17' ~ 1,
    abertura_lances < '2012-06-17' ~ 2,
    abertura_lances < '2012-09-17' ~ 3,
    abertura_lances < '2012-12-17' ~ 4,
    abertura_lances < '2012-03-17' ~ 5,
    abertura_lances < '2013-05-17' ~ 6,
    abertura_lances < '2013-09-17' ~ 7,
    abertura_lances < '2013-12-17' ~ 8,
  )) %>% 
  mutate(bim_factor = as.character(bim) %>% 
           fct_relevel(as.character(5))) %>% 
  mutate(cnet_X_bim_pre_01 = comprasnet * (bim == 1),
         cnet_X_bim_pre_02 = comprasnet * (bim == 2),
         cnet_X_bim_pre_03 = comprasnet * (bim == 3),
         cnet_X_bim_pre_04 = comprasnet * (bim == 4)) %>% 
  mutate(cnet_X_bim_post_06 = comprasnet * (bim == 6),
         cnet_X_bim_post_07 = comprasnet * (bim == 7),
         cnet_X_bim_post_08 = comprasnet * (bim == 8),
         cnet_X_bim_post_09 = comprasnet * (bim == 9),
         cnet_X_bim_post_10 = comprasnet * (bim == 10),
         cnet_X_bim_post_11 = comprasnet * (bim == 11),
         cnet_X_bim_post_12 = comprasnet * (bim == 12))


dd_m1 <- dd_treat1 %>% 
  select(log_win_bid, starts_with("cnet_X_bim"),
         bim_factor, arab_defl, futuro_defl,
         qualidade, kg_por_unid,
         unidade_compradora, municipio,
         marca_vencedor_principais, sigla_uf)

form <- str_c('log_win_bid ~ ',
              str_c(names(dd_m1)[2:12],
              'bim_factor', 'arab_defl', 'futuro_defl', 'qualidade', collapse = ' + ')) %>% 
  str_c(' | unidade_compradora + municipio + sigla_uf:bim_factor')


lfe::felm(as.formula(form), data = dd_m1)

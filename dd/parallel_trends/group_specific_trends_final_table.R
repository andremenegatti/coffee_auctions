library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = ~ readRDS(.x) %>% filter(!is.na(num_forn_lances))) %>%
  set_names(c('dd_brasil', 'dd_sp'))

# Estimating DD with group-specific linear trends -----------------------------
# log win bid
trends_sp_log_win_bid <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp)

trends_brasil_log_win_bid <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_data_list$dd_brasil)

# n bidders
trends_sp_n_bidders <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp)

trends_brasil_n_bidders <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_data_list$dd_brasil)

# Salvando tabelas de resultados
stargazer(trends_brasil_log_win_bid, trends_sp_log_win_bid,
          trends_brasil_n_bidders, trends_sp_n_bidders,
          type = 'text',
          out = 'results/group_trends/trends.txt')

stargazer(trends_brasil_log_win_bid, trends_sp_log_win_bid,
          trends_brasil_n_bidders, trends_sp_n_bidders,
          out = 'results/group_trends/trends_latex.tex',
          decimal.mark = ',', digit.separator = '.')

library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = readRDS) %>% set_names(c('dd_brasil', 'dd_sp'))

# Estimating DD with group-specific linear trends -----------------------------
trends_sp <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp)

trends_brasil <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_data_list$dd_brasil)

# Salvando tabelas de resultados
stargazer(trends_brasil, trends_sp,
          type = 'text',
          out = 'results/log_win_bid/group_trends/trends.txt')

stargazer(trends_brasil, trends_sp,
          out = 'results/log_win_bid/group_trends/trends_latex.tex',
          decimal.mark = ',', digit.separator = '.')

# HC1 SE ----------------------------------------------------------------------
trends_sp_hc1 <- 
  lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre +
       bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_data_list$dd_sp) %>% 
  PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
      )
    )

trends_brasil_hc1 <- 
  lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + 
       bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
     data = dd_data_list$dd_brasil) %>% 
  PregoesBR::get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

# Salvando dados --------------------------------------------------------------
list('trends_sp_hc1' = trends_sp_hc1,
     'trends_brasil_hc1' = trends_brasil_hc1) %>% 
  saveRDS('results/log_win_bid/group_trends/trends_hc1_results.rds')

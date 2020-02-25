library(stargazer)
library(tidyverse)
library(lfe)

# Abrindo bases ---------------------------------------------------------------
# Principais
bec_cafe <- readRDS('data/bec_cafe_dd.rds')
cnet_cafe <- readRDS('data/cnet_cafe_dd.rds')
cnet_cafe_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        PregoesBR::trim_df('win_bid_kg', perc = 2.5) %>% # <<<<
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_por_unid, 
               kg_fornecidos, num_forn_lances,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               futuro_fitted, arab_rob_fitted, arab_fitted, rob_fitted,
               qualidade, qualidade2) %>%
        mutate_if(is.factor, as.character) 
  ) %>% set_names(c('bec', 'cnet', 'cnet_sp'))

# Model specifications --------------------------------------------------------
form <- 'log_win_bid ~ comprasnet + treat1 + treat2'

df_models <- tibble(
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora'),
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade                             | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl   | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + arab_defl + comprasnet:trend_bimestre | bimestre + unidade_compradora + municipio + marca_vencedor_principais')))

# Fitting DD models -----------------------------------------------------------
for (i in seq(10, 69, by = 1)) {
  
  message(i)
  
  top100_uasgs_list <- data_list %>% 
    map(.f = ~ group_by(.x, unidade_compradora) %>% 
          summarise(valor_negociado = sum(kg_fornecidos * win_bid_kg,
                                          na.rm = TRUE)) %>% 
          ungroup() %>% 
          arrange(desc(valor_negociado)) %>% 
          slice(1:i) %>% 
          right_join(.x, by = 'unidade_compradora') %>% 
          filter(!is.na(valor_negociado)) %>% 
          select(-valor_negociado))
  
  # Montando bases DD em uma lista ----------------------------------------------
  dd_data_list <- list(top100_uasgs_list$cnet, top100_uasgs_list$cnet_sp) %>%
    map(.f = ~ bind_rows(.x, top100_uasgs_list$bec) %>%
          PregoesBR::build_dd_df()
    ) %>% set_names(c('dd_brasil', 'dd_sp'))
  
  # DD SP -----------------------------------------------------------------------
  df_fitted_models <- df_models %>% 
    mutate(models = map(.x = formula,
                        .f = ~ felm(as.formula(.x),
                                    data = dd_data_list$dd_sp))) # <<<<
  
  stargazer(df_fitted_models$models, type = 'text',
            out = str_c('dd/top_uasgs/valor_negociado/sp/txt/top', i, '.txt'))
  
  # HC1 SE
  lm_sp <- lm(log_win_bid ~ comprasnet + treat1 + treat2 + arab_defl + 
                kg_por_unid + qualidade + bimestre + municipio + 
                unidade_compradora + marca_vencedor_principais,
              data = dd_data_list$dd_sp)
  
  df_std_sp <- PregoesBR::get_robust_std_errors(lm_sp, HC = 'HC1')
  
  saveRDS(df_std_sp,
          str_c('dd/top_uasgs/valor_negociado/sp/hc1/top', i, '.rds'))
  
}

# Loading results -------------------------------------------------------------
# Vector used to define which results to import
top_seq <- seq(10, 69, by = 1)

# Loading dataframes with results
rob_est_list <- map(.x = top_seq,
                    .f = ~ str_c('dd/top_uasgs/valor_negociado/sp/hc1/top',
                                 .x, '.rds') %>% 
                      readRDS()) %>% set_names(str_c('top', top_seq))

# Binding treatment effects into a single df, calculating CI
rob_est_df <- rob_est_list %>% 
  map(.f = ~ slice(.x, 3:4)) %>% 
  bind_rows(.id = 'top_uasgs') %>% 
  mutate(no_uasgs = str_remove(top_uasgs, 'top') %>% as.numeric(),
         ci_lower_bound = estimate - 2 * std_error,
         ci_upper_bound = estimate + 2 * std_error)

# Lineplot: efeito relativo, apenas 3s ----------------------------------------
rob_est_df %>% 
  filter(coef == 'treat2') %>% 
  filter(no_uasgs < 41) %>%  # <<<<
  ggplot(aes(x = no_uasgs, y = estimate / 0.122)) +
  geom_line(color = 'black') +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = 'Número de unidades compradoras selecionadas',
       y = 'Coeficiente estimado / coeficiente do modelo principal',
       title = 'Efeito da Regra dos 3s nas principais unidades compradoras',
       subtitle = 'Unidades compradoras de SP que mais realizaram leilões',
       caption = 'Notas:
       1) O eixo vertical representa a razão entre o coeficiente estimado 
           para os conjuntos das principais unidades compradoras 
           e o resultado da amostra completa de SP (0,122).
       2) As unidades compradoras de cada grupo (tratamento e controle) foram ordenadas 
           segundo o número de leilões realizados entre mar./2011 e dez./2012.
           Para cada valor do eixo horizontal, rodou-se uma regressão considerando
           apenas os leilões das unidades compradoras com ranking igual ou superior.')

# ggsave('plots/lineplot_efeito_3s_top_uasgs.png', width = 6, height = 7)

# Lineplot: efeito estimado Regra 3s, com CI -------------------------------------------
rob_est_df %>% 
  filter(coef == 'treat2') %>% 
  filter(no_uasgs < 41) %>% # <<<<
  ggplot(aes(x = no_uasgs, y = estimate)) +
  geom_line(color = 'black') +
  geom_ribbon(aes(ymin = ci_lower_bound, ymax = ci_upper_bound),
              fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0)

# Lineplot: efeito relativo, ambos os tratamentos -----------------------------
rob_est_df %>% 
  filter(no_uasgs < 41) %>%  # <<<<
  mutate(estimate = ifelse(coef == 'treat2',
                           estimate / 0.122,
                           estimate / 0.04))  %>%
  mutate(coef = ifelse(coef == 'treat1',
                       'Regra 20s',
                       'Regra 20s + Regra 3s')) %>% 
  ggplot(aes(x = no_uasgs, y = estimate)) +
  geom_line(color = 'black') +
  scale_y_continuous(labels = PregoesBR::formatar_numero) +
  labs(x = 'Número de unidades compradoras selecionadas',
       y = 'Coeficiente estimado / coeficiente do modelo principal',
       title = 'Efeito nas principais unidades compradoras',
       subtitle = 'Unidades compradoras de SP que mais realizaram leilões') +
  facet_wrap(~ coef, nrow = 1)

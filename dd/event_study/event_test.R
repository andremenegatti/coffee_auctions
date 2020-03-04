library(tidyverse)
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
               win_bid_kg, quantidade, kg_por_unid,
               num_forn_lances, comprasnet, sigla_uf, municipio, 
               unidade_compradora, unidade_compradora_lasso,
               marca_vencedor_principais,
               futuro_defl, arab_rob_defl, arab_defl, rob_defl,
               qualidade, qualidade2, cnpj_fornecedor) %>% 
        mutate(log_win_bid = log(win_bid_kg)) %>%
        mutate_if(is.factor, as.character)
  ) %>% set_names(c('bec', 'cnet', 'cnet_sp'))

df_br <- bind_rows(data_list$cnet, data_list$bec)
df_sp <- bind_rows(data_list$cnet_sp, data_list$bec)

df_br <- df_br %>% 
  mutate(sigla_uf = as.factor(sigla_uf)) %>% 
  mutate(trim = case_when(
    # abertura_lances < '2011-03-01' ~ 1,
    abertura_lances < '2011-06-10' ~ 2,
    abertura_lances < '2011-09-20' ~ 3,
    abertura_lances < '2012-01-17' ~ 4, # <<<
    abertura_lances < '2012-04-01' ~ 5,
    abertura_lances < '2012-07-01' ~ 6,
    abertura_lances < '2012-10-01' ~ 7,
    abertura_lances < '2013-01-01' ~ 8,
    abertura_lances < '2013-04-01' ~ 9,
    abertura_lances < '2013-07-01' ~ 10,
    abertura_lances < '2013-10-07' ~ 11, # <<<
    abertura_lances < '2014-01-02' ~ 12, # <<<
    abertura_lances < '2014-04-01' ~ 13,
    abertura_lances < '2014-07-01' ~ 14, 
    abertura_lances < '2014-10-01' ~ 15, 
    abertura_lances < '2015-01-01' ~ 16,
    abertura_lances < '2015-04-01' ~ 17,
    abertura_lances < '2015-07-01' ~ 18,
    abertura_lances < '2015-10-01' ~ 19,
    abertura_lances < '2015-12-31' ~ 20 
  )) %>% 
  mutate(trim_factor = as.character(trim) %>% 
           fct_relevel(as.character(2))) %>% 
  mutate(cnet_X_trim_pre_01 = comprasnet * (trim == 1),
         cnet_X_trim_pre_02 = comprasnet * (trim == 2),
         cnet_X_trim_pre_03 = comprasnet * (trim == 3),
         cnet_X_trim_pre_04 = comprasnet * (trim == 4)) %>% 
  mutate(cnet_X_trim_post_06 = comprasnet * (trim == 6),
         cnet_X_trim_post_07 = comprasnet * (trim == 7),
         cnet_X_trim_post_08 = comprasnet * (trim == 8),
         cnet_X_trim_post_09 = comprasnet * (trim == 9),
         cnet_X_trim_post_10 = comprasnet * (trim == 10),
         cnet_X_trim_post_11 = comprasnet * (trim == 11),
         cnet_X_trim_post_12 = comprasnet * (trim == 12),
         cnet_X_trim_post_13 = comprasnet * (trim == 13),
         cnet_X_trim_post_14 = comprasnet * (trim == 14),
         cnet_X_trim_post_15 = comprasnet * (trim == 15),
         cnet_X_trim_post_16 = comprasnet * (trim == 16),
         cnet_X_trim_post_17 = comprasnet * (trim == 17),
         cnet_X_trim_post_18 = comprasnet * (trim == 18),
         cnet_X_trim_post_19 = comprasnet * (trim == 19),
         cnet_X_trim_post_20 = comprasnet * (trim == 20))

df_m1 <- df_br %>% 
  select(log_win_bid, abertura_lances,
         starts_with("cnet_X_trim"),
         trim_factor, arab_defl, futuro_defl,
         qualidade, kg_por_unid,
         unidade_compradora, municipio,
         marca_vencedor_principais, sigla_uf)

model <- lfe::felm(log_win_bid ~ 
                     # cnet_X_trim_pre_01 +
                     cnet_X_trim_pre_02 +
                     cnet_X_trim_pre_03 +
                     cnet_X_trim_pre_04 +
                     cnet_X_trim_post_06 +
                     cnet_X_trim_post_07 +
                     cnet_X_trim_post_08 +
                     cnet_X_trim_post_09 +
                     cnet_X_trim_post_10 +
                     cnet_X_trim_post_11 +
                     cnet_X_trim_post_12 +
                     cnet_X_trim_post_13 +
                     cnet_X_trim_post_14 +
                     cnet_X_trim_post_15 +
                     cnet_X_trim_post_16 +
                     cnet_X_trim_post_17 +
                     cnet_X_trim_post_18 +
                     cnet_X_trim_post_19 +
                     cnet_X_trim_post_20 +
                     qualidade + arab_defl + futuro_defl |
                     trim_factor +
                     unidade_compradora + municipio + sigla_uf,
                   data = df_m1)

stargazer::stargazer(model, type = 'text')

lm_model <- lm(log_win_bid ~ 
                 # cnet_X_trim_pre_01 +
                 cnet_X_trim_pre_02 +
                 cnet_X_trim_pre_03 +
                 cnet_X_trim_pre_04 +
                 cnet_X_trim_post_06 +
                 cnet_X_trim_post_07 +
                 cnet_X_trim_post_08 +
                 cnet_X_trim_post_09 +
                 cnet_X_trim_post_10 +
                 cnet_X_trim_post_11 +
                 cnet_X_trim_post_12 +
                 cnet_X_trim_post_13 +
                 cnet_X_trim_post_14 +
                 cnet_X_trim_post_15 +
                 cnet_X_trim_post_16 +
                 cnet_X_trim_post_17 +
                 cnet_X_trim_post_18 +
                 cnet_X_trim_post_19 +
                 cnet_X_trim_post_20 +
                 qualidade + arab_defl + futuro_defl +
                 trim_factor +
                 unidade_compradora + municipio + sigla_uf,
               data = df_m1)

df_se <- PregoesBR::get_robust_std_errors(lm_model)

trim_dict <- read_csv('dd/event/trim_dict_event.csv')
df_se <- readRDS('dd/event/data/df_se.rds')


df_se %>% 
  filter(str_detect(coef, 'cnet_X_trim')) %>% 
  mutate(t = str_match(coef, '\\d{1,2}')[, 1] %>% 
           as.numeric()) %>% 
  mutate(ci_lower = estimate - 2*std_error,
         ci_upper = estimate + 2*std_error) %>% 
  left_join(trim_dict, by = c('t' = 'trim')) %>% 
  ggplot(aes(x = abertura_lances, y = estimate)) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'darkred') +
  geom_line(aes(group = 1)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), col = 'gray', alpha = 0.2) +
  geom_vline(xintercept = data_3s, linetype = 'dotted') +
  geom_label(x = data_3s, y = -0.15, label = 'Implementação\nRegra 3s', family = 'serif', size = 3) +
  scale_x_date(breaks = trim_dict$abertura_lances) +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
  labs(
    x = 'Trimestre',
    y = 'Variação % em relação ao baseline',
    title = 'Event plot'
  )
  

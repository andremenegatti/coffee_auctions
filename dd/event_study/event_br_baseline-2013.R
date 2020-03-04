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


trim_dict <- read_csv('dd/event/trim_dict_event3.csv')
trim_dict

df_br2 <- df_br %>% 
  mutate(sigla_uf = as.factor(sigla_uf)) %>% 
  add_custom_trimesters(trim_dict, baseline = 7:9) %>%
  add_post_interactions(1:6) %>% 
  add_post_interactions(8:13) %>% 
  mutate(trim_factor = as.factor(trim) %>% 
           fct_relevel('6'))

df_br2 %>% glimpse()

df_dates <- df_br2 %>% group_by(trim) %>% 
  summarize(date_first_auction = min(abertura_lances),
            date_last_auction = max(abertura_lances),
            avg_date = mean(abertura_lances),
            n = n()) ; df_dates

library(lubridate)
df_m1 <- df_br2 %>% 
  select(log_win_bid, abertura_lances,
         starts_with("cnet_X_trim"),
         trim_factor, arab_defl, futuro_defl,
         qualidade, kg_por_unid,
         unidade_compradora, municipio,
         marca_vencedor_principais, sigla_uf) %>% 
  PregoesBR::create_time_variables() %>% 
  mutate(mes = as.factor(inicio_mes),
         bimestre = as.factor(inicio_bimestre))


model <- lfe::felm(log_win_bid ~ 
                     cnet_X_trim_post_01 +
                     cnet_X_trim_post_02 +
                     cnet_X_trim_post_03 +
                     cnet_X_trim_post_04 +
                     cnet_X_trim_post_05 +
                     cnet_X_trim_post_06 +
                     cnet_X_trim_post_08 +
                     cnet_X_trim_post_09 +
                     cnet_X_trim_post_10 +
                     cnet_X_trim_post_11 +
                     cnet_X_trim_post_12 +
                     cnet_X_trim_post_13 +
                     qualidade + arab_defl + futuro_defl |
                     trim_factor + unidade_compradora + 
                     municipio + sigla_uf + mes,
                   data = df_m1)

stargazer::stargazer(model, type = 'text')

lm_model <- 
  lm(log_win_bid ~ 
       cnet_X_trim_post_01 +
       cnet_X_trim_post_02 +
       cnet_X_trim_post_03 +
       cnet_X_trim_post_04 +
       cnet_X_trim_post_05 +
       cnet_X_trim_post_06 +
       cnet_X_trim_post_08 +
       cnet_X_trim_post_09 +
       cnet_X_trim_post_10 +
       cnet_X_trim_post_11 +
       cnet_X_trim_post_12 +
       cnet_X_trim_post_13 +
       qualidade + arab_defl + futuro_defl +
       trim_factor + unidade_compradora + 
       municipio + sigla_uf + mes,
     data = df_m1)

df_se <- PregoesBR::get_robust_std_errors(lm_model)

df_plot <- df_se %>% 
  filter(str_detect(coef, 'cnet_X_trim')) %>% 
  mutate(t = str_match(coef, '\\d{1,2}')[, 1] %>% 
           as.numeric()) %>% 
  mutate(ci_lower = estimate - 2*std_error,
         ci_upper = estimate + 2*std_error) %>% 
  left_join(df_dates, by = c('t' = 'trim'))


df_plot %>% 
  ggplot(aes(x = date_first_auction, y = estimate)) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'darkred') +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              col = 'gray', alpha = 0.2) +
  # geom_vline(xintercept = data_3s, linetype = 'dotted') +
  # geom_label(x = data_3s, y = -0.15, label = 'Implementação\nRegra 3s',
  # family = 'serif', size = 3) +
  scale_x_date(breaks = df_plot$date_first_auction) +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
  labs(
    x = 'Trimestre',
    y = 'Variação % em relação ao baseline',
    title = 'Event plot'
  )

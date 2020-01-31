
df_bid_inc <- readRDS('Comprasnet/cnet_cafe_07_1_df_bid_inc_nested_v4.rds')

#### LANCES INTERMEDIARIOS ####

df_bid_inc <- df_bid_inc %>%
  mutate(int_ratio = map2_dbl(.x = bid_increments, .y = bid_increments_non_negative,
                              .f = ~ nrow(.y)/nrow(.x)))

# Pequeno aumento da proporcao de lances intermediarios apos a regra dos 3s
# Significante a 5%, mas magnitude pequena (1,5 p.p.)
df_bid_inc %>%
  get_summary_stats(int_ratio, regime_juridico_3s)

summary(m1 <- lm(int_ratio ~ regime_juridico_3s, data = df_bid_inc))


#### VARIANCIA ENTRE LANCES DE UM MESMO FORNECEDOR ####

df_sd <- df_bid_inc_unnested %>%
  group_by(id_item, CNPJ_CPF) %>%
  summarise(sd_inc = sd(norm_inc_first),
            n = n()) %>%
  ungroup() %>%
  arrange(sd_inc)

df_sd <- df_sd %>%
  left_join(df_bid_inc %>% select(id_item, regime_juridico, regime_juridico_3s), by = 'id_item') %>%
  filter(n > 0)

df_sd %>%
  get_summary_stats(sd_inc, regime_juridico)

df_zero_var <- df_sd %>%
  filter(sd_inc == 0) %>%
  arrange(desc(n))

df_zero_var %>%
  count(regime_juridico)

zero_var_companies <- df_zero_var$CNPJ_CPF
zero_var_ids <- df_zero_var$id_item

df_bid_inc_zero_var <- df_bid_inc %>%
  filter(id_item %in% zero_var_ids)

(df_bid_inc_zero_var %>% filter(id_item ==  "25500300000520140003"))$bid_increments[[1]] %>% View()

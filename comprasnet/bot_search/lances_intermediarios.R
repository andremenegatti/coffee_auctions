library(tidyverse)

df_bid_inc <- readRDS('data/cnet_bid_increments.rds')

# Lances Intermediários -------------------------------------------------------
df_bid_inc <- df_bid_inc %>%
  mutate(int_ratio = map2_dbl(.x = bid_increments,
                              .y = bid_increments_non_negative,
                              .f = ~ nrow(.y)/nrow(.x)))

# Pequeno aumento da proporcao de lances intermediarios apos a regra dos 3s
# Significante a 5%, mas magnitude pequena (1,5 p.p.)
df_bid_inc %>%
  PregoesBR::get_summary_stats(int_ratio, regime_juridico_3s)

summary(m1 <- lm(int_ratio ~ regime_juridico_3s, data = df_bid_inc))
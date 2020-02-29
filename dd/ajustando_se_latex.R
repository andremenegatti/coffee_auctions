library(tidyverse)

results <- readRDS('results/main_results_sp.rds')

treat_var <- 'treat1'

# SE
map_dbl(.x = results$hc1_se,
        .f = ~ filter(.x, coef == treat_var) %>%
          pull(std_error)) %>% 
  round(digits = 4)

# Significance
map_chr(.x = results$hc1_se,
        .f = ~ filter(.x, coef == treat_var) %>%
          pull(significance))

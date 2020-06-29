library(tidyverse)

results <- readRDS('results/n_bidders/main_results_brasil.rds')

treat_var <- 'comprasnet'

# Checking coefficient estimate
map_dbl(.x = results$hc1_se,
        .f = ~ filter(.x, coef == treat_var) %>%
          pull(estimate)) %>% 
  round(digits = 3)

# SE
map_dbl(.x = results$hc1_se,
        .f = ~ filter(.x, coef == treat_var) %>%
          pull(std_error)) %>% 
  round(digits = 3)

# Significance
map_chr(.x = results$hc1_se,
        .f = ~ filter(.x, coef == treat_var) %>%
          pull(significance))

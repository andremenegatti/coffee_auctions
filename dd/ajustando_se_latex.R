library(tidyverse)

results <- readRDS('results/n_bidders/main_results_sp.rds')

for (treat_var in c(
  'treat1',
  'treat2'
  #'(Intercept)'
  # 'qualidadeDESCAFEINADO',
  # 'qualidadeGOURMET',
  # 'qualidadeSUPERIOR',
  # 'kg_por_unid_0.25',
  # 'kg_por_unid_1'
  # 'kg_por_unid_5'
)){
  
  message(treat_var)
  
  message('Coefficient:')
  map_dbl(.x = results$hc1_se,
          .f = ~ filter(.x, coef == treat_var) %>%
            pull(estimate)) %>% 
    round(digits = 3) %>% 
    print()
  
  message('SE:')
  map_dbl(.x = results$hc1_se,
          .f = ~ filter(.x, coef == treat_var) %>%
            pull(std_error)) %>% 
    round(digits = 3) %>% 
    print()
  
  message('Significance:')
  map_chr(.x = results$hc1_se,
          .f = ~ filter(.x, coef == treat_var) %>%
            pull(significance)) %>% 
    print()
  
  message('-------------------------------------')
  message('')
}

treat_var <- 'qualidadeDESCAFEINADO'

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

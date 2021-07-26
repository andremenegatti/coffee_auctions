library(tidyverse)

# Abrindo bases ---------------------------------------------------------------
dd_data_list <- c('ajustes-marcas/dd_brasil_main15.rds', 'ajustes-marcas/dd_sp_main15.rds') %>% 
  map(.f = ~ readRDS(.x) %>% filter(!is.na(num_forn_lances))) %>%
  set_names(c('dd_brasil', 'dd_sp'))

count_marcas <- dd_data_list$dd_sp %>%
  count(comprasnet, marca_vencedor) %>%
  arrange(desc(n)) %>% 
  mutate(plataforma = ifelse(comprasnet == 0, 'BEC', 'Comprasnet')) %>% 
  select(-comprasnet) %>% 
  complete(marca_vencedor, plataforma, fill = list(n = 0)) %>% 
  mutate(marca_vencedor = str_remove(marca_vencedor, '^marca_'))

count_marcas_wide <- count_marcas %>% 
  pivot_wider(names_from = plataforma, values_from = n) %>% 
  mutate(Total = BEC + Comprasnet) %>% 
  arrange(desc(Total))

count_cnet_sp <- count_marcas_wide %>% 
  select(marca_vencedor, Comprasnet) %>% 
  arrange(desc(Comprasnet)) ; count_cnet_sp

count_bec <- count_marcas_wide %>% 
  select(marca_vencedor, BEC) %>% 
  arrange(desc(BEC))

count_cnet_sp %>% 
  write_excel_csv2('ajustes-marcas/tabela-contagem-marcas-comprasnet-sp-main15.csv')

count_bec %>% 
  write_excel_csv2('ajustes-marcas/tabela-contagem-marcas-bec-main15.csv')


# Representatividade 10 principais CNET SP
total_main_cnet_sp <- count_cnet_sp %>% 
  filter(marca_vencedor != 'Outra') %>% 
  head(15) %>% 
  pull(Comprasnet) %>% 
  sum()

n_cnet <- dd_data_list$dd_sp %>% 
  filter(comprasnet == 1) %>% 
  nrow()

total_main_cnet_sp / n_cnet

# Representatividade 10 principais BEC
total_main_bec <- count_bec %>% 
  filter(marca_vencedor != 'Outra') %>% 
  head(15) %>% 
  pull(BEC) %>% 
  sum()

n_bec <- dd_data_list$dd_sp %>% 
  filter(comprasnet == 0) %>% 
  nrow()

total_main_bec / n_bec

library(tidyverse)

# Loading data ----------------------------------------------------------------
data_list <- c('data/dd_brasil.rds', 'data/dd_sp.rds') %>% 
  map(.f = ~ readRDS(.x) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(regime_juridico = case_when(
          abertura_lances < data_20s ~ 1,
          abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
          abertura_lances >= data_3s ~ 3
        ) %>% as.factor())) %>% set_names(c('dd_brasil', 'dd_sp'))

bec <- data_list$dd_brasil %>% filter(comprasnet == 0)
cnet <- data_list$dd_brasil %>% filter(comprasnet == 1)
cnet_sp <- data_list$dd_sp %>% filter(comprasnet == 1)

# Summary statistics ----------------------------------------------------------
summary_table <- bec %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(cnet %>% mutate(Grupo = "Tratamento")) %>%
  bind_rows(cnet_sp %>% mutate(Grupo = "Tratamento - SP")) %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(win_bid_kg, na.rm = TRUE),
            Mediana = median(win_bid_kg, na.rm = TRUE),
            SD = sd(win_bid_kg, na.rm = TRUE)) ; summary_table

summary_table %>% 
  write.csv2('dd/descricao_amostra/tabela_descritiva_win_bid.csv',
             row.names = FALSE)

library(tidyverse)

cnet_cafe <- readRDS('data/cnet_cafe.rds') %>% 
  filter(abertura_lances >= '2011-03-01',
         abertura_lances < '2016-01-01')

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < lubridate::ymd('2016-01-01')) %>% 
  left_join(cnet_cafe %>% 
              select(id_item, inicio_fase_aleatoria),
            by = 'id_item')
  
df_vencedor <- cnet_cafe %>% 
  select(id_item, propostas, vencedor) %>% 
  mutate(cnpj_vencedor = map2_chr(.x = propostas,
                                  .y = vencedor,
                                  .f = ~ .x %>% 
                                    filter(str_detect(fornecedor, .y)) %>% 
                                    select(CNPJ_CPF) %>% unlist()))

df_lances2 <- df_lances %>% 
  left_join(df_vencedor %>% select(id_item, cnpj_vencedor), by = 'id_item') %>% 
  group_by(id_item, CNPJ_CPF) %>% 
  mutate(n_lances = n(),
         n_lances_random = sum(ifelse(data_hora >= inicio_fase_aleatoria,
                                      1, 0))) %>% 
  ungroup() %>% 
  group_by(id_item) %>%
  arrange(desc(n_lances)) %>% 
  mutate(cnpj_1st_bidder = CNPJ_CPF[1],
         cnpj_2nd_bidder = CNPJ_CPF[2]) %>% 
  arrange(desc(n_lances_random)) %>% 
  mutate(cnpj_1st_bidder_random = CNPJ_CPF[1],
         cnpj_2nd_bidder_random = CNPJ_CPF[2]) %>% 
  ungroup() %>% 
  mutate(
    win_1st_bidder = ifelse(cnpj_1st_bidder == cnpj_vencedor, 1, 0),
    win_2nd_bidder = ifelse(cnpj_2nd_bidder == cnpj_vencedor, 1, 0),
    win_1st_bidder_random = ifelse(cnpj_1st_bidder_random == cnpj_vencedor, 1, 0),
    win_2nd_bidder_random = ifelse(cnpj_2nd_bidder_random == cnpj_vencedor, 1, 0),
    win_1st_or_2nd = ifelse(cnpj_vencedor %in% c(cnpj_2nd_bidder, cnpj_1st_bidder), 1, 0),
    win_1st_or_2nd_random = ifelse(cnpj_vencedor %in% c(cnpj_2nd_bidder_random, cnpj_1st_bidder_random), 1, 0))


df_lances2 %>% 
  group_by(regime_juridico) %>% 
  summarise(avg_win_1st_bidder = mean(win_1st_bidder),
            avg_win_1st_bidder_random = mean(win_1st_bidder_random),
            avg_win_2nd_bidder = mean(win_2nd_bidder),
            avg_win_2st_bidder_random = mean(win_2nd_bidder_random),
            avg_1st_or_2nd = mean(win_1st_or_2nd),
            avg_1st_or_2nd_random = mean(win_1st_or_2nd_random))

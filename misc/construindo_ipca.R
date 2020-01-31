library(tidyverse)

ipca_new <-
  data.frame(
    inicio_mes = str_c(2017,
                       str_pad(string = 1:12, width = 2,
                               side = 'left', pad = 0),
                       '01', sep = '-') %>% as.Date(),
                       ipca = c(4793.85,
                               4809.67,
                               4821.69,
                               4828.44,
                               4843.41,
                               4832.27,
                               4843.87,
                               4853.07,
                               4860.83,
                               4881.25,
                               4894.92,
                               4916.46))

ipca2 <- bind_rows(ipca, ipca_new)

ipca3 <- ipca2 %>%
  mutate(ipca = as.character(ipca))

write.csv(ipca3, file = 'ipca.csv')

library(tidyverse)
library(lubridate)

for (i in 1:3) {
  filename <- str_c('data/bec_dados_parte', i, '.rds')
  message(paste0('Reading ', filename, '...'))

  bec_fornecedores <- readRDS(filename) %>%
    select(id_pregao = OC,
           fornecedores = DESC_ATA_GERADAPR.OCCompleta.Fornecedores)

  saveRDS(bec_fornecedores,
          str_c('data/bec_fornecedores_parte', i, '.rds'))
}

library(jsonlite)
library(tidyverse)

# Desativando notacao cientifica para evitar erros nos links
options(scipen = 999)

# Criando lista de links
base_url <- "http://compras.dados.gov.br/licitacoes/doc/uasg/"

# Construindo lista de UASGs a partir dos dados gerais dos pregoes
df_dados_gerais <-
  readRDS('~/Documents/WebScrapingPregoes/Comprasnet/Dados/COMPRASNET_dados_gerais.rds')

uasgs <- df_dados_gerais %>%
  mutate(co_uasg = str_extract(id_pregao, '\\d{6}')) %>%
  select(co_uasg) %>%
  distinct() %>%
  unlist()

# Criando dataframe que recebera os dados
dados_uasgs <- tibble()

# Iniciando web scraping ------------------------------------------------------
# Iterando entre as paginas
for (i in 1:length(uasgs)) {

  message(str_c(i, length(uasgs), sep = '/'))

  json_uasg <- str_c(base_url, uasgs[i], '.json') %>%
    fromJSON()

  json_uasg$`_links` <- NULL

  df_uasg <- t(unlist(json_uasg)) %>%
    as_tibble()

  dados_uasgs <- bind_rows(dados_uasgs, df_uasg)

}

# Incluindo manualmente dados da uasg 988771
dados_uasgs$id_municipio[dados_uasgs$id == '988771'] <- '87718' # Novo Hamburgo
dados_uasgs$sigla_uf[dados_uasgs$id == '988771'] <- 'RS'

saveRDS(dados_uasgs, 'comprasnet/municipios_uasgs/dados_uasgs.rds')

library(jsonlite)
library(tidyverse)

# Desativando notacao cientifica para evitar erros nos links
options(scipen = 999)

# Criando lista de links
base_url <- "http://compras.dados.gov.br/fornecedores/doc/municipio/"

# Construindo lista de ids de municipios a partir dos dados de uasgs
dados_uasgs <- readRDS('Comprasnet/dados_uasgs.rds')

municipios <- dados_uasgs %>%
  select(id_municipio) %>%
  distinct() %>%
  unlist()

len_municipios <- length(municipios)

# Criando dataframe que recebera os dados
dados_municipios <- tibble()

# Iniciando web scraping ------------------------------------------------------
# Iterando entre as paginas
for (i in 445:len_municipios) {

  message(str_c(i, len_municipios, sep = '/'))

  json_mun <- str_c(base_url, municipios[i], '.json') %>%
    fromJSON()

  json_mun$`_links` <- NULL
  json_mun$ativo <- NULL

  df_municipios <- t(unlist(json_mun)) %>%
    as_tibble()

  dados_municipios <- bind_rows(dados_municipios, df_municipios)

}

substituir_caracteres_especiais <- function(x) {
  toupper(x) %>%
    str_replace_all('Á', 'A') %>%
    str_replace_all('Ó', 'O') %>%
    str_replace_all('É', 'E') %>%
    str_replace_all('Ã', 'A') %>%
    str_replace_all('Õ', 'O') %>%
    str_replace_all('Ç', 'C') %>%
    str_replace_all('Í', 'I') %>%
    str_replace_all('Â', 'A') %>%
    str_replace_all('Ê', 'E') %>%
    str_replace_all('Ô', 'O') %>%
    str_replace_all('Ú', 'U') %>%
    str_replace_all('À', 'A')
}

dados_municipios <- dados_municipios %>%
  mutate(nome = substituir_caracteres_especiais(nome),
         nome_uf = substituir_caracteres_especiais(nome_uf))

saveRDS(dados_municipios, 'comprasnet/municipios_uasgs/id_municipios.rds')

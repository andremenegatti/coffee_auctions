library(tidyverse)
library(ggmap)

bec_municipios_pregoes <- readRDS("data/bec_municipios_pregoes.rds")

# Preparando DF para geocoding
muns <- bec_municipios_pregoes %>%
  distinct(municipio) %>%
  unlist()

address <- str_c(muns, ', SP, BRASIL')

# Registrando no API
my_key <- ''
register_google(key = my_key)

# Safely-wrapping geocode
safe_geocode <- safely(geocode)

# Obtendo dados de localizacao
list_muns_geocodes <- address %>%
  map(safe_geocode) %>%
  transpose() %>%
  simplify_all()

# Construindo DF com os dados
df_muns_geocodes <- list_muns_geocodes$result %>%
  set_names(muns) %>%
  bind_rows(.id = 'id')

saveRDS(df_muns_geocodes, 'data/bec_municipios_geocoded.rds')

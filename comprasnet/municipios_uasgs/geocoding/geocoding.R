library(tidyverse)
library(ggmap)

df_uasgs <-
  readRDS('comprasnet/municipios_uasgs/dados_uasgs_com_municipios.rds')

# Preparando DF para geocoding
df_uasgs <- df_uasgs %>%
  mutate(cep = ifelse(is.na(cep), '', cep)) %>%
  mutate(full_address = str_c(endereco, municipio, sigla_uf,
                              str_c('CEP ', cep), sep = ', '))

# Registrando no API
my_key <- ''
register_google(key = my_key)

# Safely-wrapping geocode
safe_geocode <- safely(geocode)

# Obtendo dados de localizacao
list_uasgs_geocodes <- df_uasgs$full_address %>%
  map(safe_geocode) %>%
  transpose() %>%
  simplify_all()

# Construindo DF com os dados
df_uasgs_geocodes <- list_uasgs_geocodes$result %>%
  set_names(df_uasgs$id) %>%
  bind_rows(.id = 'id')

saveRDS(df_uasgs_geocodes,
        'comprasnet/municipios_uasgs/geocoding/uasgs_geocodes.rds')

# No DF salvo acima, 10 o geocoding falhou em 10 observacoes.
# Para esses casos, corrigi os endereços manualmente e repeti
# o geocoding. Os resultados foram salvos em 'df_geocode_na.rds'.
# Juntando esses novos dados e salvando base completa:
df_na_corrigido_manualmente <-
  readRDS('comprasnet/municipios_uasgs/geocoding/df_geocode_na.rds')

df_uasgs_geocodes_completo <- df_uasgs_geocodes %>%
  mutate(full_address_corrigido_manualmente = NA) %>%
  filter(!is.na(lon)) %>%
  select(id, full_address_corrigido_manualmente, lon, lat) %>%
  bind_rows(df_na_corrigido_manualmente)

saveRDS(df_uasgs_geocodes_completo,
        'comprasnet/municipios_uasgs/geocoding/uasgs_geocodes_completo.rds')

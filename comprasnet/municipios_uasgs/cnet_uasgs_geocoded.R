library(tidyverse)

# Carregando bases ------------------------------------------------------------
df_uasgs <- readRDS('comprasnet/municipios_uasgs/dados_uasgs.rds')

id_municipios <-
  readRDS('comprasnet/municipios_uasgs//id_municipios.rds') %>%
  rename(municipio = nome) %>%
  select(-nome_uf)

uasgs_geocodes <-
  readRDS('comprasnet/municipios_uasgs/geocoding/uasgs_geocodes_completo.rds')

# Juntando, criando 'endereco_completo' (usado para geocoding) e ordenando ----
df_uasgs_completo <- df_uasgs %>%
  inner_join(id_municipios,
            by = c('id_municipio' = 'id', 'sigla_uf')) %>%
  inner_join(uasgs_geocodes, by = 'id') %>%
  mutate(endereco_completo = str_c(endereco, municipio, sigla_uf,
                                   str_c('CEP ', cep), sep = ', ')) %>%
  select(id:id_municipio, municipio, lon, lat, sigla_uf,
         endereco, endereco_completo,
         endereco_completo_corrigido = full_address_corrigido_manualmente,
         cep:ramal2)

saveRDS(df_uasgs_completo,
        'comprasnet/municipios_uasgs/cnet_uasgs_geocoded.rds')

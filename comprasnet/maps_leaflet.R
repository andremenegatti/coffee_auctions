library(PregoesBR)
library(htmltools)
library(leaflet)

# Leitura dados ---------------------------------------------------------------
dados_uasgs <-
  readRDS("comprasnet/municipios_uasgs/dados_uasgs_municipio_geocode.rds") %>%
  mutate(co_uasg = str_pad(id, width = 6, side = 'left', pad = '0'))

df_atas <- readRDS("comprasnet/cnet_cafe.rds")

# Data wrangling --------------------------------------------------------------
df_mapa <- df_atas %>%
  mutate(co_uasg = str_extract(id_item, '\\d{6}')) %>%
  inner_join(
    dados_uasgs %>%
      mutate(co_uasg = str_pad(id, width = 6, side = 'left', pad = '0')) %>%
      select(co_uasg, municipio_uasg = municipio,
             id_municipio_uasg = id_municipio,
             nome_uasg = nome, lat, lon),
    by = c('co_uasg', 'municipio_uasg', 'id_municipio_uasg', 'nome_uasg')
    ) %>%
  filter(abertura_lances >= '2011-03-01')

df_mapa_sp <- df_mapa %>%
  filter(sigla_uf == 'SP')

df_mapa_uasgs <- df_mapa %>%
  select(regime_juridico, nome_uasg, lat, lon) %>%
  group_by(nome_uasg) %>%
  mutate(n_leiloes = n()) %>%
  ungroup()

split_mapa <- df_mapa %>%
  split(df_mapa$regime_juridico) %>%
  set_names(unique(df_mapa$regime_juridico))

# Mapas -----------------------------------------------------------------------
nomes_regimes <- c("Sem intervalo minimo", "Regra 20s", "Regra 20s + Regra 3s")

pal <- colorFactor(palette = c("black", "red", "blue"),
                   levels = nomes_regimes)

# Cluster points
df_mapa %>%
  leaflet() %>%
    addProviderTiles("CartoDB")  %>%
    setView( lat = -15.9, lng = -47.8, zoom = 5) %>%
    addCircleMarkers(lng = ~lon, lat = ~lat,
               popup = ~htmlEscape(nome_uasg),
               radius = 8,
               color = ~pal(regime_juridico),
               clusterOptions = markerClusterOptions()) %>%
  addLegend(pal = pal,
            values = nomes_regimes)

# OverlayGroups, por regime juridico
leaflet() %>%
  addProviderTiles("CartoDB")  %>%
  setView( lat = -15.9, lng = -47.8, zoom = 3) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   popup = ~htmlEscape(nome_uasg),
                   radius = 4,
                   opacity = 0.3,
                   # clusterOptions = markerClusterOptions(),
                   color = ~pal(regime_juridico),
                   group = 'Sem intervalo minimo',
                   data = split_mapa$`Sem intervalo minimo`) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   popup = ~htmlEscape(nome_uasg),
                   radius = 4,
                   opacity = 0.3,
                   # clusterOptions = markerClusterOptions(),
                   color = ~pal(regime_juridico),
                   group = 'Regra 20s',
                   data = split_mapa$`Regra 20s`) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   popup = ~htmlEscape(nome_uasg),
                   radius = 4,
                   opacity = 0.3,
                   # clusterOptions = markerClusterOptions(),
                   color = ~pal(regime_juridico),
                   group = 'Regra 20s + Regra 3s',
                   data = split_mapa$`Regra 20s + Regra 3s`) %>%
  addLayersControl(overlayGroups = nomes_regimes)

# Mapa UASGs
df_mapa_uasgs %>%
  leaflet() %>%
  addProviderTiles("CartoDB")  %>%
  setView( lat = -15.9, lng = -47.8, zoom = 3) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   popup = ~htmlEscape(nome_uasg),
                   radius = ~sqrt(n_leiloes),
                   opacity = 0.2,
                   fill = 0.1)

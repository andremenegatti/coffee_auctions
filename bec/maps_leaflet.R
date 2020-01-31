library(PregoesBR)
library(leaflet)
library(htmltools)

bec_municipios_pregoes <- readRDS("~/Mestrado/Dissertacao/Auctions/BEC/bec_municipios_pregoes.rds")
bec_municipios_geocoded <- readRDS("~/Mestrado/Dissertacao/Auctions/BEC/bec_municipios_geocoded.rds")

bec_pregoes <- readRDS("~/Mestrado/Dissertacao/Auctions/BEC/BEC_cafe_etapa3.rds") %>%
  mutate(regime_juridico = case_when(abertura_lances < data_20s ~ 1,
                                     abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
                                     abertura_lances >= data_3s ~ 3) %>%
           factor(labels = c('Sem intervalo minimo', 'Regra 20s', 'Regra 20s + Regra 3s'))) %>%
  mutate(regime_juridico_3s = ifelse(regime_juridico == 'Regra 20s + Regra 3s',
                                     'Após regra dos 3s',
                                     'Antes da regra dos 3s')) %>%
  mutate(regime_juridico_20s = ifelse(regime_juridico %in% c('Regra 20s', 'Regra 20s + Regra 3s'),
                                      'Após regra dos 20s',
                                      'Antes da regra dos 20s'))


df_mapa <- bec_pregoes %>%
  inner_join(bec_municipios_pregoes, by = 'id_item') %>%
  inner_join(bec_municipios_geocoded, by = c('municipio' = 'id')) %>%
  filter(abertura_lances >= '2011-03-01')


df_mapa <- df_mapa %>%
  group_by(municipio) %>%
  mutate(n_leiloes = n()) %>%
  ungroup()

df_mapa <- df_mapa %>%
  mutate(label = str_c("<b>Unidade Compradora:</b> ", htmlEscape(UNIDADE_COMPRADORA), "<br><b>Data:</b> ", abertura_lances))

pal <- colorFactor(palette = c("black", "red", "blue"),
                   levels = c("Sem intervalo minimo", "Regra 20s", "Regra 20s + Regra 3s"))

df_mapa %>%
  leaflet() %>%
  addProviderTiles("CartoDB")  %>%
  setView( lng = -48.2, lat = -21.8, zoom = 8) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   label = ~UNIDADE_COMPRADORA,
                   radius = 7,
                   # opacity = 0.3
                   color = ~pal(regime_juridico),
                   clusterOptions = markerClusterOptions()
                   ) %>%
  addLegend(pal = pal,
            values = c("Sem intervalo minimo", "Regra 20s", "Regra 20s + Regra 3s"))



split_mapa <- df_mapa %>%
  split(df_mapa$regime_juridico) %>%
  set_names(unique(df_mapa$regime_juridico))

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
  addLayersControl(overlayGroups = c("Sem intervalo minimo", "Regra 20s", "Regra 20s + Regra 3s"))




df_mapa_uasgs %>%
  leaflet() %>%
  addProviderTiles("CartoDB")  %>%
  setView( lat = -15.9, lng = -47.8, zoom = 3) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   popup = ~htmlEscape(nome_uasg),
                   radius = ~sqrt(n_leiloes),
                   opacity = 0.2,
                   fill = 0.1)

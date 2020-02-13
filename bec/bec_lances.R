library(tidyverse)
library(lubridate)

bec_cafe <- readRDS('data/bec_cafe.rds')

bid_inc1 <- bec_cafe %>% 
  filter(map_lgl(.x = lances, .f = ~ 'valor' %in% names(.x))) %>% 
  mutate(lances = map(.x = lances, .f = ~ arrange(.x, data)))

bid_inc2 <- bid_inc1 %>% 
  mutate(lances = map2(.x = lances, .y = dt_inicio,
                       .f = ~ filter(.x, min(data) >= .y))) %>% 
  mutate(n_bids = map(.x = lances, .f = ~ nrow(.x))) %>% 
  filter(n_bids > 1)

# Deflacionando
bid_inc3 <- bid_inc2 %>%
  mutate(
    lances =
      map2(.x = lances,
           .y = indice_ipca_mes,
           .f = ~  .x %>% 
             mutate(
               valor_defl = PregoesBR::deflacionar(valor,
                                                   # IPCA dez/2015
                                                   indice_referencia = 4493.17,
                                                   indice_no_periodo = .y)
               ))
    )

# Valor/kg
bid_inc3 <- bid_inc3 %>% 
  mutate(
    lances = 
      pmap(.l = list(lances, kg_por_unid, quantidade),
           .f = ~ ..1 %>% 
             mutate(valor_kg_defl = (valor_defl / ..2) %>% 
                      PregoesBR::corrigir_erro_de_medida(corte = 40,
                                                         quantidade = ..3)))
  ) %>%
  # Variaveis com primeiros e ultimos lances
  mutate(last_bid_kg_defl = map_dbl(.x = lances,
                                    .f = ~ last(.x$valor_kg_defl)),
         first_bid_kg_defl = map_dbl(.x = lances,
                                     .f = ~ first(.x$valor_kg_defl)))

# Calculando incrementos para valores corrigidos
bid_inc4 <- bid_inc3 %>%
  mutate(
    lances =
      map2(.x = lances, .y = first_bid_kg_defl,
           .f = ~ .x %>%
             mutate(
               lowest_bid_so_far_kg_defl =
                 cummin(.x$valor_kg_defl),
               incremento_anterior = 
                 valor_kg_defl - lag(valor_kg_defl),
               incremento_menor = 
                 valor_kg_defl - lag(lowest_bid_so_far_kg_defl),
               incremento_menor_bruto = 
                 valor - lag(cummin(..1$valor)),
               norm_inc_first = incremento_menor / .y
             )
      )
  )

# Calculando intervalo entre lances consecutivos
bid_inc4 <- bid_inc4 %>%
  mutate(
    lances =
      map(.x = lances,
          .f = ~ .x %>%
            mutate(
              intervalo_anterior =
                PregoesBR::calcular_intervalo_lances(data_hora1 = data)
              ))
    )


get_time_lowest_bid <- function(x, time_var, lowest_bid = NULL) {
  if (is.null(lowest_bid)) lowest_bid <- find_lowest(x)
  out <- map_dbl(.x = 1:length(x),
                 .f = ~ time_var[min(which(lowest_bid[.x] == x[1:.x]))]) %>%
    unlist() 
  
  out[1] %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC")
}

# Calculando o intervalo entre um lance e o melhor lance que o antecedeu
bid_inc4 <- bid_inc4 %>%
  mutate(
    lances =
      map(.x = lances,
          .f = ~ .x %>%
            mutate(
              
              hora_lowest_bid_so_far =
                get_time_lowest_bid(
                  valor_kg_defl,
                  data,
                  lowest_bid_so_far_kg_defl
                  ),
              
              intervalo_menor =
                PregoesBR::calcular_intervalo_lances(
                  data_hora1 = data,
                  data_hora2 = lag(hora_lowest_bid_so_far)
                )
            ))
  )

bec_lances <- bid_inc4 %>% 
  select(id_item, first_bid_kg_defl,
         last_bid_kg_defl, lances) %>% 
  unnest(cols = lances)

saveRDS(bec_lances, 'data/bec_lances.rds')

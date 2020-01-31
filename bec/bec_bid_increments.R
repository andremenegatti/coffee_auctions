library(tidyverse)
library(lubridate)
theme_set(theme_bw())

# Carregando funcao para trimming
source("trim_df.R")
source("trim_df_grouped.R")
source("make_lineplot.R")
source("get_summary_stats.R")


data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

BEC_cafe_full <- readRDS('BEC_cafe_etapa4.rds')


BEC_bid_inc <- BEC_cafe_full %>% 
  # Removendo observacoes em que nao existe uma coluna 'Valor' no historico de lances
  filter(map_lgl(.x = ListaLances_clean, .f = ~ 'Valor' %in% names(.x))) %>% 
  # Limpando valores
  mutate(ListaLances_clean = map(.x = ListaLances_clean,
                                 .f = ~ .x %>% 
                                   mutate(Valor = readr::parse_number(Valor, locale = readr::locale(decimal_mark = ",")))))

BEC_bid_inc <- BEC_bid_inc %>% 
  # Criando coluna com dfs de lances (ja excluidas as propostas) com valores limpos
  mutate(bid_increments = map(.x = ListaLances_clean, .f = ~ .x %>% arrange(Data))) %>% 
  # Coluna com contagem de linhas dos dfs criados acima
  mutate(nrow_bid_increments = map_int(.x = bid_increments, .f = ~ nrow(.x)))


# Removendo observacoes em que nao houve lances ou em que houve apenas um lance
BEC_bid_inc <- BEC_bid_inc %>% 
  filter(nrow_bid_increments > 1)

# Funcao para retornar o 'minimo cumulativo' de um vetor numerico
# Retorna vetor de mesmo comprimento do vetor original
find_lowest <- function(x) {
  index = 1:length(x)
  map_dbl(.x = index,
          .f = ~ min(x[1:.x]))
}

# Adicionando colunas nos dfs da coluna bid_increments
BEC_bid_inc <- BEC_bid_inc %>% 
  mutate(bid_increments = map(.x = bid_increments,
                               .f = ~ .x %>% 
                                 mutate(lowest_bid_so_far = find_lowest(.x$Valor)) %>%
                                 mutate(lag_lowest_bid = lag(lowest_bid_so_far),
                                        increment = Valor - lag_lowest_bid,
                                        norm_inc_first = increment / first(Valor)) %>% 
                                 filter(increment < 0)))

# Nos casos em que ha apenas um lance (apos o filtro Valor == lowest_bid_so_far), tem-se NA no incremento por causa do lag; 
# Filtramos para ficar apenas com casos em que ha no minimo dois lances em 'bid_increments'
BEC_bid_inc <- BEC_bid_inc %>% 
  filter(map_lgl(.x = bid_increments, .f = ~ nrow(.x) > 1))

# Carregando dataframe para correcao
df_ipca <- read_csv('ipca.csv') %>% mutate(inicio_mes = lubridate::ymd(inicio_mes))
deflacionar <- function(x, indice_referencia = df_ipca$ipca[nrow(df_ipca)], indice_no_periodo) x * indice_referencia / indice_no_periodo
corrigir_erro_de_medida <- function(valor, corte, quantidade) if_else(valor > corte, valor / quantidade, valor)


BEC_bid_inc <- BEC_bid_inc %>% 
  # Adicionando novas colunas em bid_increments para tentar abordar problemas nos valores
  mutate(bid_increments = pmap(.l = list(bid_increments, indice_ipca_mes, kg_fornecidos),
                               .f = ~ ..1 %>% 
                                 mutate(Valor_defl = deflacionar(x = Valor, indice_no_periodo = ..2),
                                        Valor_corr = corrigir_erro_de_medida(valor = Valor_defl, corte = 40, quantidade = ..3),
                                        first_bid_corr = first(Valor_corr)))) %>% 
  # Variaveis com primeiros e ultimos lances apos deflacao/correcao para erro de medida
  mutate(last_bid_defl = map_dbl(.x = bid_increments, .f = ~ last(.x$Valor_defl)),
         last_bid_corr = map_dbl(.x = bid_increments, .f = ~ last(.x$Valor_corr)),
         first_bid_defl = map_dbl(.x = bid_increments, .f = ~ first(.x$Valor_defl)),
         first_bid_corr = map_dbl(.x = bid_increments, .f = ~ first(.x$Valor_corr)))


# Recalculando incrementos para valores corrigidos
BEC_bid_inc <- BEC_bid_inc %>%
  mutate(bid_increments = map2(.x = bid_increments, .y = first_bid_corr,
                               .f = ~ .x %>%
                                 mutate(lowest_bid_so_far_corr = find_lowest(.x$Valor_corr)) %>%
                                 mutate(lag_lowest_bid_corr = lag(lowest_bid_so_far_corr),
                                        increment_corr = Valor_corr - lag_lowest_bid_corr,
                                        norm_inc_first_corr = increment_corr / .y)))



# Filtrando dfs para manter incrementos NEGATIVOS apenas (valor menor que o melhor lance ate o momento)
BEC_bid_inc <- BEC_bid_inc %>% 
  mutate(bid_increments = map(.x = bid_increments, .f = ~ .x %>% filter(increment_corr < 0)))

# Nos casos em que ha apenas um lance (apos o filtro valor_lance == lowest_bid_so_far OU increment < 0), tem-se NA no incremento por causa do lag; 
# Filtramos para ficar apenas com casos em que ha no minimo dois lances em 'bid_increments'
BEC_bid_inc <- BEC_bid_inc %>% 
  filter(map_lgl(.x = bid_increments, .f = ~ nrow(.x) > 1))


#### MEDIA DO INCREMENTO NORMALIZADO DE CADA PREGÃO ####
BEC_bid_inc <- BEC_bid_inc %>% 
  mutate(avg_norm_inc_reserve = map_dbl(.x = bid_increments, .f = ~ mean(.x$norm_inc_reserve_corr, na.rm = TRUE)),
         avg_norm_inc_first = map_dbl(.x = bid_increments, .f = ~ mean(.x$norm_inc_first_corr, na.rm = TRUE)))

# saveRDS(BEC_bid_inc, 'cafe_comprasnet_BEC_bid_inc_fim_fase7_1.rds')

#### UNNESTING PARA CALCULAR O INCREMENTO MEDIO GERAL ####
BEC_bid_inc_unnested <- BEC_bid_inc %>% 
  select(id_item, inicio_ano, abertura_lances, inicio_semana, inicio_mes,
         inicio_bimestre, inicio_trimestre, inicio_semestre,
         #periodos2, periodos3,
         bid_increments) %>% 
  unnest()
# saveRDS(BEC_bid_inc_unnested, 'cafe_comprasnet_BEC_bid_inc_unnested_fim_fase7_1.rds')

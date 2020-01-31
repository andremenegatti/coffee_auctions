devtools::load_all(".")

# Carregando parte 1
futuros1 <- readxl::read_excel('Contratos futuros.xlsx',
                               sheet = 1, range = 'A2:B1468',
                               col_names = c('data', 'valor')) %>%
  mutate(data = mdy(data))

# Carregando parte 2
futuros2 <- readxl::read_excel('Contratos futuros3.xlsx',
                               sheet = 2, range = 'A2:B963', col_names = c('data', 'valor')) %>%
  mutate(data = as.Date(x = data, origin = '1899-12-30'))

# Juntando
futuros <- futuros1 %>%
  bind_rows(futuros2) %>%
  mutate(valor = as.numeric(valor)) %>%
  filter(data >= '2011-03-01',
         data < '2018-01-01')

# Criando variaveis de tempo
futuros <- futuros %>%
  create_time_variables(time_var = data) %>%
  group_by(inicio_mes) %>%
  mutate(futuro_media_mensal = mean(valor, na.rm = TRUE)) %>%
  ungroup()

saveRDS(futuros, 'futuros.rds')

# Graficos exploratorios
library(xts)

ts_futuros <- xts(x = futuros[, 'valor'], order.by = futuros$data)

plot(ts_futuros)

# Calculando medias mensais
split_months <- split(ts_futuros$valor, f = 'months')
split_months_begin <- purrr::map_df(.x = split_months, .f = ~ as.data.frame(.x) %>% rownames_to_column('data') %>% first())
monthly_means <- do.call(rbind, lapply(X = split_months, FUN = mean, na.rm = TRUE))

ts_futuros_mensal <- xts(x = monthly_means, order.by = split_months_begin$data %>% ymd() )

plot(ts_futuros_mensal)

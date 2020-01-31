library(tidyverse)
library(ComprasnetAtasParsing)

# pregoes_cafe <- read_csv('C:/Users/Dell/Desktop/pregoes_cafe06junho2019.csv')
df_cafe_completo <- readRDS('Comprasnet/Dados/df_cafe_completo.rds')

# Nesting: id_pregao
df_cafe_completo <- df_cafe_completo %>%
  nest(-id_pregao)

# Criando coluna 'indices', com os indices dos itens de interesse
df_cafe_completo <- df_cafe_completo %>%
  mutate(indices = map(.x = data,
                       .f = ~ .x$id_item %>% str_remove(pattern = '\\d{16}') %>% as.numeric()))

# Parsing das atas, usando indices dos itens para maior eficiencia
df_atas_indices <- tibble()
for (i in 178:nrow(df_cafe_completo)) {
  print(i)
  file <- str_c('C:/Users/Dell/Desktop/atas/atas_cafe/cafe/', df_cafe_completo$id_pregao[i], '_ata000.html')
  print(file)
  html_file <- read_html(file, encoding = 'utf-8')
  ata <- get_bid_data(html_file, indices = df_cafe_completo$indices[[i]])
  df_atas_indices <- bind_rows(df_atas_indices, ata)
}

# saveRDS(df_atas_indices, 'C:/Users/Dell/Desktop/df_atas_cafe.rds')

library(tidyverse)

df_itens <- readRDS('df_itens2.rds')

parts <- 7L
n_obs <- nrow(df_itens)
obs_per_complete_part <- 100000L
begin <- map_int(0L:(parts - 1L), ~ (.x * obs_per_complete_part) + 1L)
end <- map_int(1L:parts, ~ if_else(.x * obs_per_complete_part < n_obs, .x * obs_per_complete_part, n_obs))

for (i in 1:parts) {
  print(i)
  
  print('Unnesting...')
  df_partial <- df_itens[begin[i]:end[i], ] %>% 
    unnest() %>% 
    select(-link) %>% 
    mutate_all(toupper)
  
  print('Criando indices...')
  df_partial <- df_partial %>% 
    mutate(indice_item = str_match(title, 'ITEM\\s{1,2}(\\d+):')[,2],
           indice_grupo = str_match(title, 'ITEM -(\\d+):')[,2])
  
  print('Criando id_item')
  df_partial <- df_partial %>% 
    mutate(id_item = str_c(id_pregao,
                           str_pad(string = indice_item, width = 4, side = 'left', pad = '0')))
  
  print('Reordenando colunas e removendo variavel "title"...')
  df_partial <- df_partial %>% 
    select(id_item, id_pregao:valor_negociado, indice_item, indice_grupo)
  
  file_name <- str_c('COMPRASNET_itens_parte', as.character(i), '.rds')
  print('Salvando...')
  saveRDS(df_partial, file_name)
  print('Feito!')
  rm(df_partial)
}

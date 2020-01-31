library(purrr)
library(tidyverse)

df_resultados_unnested <- readRDS("~/Mestrado/Dissertacao/R/WebScrapingPregoes/Comprasnet/Dados/df_resultados_unnested.rds")

parts <- 6L
n_obs <- nrow(df_resultados_unnested)
obs_per_complete_part <- 2600000L
begin <- map_int(0L:(parts - 1L), ~ (.x * obs_per_complete_part) + 1L)
end <- map_int(1L:parts, ~ if_else(.x * obs_per_complete_part < n_obs, .x * obs_per_complete_part, n_obs))

for (i in 1:parts) {
  df_resultados_partial <- df_resultados_unnested[begin[i]:end[i], ]
  file_name <- str_c('COMPRASNET_resultados_parte', as.character(i), '.rds')
  saveRDS(df_resultados_partial, file_name)
  rm(df_resultados_partial)
}

library(tidyverse)
library(plotly)
# Outros pacotes necessarios: broom, lfe, viridis, htmlwidgets, PregoesBR

# Carregando base de dados
dd_sp <- readRDS('data/dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Rodando modelos placebo -----------------------------------------------------
# Janela a ser aberta ao redor de cada data
bandwidth <- seq(-210, 210, by = 5)
# Nota: para deixar computacionalmente mais leve,
# experimentar colocar intervalos entre as datas

# Cruzando datas: todas 121x121 = 14631 combinacoes
df_placebo <-
  crossing(data_20s_alt = bandwidth + data_20s,
           data_3s_alt = bandwidth + data_3s) %>%
  # Criando dummies de tratamento alternativas (cada celula contem uma lista)
  mutate(
    treat1 = map2(
      .x = data_20s_alt,
      .y = data_3s_alt,
      .f = ~ if_else(
        comprasnet == 1 & abertura_lances >= .x & abertura_lances < .y,
        0 , 1)
    ),
    treat2 = map(
      .x = data_3s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances > .x, 1, 0)
    )
  )


for (i in 2422:nrow(df_placebo)) {
  
  message(str_c('Fitting model ', i, '/', nrow(df_placebo)))
  
  model <- 
    lm(num_forn_lances ~ df_placebo$treat1[[i]] + df_placebo$treat2[[i]] +
         qualidade + kg_por_unid +  futuro_defl + arab_defl + bimestre + 
         unidade_compradora + municipio + marca_vencedor_principais)
  
  message('Estimating HC1 standard errors')
  robust_se <- PregoesBR::get_robust_std_errors(model)
  
  filename <- str_c('dd/placebo/data/n_bidders_grid/sp/',
                    as.character(df_placebo$data_20s_alt[[i]]), '_',
                    as.character(df_placebo$data_3s_alt[[i]]),
                    '.rds')
  
  message('Writing ', filename, '\n')
  
  saveRDS(robust_se, filename)
  
  rm(model, robust_se)
  
}

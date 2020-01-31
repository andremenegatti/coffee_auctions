library(tidyverse)

# Abrindo cada parte da base geral (limpa) da BEC
# e selecionando apenas observacoes de cafe
bec_cafe <- tibble()

for (i in 1:3) {

  file <- str_c('bec/montando_base_v2/bec_itens_parte',
                as.character(i), '_v2.rds')

  message(str_c(i, ': ', file))

  bec_cafe_partial <- readRDS(file)

  bec_cafe_partial <-  bec_cafe_partial %>%
    rename(melhor_lance = menor_valor) %>%
    filter(str_detect(descricao_item, '^CAF[EÉ] '),
           !str_detect(unidade_fornecimento, 'SACHE|CAIXA|POTE'),
           # Pregoes sem melhor lance parecem nao ter sido concluidos.
           # Na maior parte dos casos, nao houve lances ou os lances
           # nao foram considerados aceitaveis. Em alguns casos, parece
           # ter havido negociacao ou recurso.
           melhor_lance != 0,
           dt_inicio < as.Date('2018-01-01'))

  # Juntando dados e excluindo df parcial
  bec_cafe <- bind_rows(bec_cafe, bec_cafe_partial)
  rm(bec_cafe_partial)
}

# Eliminando observacoes duplicadas
bec_cafe <- bec_cafe %>%
  distinct(id_item, .keep_all = TRUE)

# Salvando
saveRDS(bec_cafe, 'bec/montando_base_v2/bec_cafe_raw_v2.rds')

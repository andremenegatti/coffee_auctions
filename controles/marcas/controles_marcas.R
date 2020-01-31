library(tidyverse)

# Carregando bases, renomeando variavel com valores limpos e filtrando período
cnet_marcas <- readRDS('controles/marcas/cnet_marcas.rds') %>%
  rename(marca_vencedor_clean_final = marca_vencedor_clean4) %>%
  filter(abertura_lances >= '2011-03-01', abertura_lances < '2017-01-01')

bec_marcas <- readRDS('controles/marcas/bec_marcas.rds') %>%
  rename(marca_vencedor_clean_final = marca_vencedor_clean2) %>%
  filter(dt_inicio >= '2011-03-01', dt_inicio < '2017-01-01')

# 15 principais marcas de cada base
main15_bec <- table(bec_marcas$marca_vencedor_clean_final) %>%
  sort(decreasing = TRUE) %>% head(n = 15) %>% names()

main15_cnet <- table(cnet_marcas$marca_vencedor_clean_final) %>%
  sort(decreasing = TRUE) %>% head(n = 15) %>% names()

# Criando vetor com a uniao dos dois conjuntos acima
main15 <- union(main15_bec, main15_cnet)

# Juntando bases de marcas em uma base unica
marcas_full <- cnet_marcas %>%
  select(id_item, propostas, vencedor, marca_vencedor,
         marca_vencedor_clean, marca_vencedor_clean_final) %>%
  bind_rows(bec_marcas %>%
              select(id_item, vencedor = nome_fornecedor,
                     marca_vencedor, marca_vencedor_clean,
                     marca_vencedor_clean_final))

# Criando variavel com as principais marcas
marcas_full <- marcas_full %>%
  mutate(marca_vencedor_principais =
           ifelse(marca_vencedor_clean_final %in% main15,
                  marca_vencedor_clean_final,
                  'Outra') %>% factor() %>% relevel(ref = 'Outra'))

# saveRDS(marcas_full, 'controles/marcas/controles_marcas.rds')

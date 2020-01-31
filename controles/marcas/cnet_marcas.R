library(PregoesBR)

# Abrindo base
df_atas <- readRDS("data/cnet_cafe.rds")

# Subsetting
df_marcas <- df_atas %>%
  filter(!is.na(vencedor)) %>%
  filter(abertura_lances >= '2011-03-01') %>%
  select(id_item, abertura_lances, sigla_uf, propostas, resultado, vencedor)

# Extraindo especificacoes do vencedor
df_marcas <- df_marcas %>%
  mutate(especificacoes_vencedor =
           map2(.x = propostas, .y = vencedor,
                .f = ~ filter(.x, str_detect(fornecedor, .y)) %>%
                  select(especificacoes)))

# Corrigindo o unico erro manualmente
df_tilt <- df_marcas %>%
  filter(map_lgl(.x = especificacoes_vencedor, .f = ~ nrow(.x) > 1)) %>%
  mutate(especificacoes_vencedor =
           list(especificacoes_vencedor[[1]] %>% slice(2)))

tilt_index <- which(df_marcas$id_item == df_tilt$id_item[1])
df_marcas$especificacoes_vencedor[tilt_index] <- df_tilt$especificacoes_vencedor[1]

# Extraindo marca e fabricante
df_marcas <- df_marcas %>%
  mutate(especificacoes_vencedor = unlist(especificacoes_vencedor),
         marca_vencedor = str_match(especificacoes_vencedor,
                                    'Marca: (.+)\r\n')[,2],
         fabricante_vencedor = str_match(especificacoes_vencedor,
                                         'Fabricante: (.+)\r\n')[,2])

# Limpando padrões comuns -----------------------------------------------------
df_marcas <- df_marcas %>%
  mutate(marca_vencedor_clean = marca_vencedor %>%
           substituir_caracteres_especiais() %>%
           str_trim() %>%
           str_remove(pattern = '^CAFE ') %>%
           str_remove(pattern = ' CAFE') %>%
           str_remove(pattern = ' ESPECIAL| VACUO| EXPRESSO') %>%
           str_replace(pattern = 'SAVASSI.*', replacement = 'SAVASSI') %>%
           str_replace(pattern = 'SAO BRAZ.*', replacement = 'SAO BRAZ') %>%
           str_remove(pattern = ' PREMIUM| GOLDEN| SUPERIOR| GOURMET| A VACUO') %>%
           str_remove(pattern = 'EXTRA F*.') %>%
           str_replace(pattern = 'ITAMARAT[A-Z]{1,3}',
                       replacement = 'ITAMARATY') %>%
           str_replace(pattern = 'MELITA', replacement = 'MELITTA') %>%
           str_replace(pattern = 'TRES', replacement = '3') %>%
           str_replace(pattern = '.*3 ?CORA(COES|CAO).*',
                       replacement = '3 CORACOES') %>%
           str_replace(pattern = 'ODEBRE[A-Z]+', replacement = 'ODEBRECHT') %>%
           str_replace(pattern = 'ODEBRECHT.*', replacement = 'ODEBRECHT') %>%
           str_replace(pattern = 'LOZANGO', replacement = 'LOSANGO') %>%
           str_replace(pattern = 'BICO DE OURO ', replacement = 'BICO DE OURO'))

# Primeira etapa de limpeza manual --------------------------------------------
# Salvando coluna 'marcas_vencedor_clean' para limpeza manual no Excel
df_marcas %>%
  select(marca_vencedor_clean) %>%
  distinct() %>%
  write.csv('limpando_marcas_cnet.csv')

# Importando tabela excel com nomes limpos manualmente
limpando_marcas <- readxl::read_excel('controles/marcas/limpando_marcas_cnet.xlsx',
                                      sheet = 1, trim_ws = FALSE)

# Juntando dados
df_marcas2 <- df_marcas %>%
  left_join(limpando_marcas, by = 'marca_vencedor_clean')

# Segunda etapa de limpeza manual ---------------------------------------------
# Salvando coluna 'marcas_vencedor_clean' para limpeza manual no Excel
df_marcas2 %>%
  filter(is.na(marca_vencedor_clean2)) %>%
  select(fabricante_vencedor, marca_vencedor, marca_vencedor_clean) %>%
  distinct() %>%
  write.csv('limpando_marcas_cnet_etapa2.csv')

# Importando tabela excel com o restante dos nomes limpos manualmente
limpando_marcas_etapa2 <-
  readxl::read_excel('controles/marcas/limpando_marcas_cnet_etapa2.xlsx',
                     sheet = 1, trim_ws = FALSE)

# Juntando dados --------------------------------------------------------------
# Nova coluna incorpora resultados de todas as etapas de limpeza
df_marcas3 <- df_marcas2 %>%
  left_join(limpando_marcas_etapa2,
            by = c('fabricante_vencedor',
                   'marca_vencedor',
                   'marca_vencedor_clean')) %>%
  mutate(marca_vencedor_clean4 =
           ifelse(is.na(marca_vencedor_clean2),
                  marca_vencedor_clean3,
                  marca_vencedor_clean2))

# Checando quanto as principais marcas representam do total de leiloes --------
# UASGS de todo o Brasil
n_principais <- 15

total_principais <- table(df_marcas3$marca_vencedor_clean4) %>%
  sort(decreasing = TRUE) %>% head(n = n_principais) %>% sum()

share_principais <- total_principais / nrow(df_marcas3)

print(str_c(round(share_principais*100, digits = 2),
            '% do total de leiloes da amostra teve como vencedor um produto de uma das ',
            n_principais,
            ' principais marcas.'))

# UASGs de SP
df_marcas_sp <- df_marcas3 %>%
  filter(sigla_uf == 'SP')

total_principais_sp <- table(df_marcas_sp$marca_vencedor_clean4) %>%
  sort(decreasing = TRUE) %>% head(n = n_principais) %>% sum()

share_principais_sp <- total_principais_sp / nrow(df_marcas_sp)

print(str_c(round(share_principais_sp*100, digits = 2),
            '% do total de leiloes de SP teve como vencedor um produto de uma das ',
            n_principais,
            ' principais marcas.'))

# Limpando nome dos fabricantes -----------------------------------------------
df_marcas3 <- df_marcas3 %>%
  mutate(fabricante_vencedor_clean = fabricante_vencedor %>%
           substituir_caracteres_especiais() %>%
           str_trim())

saveRDS(df_marcas3, 'controles/marcas/cnet_marcas.rds')

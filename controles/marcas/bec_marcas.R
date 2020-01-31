library(PregoesBR)

bec_cafe <- readRDS("data/bec_cafe.rds")

# Filtrando periodo de interesse e selecionando variaveis
bec_marcas <- bec_cafe %>%
  filter(dt_inicio > '2011-03-01',
         !is.na(win_bid_kg)) %>%
  select(id_item, dt_inicio, propostas, nome_fornecedor)

# Extraindo marca do licitante vencedor do DF de propostas
bec_marcas <- bec_marcas %>%
  mutate(marca_vencedor =
           map2_chr(.x = propostas, .y = nome_fornecedor,
                    .f = ~ filter(.x, licitante == .y) %>%
                      select(marca) %>%
                      unlist()))

# Limpando padrões mais comuns ------------------------------------------------
bec_marcas <- bec_marcas %>%
  mutate(marca_vencedor_clean = marca_vencedor %>%
           substituir_caracteres_especiais() %>%
           str_trim()) %>%
  mutate(marca_vencedor_clean = marca_vencedor_clean %>%
           str_remove(pattern = '^CAFE ') %>%
           str_remove(pattern = ' CAFE') %>%
           str_remove(pattern = ' ESPECIAL| VACUO| EXPRESSO') %>%
           str_replace(pattern = 'SAVASSI.*', replacement = 'SAVASSI') %>%
           str_replace(pattern = 'SAO BRAZ.*', replacement = 'SAO BRAZ') %>%
           str_remove(pattern =
                        ' PREMIUM| GOLDEN| SUPERIOR| GOURMET| A VACUO') %>%
           str_remove(pattern = 'EXTRA F*.') %>%
           str_replace(pattern = 'ITAMARAT[A-Z]{1,3}',
                       replacement = 'ITAMARATY') %>%
           str_replace(pattern = 'MELITA', replacement = 'MELITTA') %>%
           str_replace(pattern = 'TRES', replacement = '3') %>%
           str_replace(pattern = '.*3 ?CORA(COES|CAO).*',
                       replacement = '3 CORACOES') %>%
           str_replace(pattern = 'ODEBRE[A-Z]+', replacement = 'ODEBRECHT') %>%
           str_replace(pattern = 'ODEBRECHT.*', replacement = 'ODEBRECHT') %>%
           str_replace(pattern = 'LOZANGO', replacement = 'LOSANGO'))

# Resolvendo problema com espacos adicionais em MARIA MARIA
bec_marcas <- bec_marcas %>%
  mutate(marca_vencedor_clean =
           ifelse(str_detect(marca_vencedor_clean, 'MARIA'),
                  'MARIA MARIA',
                  marca_vencedor_clean))

# Limpeza manual --------------------------------------------------------------
# Salvando em CSV para limpeza manual do Excel
bec_marcas %>%
  distinct(marca_vencedor_clean) %>%
  write.csv('limpando_marcas_bec.csv', row.names = FALSE)

# Importando tabela excel com nomes ja limpos manualmente
limpando_marcas_bec <-
  readxl::read_excel('controles/marcas/limpando_marcas_bec.xlsx',
                     sheet = 1, trim_ws = FALSE)

# Juntando dados
bec_marcas2 <- bec_marcas %>%
  left_join(limpando_marcas_bec, by = 'marca_vencedor_clean')

# Checando quanto as principais marcas representam do total de leiloes
n_principais <- 15

total_principais <- table(bec_marcas2$marca_vencedor_clean2) %>%
  sort(decreasing = TRUE) %>% head(n = n_principais) %>% sum()

share_principais <- total_principais / nrow(bec_marcas2)

print(str_c(round(share_principais*100, digits = 2),
            '% do total de leiloes da amostra teve como vencedor um produto de uma das ',
            n_principais,
            ' principais marcas.'))

saveRDS(bec_marcas2, 'controles/marcas/bec_marcas.rds')

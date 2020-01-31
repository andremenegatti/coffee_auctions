library(tidyverse)

bec <- readRDS('data/bec_cafe.rds') %>%
  rename(abertura_lances = dt_inicio) %>%
  filter(abertura_lances >= '2011-03-01') %>%
  select(id_item, descricao = descricao_item,
         desc_comp = descricao_detalhada,
         kg_por_unid, kg_fornecidos, win_bid_kg)

cnet <- readRDS('data/cnet_cafe.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  select(id_item, sigla_uf,
         desc_comp = descricao_complementar,
         kg_por_unid, kg_fornecidos, win_bid = win_bid_kg) %>%
  mutate(desc_comp = PregoesBR::substituir_caracteres_especiais(desc_comp))

cnet_sp <- cnet %>%
  filter(sigla_uf == 'SP')

# BEC -------------------------------------------------------------------------
bec_qualidade <- bec %>%
  select(id_item, descricao) %>%
  mutate(
    qualidade = case_when(
      str_detect(descricao, '^CAFE ESPECIAL GOURMET') ~ 'GOURMET',
      str_detect(descricao, '^CAFE ESPECIAL (SUPERIOR|TORRADO)') ~ 'SUPERIOR',
      str_detect(descricao, '^CAFE TRADICIONAL') ~ 'TRADICIONAL'
    )
  )

# saveRDS(bec_qualidade, 'controles/qualidade/bec_qualidade.rds')

# CNET SP ---------------------------------------------------------------------
# Salvando csv para limpeza manual no Excel
cnet_sp_desc <- cnet_sp %>%
  count(desc_comp) %>%
  arrange(-n)

# write_delim(cnet_sp_desc,
#             'controles/qualidade/cnet_sp_desc.csv', delim = ';-;')

# Abrindo arquivo xlsx com dados limpos manualmente
cnet_sp_desc_xlsx <-
  readxl::read_excel('controles/qualidade/cnet_sp_desc.xlsx',
                     sheet = 1, range = 'A1:C102') %>%
  # Durante criacao do csv e xlsx, houve introdução de 'Â' antes de alguns
  # caracteres especiais. Precisamos excluir para realizar o inner_join
  mutate(desc_comp = str_remove_all(desc_comp, 'Â')) %>%
  select(-n)

# Incluindo coluna com dados limpos manualmente
cnet_sp_desc_clean <- cnet_sp_desc %>%
  inner_join(cnet_sp_desc_xlsx, by = 'desc_comp') %>%

  # Considerando EXTRA-FORTE como TRADICIONAL
  mutate(qualidade =
           ifelse(str_detect(desc_comp,
                             'EXTRAFORTE|EXTRA-FORTE|EXTRA FORTE') & is.na(qualidade),
                  'TRADICIONAL', qualidade)) %>%

  # Criando duas variaveis de qualidade, conforme NA sejam classificados como TRADICIONAL ou nao
  mutate(qualidade2 = ifelse(is.na(qualidade), 'NAO INFORMADA', qualidade),
         qualidade = ifelse(is.na(qualidade), 'TRADICIONAL', qualidade))

cnet_sp_qualidade <- cnet_sp %>%
  left_join(cnet_sp_desc_clean, by = 'desc_comp') %>%
  select(id_item, desc_comp, qualidade, qualidade2)

# saveRDS(cnet_sp_qualidade, 'controles/qualidade/cnet_sp_qualidade.rds')

# CNET SEM SP -----------------------------------------------------------------
cnet_desc_to_be_cleaned <- cnet %>%
  anti_join(cnet_sp_desc_clean, by = 'desc_comp') %>%
  count(desc_comp) %>%
  arrange(-n)

# Selecionando observacoes em que ha mencao a 'PONTOS'
cnet_pontos <- cnet_desc_to_be_cleaned %>%
  filter(str_detect(desc_comp, 'PONTOS')) %>%
  mutate(pontos_context = str_extract(desc_comp, '.{20}PONTOS'))

# Salvando para limpeza manual
# write_delim(cnet_pontos, 'controles/qualidade/cnet_pontos.csv', delim = ';-;')

# Abrindo arquivo xlsx com dados limpos manualmente
cnet_pontos_xlsx <- readxl::read_excel('controles/qualidade/cnet_pontos.xlsx',
                                       sheet = 1, range = 'A1:D246') %>%
  # Vide comentário anterior sobre 'Â'
  mutate(desc_comp = str_remove_all(desc_comp, 'Â')) %>%
  select(desc_comp, qualidade)

# Juntando dados limpos com base na pontuacao
cnet_pontos <- cnet_pontos %>%
  inner_join(cnet_pontos_xlsx, by = 'desc_comp')

# Atualizando base a ser limpa
cnet_desc_to_be_cleaned2 <- cnet_desc_to_be_cleaned %>%
  anti_join(cnet_pontos, by = 'desc_comp')

# Selecionando observacoes que parecem ser de qualidade superior
cnet_superior <- cnet_desc_to_be_cleaned2 %>%
  filter(str_detect(desc_comp, '(TIPO|QUALIDADE) SUPERIOR')) %>%
  mutate(
    superior_context =
      str_extract(desc_comp, '.{15}(TIPO|QUALIDADE) SUPERIOR.{15}'),
    escala_context =
      str_extract(desc_comp, '.{30}ESCALA( SENSORIAL)+.{15}'),
    minimo_context =
      str_extract(desc_comp, '.{15}QUALIDADE (GLOBAL )?MINIMA.{20}')
  )

# Salvando para limpeza manual
# write_delim(cnet_superior, 'controles/qualidade/cnet_superior.csv',
#             delim = ';-;')

# Abrindo dados limpos manualmente
cnet_superior_xlsx <-
  readxl::read_excel('controles/qualidade/cnet_superior.xlsx',
                     sheet = 1, range = 'A1:F121') %>%
  # Vide comentário anterior sobre 'Â'
  mutate(desc_comp = str_remove_all(desc_comp, 'Â')) %>%
  select(desc_comp, qualidade)

# Juntando dados limpos
cnet_superior <- cnet_superior %>%
  inner_join(cnet_superior_xlsx, by = 'desc_comp')

# Atualizando base a ser limpa
cnet_desc_to_be_cleaned3 <- cnet_desc_to_be_cleaned2 %>%
  anti_join(cnet_superior, by = 'desc_comp')

# Selecionando observacoes que parecem ser de qualidade tradicional
cnet_tradicional <- cnet_desc_to_be_cleaned3 %>%
  filter(str_detect(desc_comp, '(TIPO|QUALIDADE) TRADICIONAL')) %>%
  mutate(superior_context = str_extract(desc_comp, '.{15}(TIPO|QUALIDADE) TRADICIONAL.{15}')) %>%
  mutate(escala_context = str_extract(desc_comp, '.{30}ESCALA( SENSORIAL)+.{15}')) %>%
  mutate(minimo_context = str_extract(desc_comp, '.{15}QUALIDADE (GLOBAL )?MINIMA.{20}'))

# Salvando para limpeza manual
# write_delim(cnet_tradicional, 'controles/qualidade/cnet_tradicional.csv', delim = ';-;')

# Todas as observacoes de cnet_tradicional sao de qualidade tradicional
cnet_tradicional$qualidade <- 'TRADICIONAL'

# Atualizado base a ser limpa
cnet_desc_to_be_cleaned4 <- cnet_desc_to_be_cleaned3 %>%
  anti_join(cnet_tradicional, by = 'desc_comp')

# Tradicional: segunda etapa
cnet_tradicional2 <- cnet_desc_to_be_cleaned4 %>%
  filter(str_detect(desc_comp, 'TRADICIONAL')) %>%
  mutate(superior_context = str_extract(desc_comp, '.{15}TRADICIONAL.{15}')) %>%
  mutate(escala_context = str_extract(desc_comp, '.{30}ESCALA( SENSORIAL)+.{15}')) %>%
  mutate(minimo_context = str_extract(desc_comp, '.{15}QUALIDADE (GLOBAL )?MINIMA.{20}'))

# Salvando para limpeza manual
# write_delim(cnet_tradicional2, 'controles/qualidade/cnet_tradicional2.csv',
#             delim = ';-;')

# Abrindo xlsx com dados limpos manualmente
cnet_tradicional2_xlsx <-
  readxl::read_excel('controles/qualidade/cnet_tradicional2.xlsx',
                     sheet = 1, range = 'A1:F71')  %>%
  # Vide comentário anterior sobre 'Â'
  mutate(desc_comp = str_remove_all(desc_comp, 'Â')) %>%
  select(desc_comp, qualidade)

# Juntando dados limpos
cnet_tradicional2 <- cnet_tradicional2 %>%
  inner_join(cnet_tradicional2_xlsx, by = 'desc_comp')

# Atualizando base a ser limpa
cnet_desc_to_be_cleaned5 <- cnet_desc_to_be_cleaned4 %>%
  anti_join(cnet_tradicional2, by = 'desc_comp')

# Salvando em csv para limpeza manual no excel
# cnet_desc_to_be_cleaned5 %>%
#   write_delim('controles/qualidade/cnet_desc_to_be_cleaned5.csv',
#               delim = ';-;')

# Abrindo xlsx com dados limpos manualmente
cnet_desc_to_be_cleaned5_xlsx <-
  readxl::read_excel('controles/qualidade/cnet_desc_to_be_cleaned5.xlsx',
                     sheet = 1, range = 'A1:C838') %>%
  # Vide comentário sobre 'Â'; nesse caso, tivemos alguns erros adicionais
  mutate(desc_comp =  str_remove_all(desc_comp, 'Â') %>%
           str_replace_all('CINQÃœENTA', 'CINQÜENTA') %>%
           str_replace_all('Ã–RGANICO', 'ÖRGANICO')) %>%
  select(desc_comp, qualidade)

# Juntando dados limpos
cnet_desc_to_be_cleaned5 <- cnet_desc_to_be_cleaned5 %>%
  inner_join(cnet_desc_to_be_cleaned5_xlsx, by = 'desc_comp')

# Montando base com descricoes que ficaram de fora de cnet_sp_desc_clean/cnet_sp_qualidade
cnet_desc_clean_sem_sp <- cnet_desc_to_be_cleaned5 %>%
  bind_rows(
    cnet_tradicional2 %>% select(desc_comp, n, qualidade),
    cnet_tradicional %>% select(desc_comp, n, qualidade),
    cnet_superior %>% select(desc_comp, n, qualidade),
    cnet_pontos %>% select(desc_comp, n, qualidade)
  ) %>%
  # Ajustes finais
  mutate(qualidade =
           ifelse(str_detect(desc_comp, 'DESCAFEINADO'),
                  'DESCAFEINADO', qualidade)) %>%

  mutate(qualidade =
           ifelse(is.na(qualidade) & str_detect(desc_comp, 'GOURMET'),
                  'GOURMET', qualidade)) %>%

  mutate(qualidade =
           ifelse(str_detect(desc_comp, 'EXTRAFORTE|EXTRA FORTE|EXTRA-FORTE') & is.na(qualidade),
                  'TRADICIONAL', qualidade)) %>%

  # 2 variaveis de qualidade: NA como TRADICIONAL ou NAO INFORMADA
  mutate(qualidade2 = ifelse(is.na(qualidade), 'NAO INFORMADA', qualidade),
         qualidade = ifelse(is.na(qualidade), 'TRADICIONAL', qualidade)) %>%

  # Considerando café ORGANICO como GOURMENT
  mutate(qualidade = ifelse(qualidade == 'ORGANICO', 'GOURMET', qualidade),
         qualidade2 = ifelse(qualidade2 == 'ORGANICO', 'GOURMET', qualidade2))

# saveRDS(cnet_desc_clean_sem_sp,
#         'controles/qualidade/cnet_qualidade_sem_sp.rds')

# Montando base com todos os dados da CNET ------------------------------------
# Juntando DF com descricoes presentes nos pregoes de SP
cnet_desc_clean_full <- cnet_desc_clean_sem_sp %>%
  bind_rows(cnet_sp_desc_clean)

# Joining com DF cnet que possui variavel id_item
cnet_qualidade <- cnet %>%
  left_join(cnet_desc_clean_full, by = 'desc_comp') %>%
  select(id_item, desc_comp, qualidade, qualidade2)

# saveRDS(cnet_qualidade, 'controles/qualidade/cnet_qualidade.rds')

# Juntando BEC e CNET completa em DF unico ------------------------------------
qualidade_full <- cnet_qualidade %>%
  rename(descricao = desc_comp) %>%
  # Para BEC, qualidade2 = qualidade, pois nao ha NA
  bind_rows(bec_qualidade %>% mutate(qualidade2 = qualidade)) %>%
  # Considerando café orgânico como gourmet
  mutate(qualidade = ifelse(qualidade == 'ORGANICO', 'GOURMET', qualidade),
         qualidade2 = ifelse(qualidade2 == 'ORGANICO', 'GOURMET', qualidade2))

saveRDS(qualidade_full, 'controles/qualidade/controles_qualidade.rds')

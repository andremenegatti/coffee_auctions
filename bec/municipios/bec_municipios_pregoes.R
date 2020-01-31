library(PregoesBR)

df_bec <- readRDS("bec/montando_base_v2/bec_cafe_v2.rds")

# Corrigindo erros comuns -----------------------------------------------------
df_mun <- df_bec %>%
  select(id_item, municipio) %>%
  mutate(municipio_clean = substituir_caracteres_especiais(municipio) %>%
           str_remove('.SP') %>%
           str_remove('S.P.') %>%
           str_remove('/') %>%
           str_replace('.*(PRESIDENTE|PRES) BERNARDES.*',
                       'PRESIDENTE BERNARDES') %>%
           str_replace('.*(PRESIDENTE|PRES) VENCESLAU*',
                       'PRESIDENTE VENCESLAU') %>%
           str_replace('.*(PRESIDENTE|PRES) PRUDENTE*',
                       'PRESIDENTE PRUDENTE') %>%
           str_remove_all('\\.') %>%
           str_remove('- ') %>%
           str_remove(' -') %>%
           str_remove_all(',') %>%
           str_remove('-?CEP.*') %>%
           str_remove(' ?VALE DO PARAIBA') %>%
           str_remove('ENTREGE.*') %>%
           str_remove(' SP') %>%
           str_remove('DAS \\d{2}.*') %>%
           str_remove('ENTREGA .*') %>%
           str_replace('S@O', 'SAO') %>%
           str_replace('S`A', 'SAO') %>%
           str_remove('\\(.*\\)') %>%
           str_remove('-$') %>%
           str_trim())

# Primeira limpeza manual -----------------------------------------------------
# Salvando csv para limpeza manual
df_mun %>%
  count(municipio_clean) %>%
  arrange(municipio_clean) %>%
  write.csv2('municipios_bec_cleaning.csv')

# Lendo resultados da limpeza manual
municipios_bec_cleaning <-
  readxl::read_excel('bec/municipios/municipios_bec_cleaning.xlsx',
                     sheet = 1) %>%
  mutate(municipio_clean = ifelse(is.na(municipio_clean), "", municipio_clean))

# Juntando dados
df_mun <- df_mun %>%
  left_join(municipios_bec_cleaning, by = 'municipio_clean')

# Inspecionando
count(df_mun, municipio_clean_manual) %>% arrange(-n)

# Segunda limpeza manual: extraindo municipio do campo 'unidade_compradora'----
# Identificando pregoes em que o campo 'municipio' nao foi informativo
df_na <- df_bec %>%
  semi_join(filter(df_mun, is.na(municipio_clean_manual)),
            by = 'id_item')

# Salvando csv com respectivas unidades compradoras para limpeza manual
df_na %>%
  distinct(unidade_compradora) %>%
  write.csv2('unidade_compradora_na_municipio_bec.csv', row.names = FALSE)

# Lendo dados processados manualmente
unidade_compradora_na_municipio <-
  readxl::read_excel('bec/municipios/unidade_compradora_na_municipio_bec.xlsx',
                     sheet = 1)

# Juntando dados processados na base df_na
df_mun_unid_comp <- df_na %>%
  left_join(unidade_compradora_na_municipio, by = c("unidade_compradora")) %>%
  select(id_item, municipio_clean_manual2 = municipio_unidade_compradora)

# Juntando base do bloco acima na base df_mun, com os dados limpos
df_mun <- df_mun %>%
  left_join(df_mun_unid_comp, by = 'id_item') %>%
  # 'municipio_clean_manual2' incorpora todas as etapas de limpeza até agora
  mutate(municipio_clean_manual2 = ifelse(is.na(municipio_clean_manual2),
                                          municipio_clean_manual,
                                          municipio_clean_manual2))

# Inspecionando
count(df_mun, municipio_clean_manual2) %>% View()

# Terceira etapa de limpeza manual: checando atas -----------------------------
# DF com os municipios que ainda nao foram identificados
df_na2 <- df_mun %>%
  filter(is.na(municipio_clean_manual2))

# Salvando csv para limpeza manual
df_na2 %>%
  select(id_item) %>%
  mutate(id_pregao = str_extract(id_item, '.{21}')) %>%
  mutate(no_item = str_remove(id_item, id_pregao)) %>%
  left_join(df_bec %>% select(id_item, unidade_compradora),
            by = 'id_item') %>%
  write.csv2('id_item_na_municipio_bec.csv', row.names = FALSE)

# Lendo resultados da limpeza manual
id_item_na_municipio <-
  readxl::read_excel('bec/municipios/id_item_na_municipio_bec.xlsx',
                     sheet = 1)

# Juntando dados limpos manualmente
df_mun <- df_mun %>%
  left_join(id_item_na_municipio %>% select(-unidade_compradora),
            by = 'id_item')

# Coluna 'municipio_clean_manual3' incorpora todas as etapas de limpeza
df_mun <- df_mun %>%
  mutate(municipio_clean_manual3 = ifelse(is.na(municipio_clean_manual3),
                                          municipio_clean_manual2,
                                          municipio_clean_manual3))

# Inspecionando resultados
count(df_mun, municipio_clean_manual3) %>% View()

# Salvando --------------------------------------------------------------------
bec_municipios_pregoes <- df_mun %>%
  select(id_item, municipio = municipio_clean_manual3)

saveRDS(bec_municipios_pregoes, 'data/bec_municipios_pregoes.rds')

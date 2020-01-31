# O script a seguir foi utilizado para organizar os resultados da coleta de dados 
# (dados gerais, itens e licitacoes tipo 5)

library(tidyverse)
library(lubridate)

# Pregoes com problemas nos dados de itens
tilt <- c(28115, 142368, 237812)

# DADOS GERAIS
df_dados_gerais <- dados %>%
  mutate_at(vars(starts_with("dt")), ymd) %>%
  mutate_if(is.character, toupper) %>% 
  mutate_at(vars(starts_with("ds_")), as.factor) %>% 
  mutate(indice = 1:nrow(dados),
         num_itens = numero_itens) %>% 
  select(indice, id_pregao, num_itens, co_processo:link_pregao) %>% 
  filter(!indice %in% tilt)

# ITENS
df_itens <- tibble(indice = 1:length(lista_dados_itens),
                   id_pregao = dados$id_pregao,
                   df_itens = lista_dados_itens) %>% 
  filter(!indice %in% tilt) %>%
  # Criando coluna com indice do item, em todos os dataframes (ie, para todos os pregoes)
  mutate(df_itens = map(.x = df_itens,
                           ~ .x %>% mutate(indice_item = 1:nrow(.x)))) %>% 
  unnest()
  
df_itens <- df_itens %>% 
  mutate(descricao_item_original = descricao_item,
         descricao_item = toupper(descricao_item),
         quantidade_item = as.integer(quantidade_item), # MUDAR PARA AS.NUMERIC??
         valor_estimado_item = as.double(valor_estimado_item), # MUDAR PARA AS.NUMERIC??
         descricao_detalhada_item_original = descricao_detalhada_item,
         descricao_detalhada_item = toupper(descricao_detalhada_item),
         situacao_item = factor(toupper(situacao_item)),
         menor_lance = as.double(menor_lance)) # MUDAR PARA AS.NUMERIC??


# PREGOES MODALIDADE 5
df_modalidade5 <- dados_modalidade5 %>% 
  mutate_at(vars(starts_with("data_entrega_")), ymd_hms) %>% 
  mutate(data_publicacao = ymd(data_publicacao),
         tipo_pregao = as.factor(tipo_pregao),
         situacao_aviso = as.factor(situacao_aviso),
         tipo_recurso = as.factor(tipo_recurso),
         endereco_entrega_edital = if_else(endereco_entrega_edital == " - /", "Nao informado", endereco_entrega_edital),
         objeto = toupper(objeto))


saveRDS(df_itens, "df_itens.rds")
saveRDS(df_dados_gerais, "df_dados_gerais.rds")

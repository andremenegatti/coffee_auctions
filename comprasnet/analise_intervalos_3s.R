library(tidyverse)

df_bid_inc_unnested <- readRDS('Comprasnet/cnet_df_bid_inc_unnested.rds')

# Excluindo outliers
cut <- quantile(df_bid_inc_unnested$intervalo_menor_lance, 0.975)

df_intervalo <- df_bid_inc_unnested %>%
  filter(intervalo_menor_lance < cut)

# Proporcao de leiloes que violam a regra dos 3s: 0,2%
df_intervalo %>%
  group_by(regime_juridico) %>%
  summarise(n_lances = n(),
            n_violacao_3s = sum(ifelse(intervalo_menor_lance < 3, 1, 0)),
            perc_violacao = n_violacao_3s / n_lances * 100)

# Identificando os casos que violam a regra dos 3s
df_contra_3s <- df_intervalo %>%
  filter(intervalo_menor_lance < 3,
         regime_juridico == 'Regra 20s + Regra 3s')

# Exemplo de leilao em que ha uso de robo:39302400039020170002
## Empresa que usa robo: 10.851.944/0001-26

# Exemplo de leilao em que ha uso de robo:17017900000220170003
## Empresa que usa robo: 28.337.943/0001-23

table(df_contra_3s$CNPJ_CPF) %>% View()

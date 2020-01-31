library(PregoesBR)
library(plotly)

#### IMPORTANDO DADOS ####
df_bid_inc <- readRDS('Comprasnet/cnet_df_bid_inc.rds')

df_lances_completo <- readRDS('Comprasnet/df_lances_completo.rds')

pregoes_possivel_robo <- readRDS('Comprasnet/analise_robos_pre-tratamento/pregoes_possivel_robo.rds')

df_forn_lances_por_pregao <- readRDS('Comprasnet/df_forn_lances_por_pregao.rds')

df_forn_total_lances_e_pregoes <- readRDS('Comprasnet/df_forn_total_lances_e_pregoes_por_regime.rds') %>%
  filter(regime_juridico == 'Sem intervalo minimo') ### <<<<---- Pode nao ser necessario

df_atas <- readRDS('Comprasnet/cnet_cafe_01_v3.rds') %>%
  select(id_item, inicio_fase_aleatoria)


#### DATA-WRANGLING ####

# Relacao dos 10 fornecedores que mais registraram lances
dez_mais_lances <- df_forn_total_lances_e_pregoes$CNPJ_CPF[1:10]

# DF com pregoes em que os tres fornecedores com mais lances no periodo pre-tratamento participaram
pregoes_possivel_robo <- filter(df_forn_lances_por_pregao, CNPJ_CPF %in% dez_mais_lances[1:3])

# Tambem podemos pegar os leiloes que estao no topo de df_forn_lances_por_pregao
df_forn_lances_por_pregao %>%
  filter(abertura_lances > data_3s)

df_forn_lances_por_pregao %>%
  filter(abertura_lances > data_3s) %>%
  group_by(id_item, abertura_lances) %>%
  summarise(n_lances = sum(n)) %>%
  arrange(desc(n_lances))

# Selecionando os lances do id_item 15407000005720110030
exemplo <- df_lances_completo %>%
  # filter(id_item == pregoes_possivel_robo$id_item[1]) %>%
  filter(id_item == "17017700000420150012") %>%
  # select(CNPJ_CPF, data_hora, valor_lance, valor_lance_corr_defl,
  #        increment, intervalo_lance_anterior, intervalo_menor_lance) %>%
  group_by(CNPJ_CPF) %>%
  mutate(n_lances = n()) %>%
  ungroup() %>%
  left_join(df_atas, by = 'id_item')

# Construindo legendas de letras para os CNPJs
exemplo_temp <- exemplo %>%
  select(CNPJ_CPF, n_lances) %>%
  distinct() %>%
  arrange(desc(n_lances))

exemplo_legendas1 <- exemplo_temp %>%
  slice(1:5) %>%
  mutate(Fornecedor = str_c('Fornecedor ', LETTERS[1:n()]))

exemplo_legendas2 <- exemplo_temp %>%
  anti_join(exemplo_legendas1, by = 'CNPJ_CPF') %>%
  mutate(Fornecedor = 'Outros fornecedores')

exemplo_legendas <- bind_rows(exemplo_legendas1, exemplo_legendas2)

# Juntando legendas na base original
exemplo <- exemplo %>%
  left_join(exemplo_legendas, by = c('CNPJ_CPF', 'n_lances'))


#### Plotly ####
exemplo %>%
  plot_ly(x = ~ data_hora, y = ~ valor_lance,
          hoverinfo = "text",
          text = ~ paste("<b>Fornecedor:</b> ", Fornecedor,
                        "<br><b>Data/Hora:</b> ", data_hora,
                        "<br><b>Lance:</b> R$", valor_lance %>% format(decimal.mark = ',', big.mark = '.'),
                        "<br><b>Lance/kg:</b> R$", round(valor_lance_corr_defl, digits = 2) %>% format(decimal.mark = ',', big.mark = '.'),
                        "<br><b>Desconto total:</b>: R$", round(-1*desconto_bruto, digits = 3) %>% format(decimal.mark = ',', big.mark = '.'),
                        "<br><b>Desconto/kg:</b> R$", round(-1*increment, digits = 3) %>% format(decimal.mark = ',', big.mark = '.'),
                        "<br><b>Intervalo lance anterior:</b> ", round(intervalo_lance_anterior, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s",
                        "<br><b>Intervalo menor lance:</b> ", round(intervalo_menor_lance, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s",
                        "<br><b>Intervalo lance próprio:</b> ", round(intervalo_lance_proprio, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s"
                        )
          )  %>%
  add_lines(color = ~ Fornecedor, opacity = 0.4) %>%
  add_markers(color = ~ Fornecedor, opacity = 0.8, showlegend = FALSE) %>%
  add_segments(x = ~ mean(inicio_fase_aleatoria), xend = ~ mean(inicio_fase_aleatoria), y = ~min(valor_lance), yend = ~max(valor_lance),
               color = I("red"), opacity = 0.8, name = 'Início fase aleatória') %>%
  layout(title = 'Exemplo de leilão com grande numero de lances apos a regra dos 3s - Pregão n. 1701770000042015, Superintendencia Regional da Receita Federal - 10ª Região Fiscal, item 12',
         xaxis = list(title = 'Horário de registro do lance'),
         yaxis = list(title = 'Valor do lance (reais)'))





#### ggplot2 ####

# Grafico interativo
exemplo %>%
  ggplot() +
  geom_line(aes(x = data_hora, y = valor_lance, group = Fornecedor, color = Fornecedor, linetype = Fornecedor),
            alpha = 0.4, size = 1) +
  geom_point(aes(x = data_hora, y = valor_lance, color = Fornecedor),
             alpha = 0.6, shape = 1) +
  labs(x = "Tempo (hh:mm)",
       y = "Valor do lance (reais)",
       title = "Exemplo de possível uso de robôs - Pregão n. 57/2011, Fundação Universidade Federal de Mato Grosso, item 30")

plotly::ggplotly()



# Fazendo graficos de todos os pregoes
df_plots <- df_lances_completo %>%
  filter(regime_juridico == 'Regra 20s + Regra 3s') %>%
  group_by(id_item, regime_juridico) %>%
  nest() %>%
  left_join(df_atas, by = 'id_item') %>%
  mutate(total_lances = map_dbl(.x = data,
                                .f = ~ nrow(.x)
  )
  ) %>%
  filter(total_lances > 100) %>%
  arrange(desc(total_lances)) %>%
  mutate(bid_plots = pmap(.l = list(data, inicio_fase_aleatoria, id_item, regime_juridico),
                          .f = ~ ..1 %>%
                            group_by(CNPJ_CPF) %>%
                            mutate(n_lances = n()) %>%
                            ungroup() %>%
                            mutate(ranking_lances = dense_rank(desc(n_lances))) %>%
                            filter(ranking_lances < 6) %>%
                            # filter(data_hora >= ..2) %>%
                            ggplot(aes(x = data_hora, y = valor_lance)) +
                            geom_point(aes(color = CNPJ_CPF)) +
                            geom_smooth(method = 'loess', se = FALSE, alpha = 0.6) +
                            geom_vline(aes(xintercept = ..2)) +
                            labs(x = "Horário de registro do lance", y = "Valor do lance (reais)", title = ..3, subtitle = ..4)
  )
  )

walk(.x = 1:25, .f = ~ print(df_plots$bid_plots[[.x]]))


library(tidyverse)

# Importando dados ------------------------------------------------------------
df_lances_completo <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < lubridate::ymd('2016-01-01')) %>%
  select(-regime_juridico_20s, -regime_juridico_3s)

df_atas <- readRDS('data/cnet_cafe.rds') %>%
  select(id_item, abertura_lances, inicio_fase_aleatoria, encerramento_lances,
         situacao, tratamento_diferenciado, decreto_7174, margem_preferencia,
         melhor_lance, valor_negociado, kg_fornecidos, kg_por_unid,
         sigla_uf, municipio, nome_uasg, menor_proposta,
         menor_proposta_global, menor_lance, valor_estimado,
         win_bid_kg, reserve_kg, ano, avg_bids_per_bidder,
         num_forn_propostas, num_forn_lances)

# Data wrangling --------------------------------------------------------------
# Nesting df_lances no nível do leilão
df_random <- df_lances_completo %>% 
  left_join(df_atas %>%
              select(id_item, inicio_fase_aleatoria, encerramento_lances),
            by = 'id_item') %>% 
  filter(data_hora >= inicio_fase_aleatoria) %>% 
  mutate(duracao_fase_aleatoria = 
           difftime(encerramento_lances, inicio_fase_aleatoria,
                    units = 'mins') %>% as.numeric())

# Contando lances para cada grupo de leilões ----------------------------------
df_n_bids_all <- df_random %>% 
  group_by(id_item, regime_juridico, CNPJ_CPF) %>% 
  summarise(n_bids = n(),
            bids_per_min = n_bids / mean(duracao_fase_aleatoria)) %>% 
  arrange(desc(n_bids)) %>% 
  ungroup()

df_n_bids_jose <- df_n_bids_all %>% 
  filter(CNPJ_CPF == '22.122.311/0001-66') %>% 
  mutate(grupo = 'José')

df_n_bids_ivy <- df_n_bids_all %>% 
  filter(CNPJ_CPF == '11.221.523/0001-84') %>% 
  mutate(grupo = 'Ivy')

df_n_bids_sao_luis <- df_n_bids_all %>% 
  filter(CNPJ_CPF == '13.434.138/0001-40') %>% 
  mutate(grupo = 'São Luís')

df_n_bids_quality <- df_n_bids_all %>% 
  filter(CNPJ_CPF == '06.946.072/0001-02')  %>% 
  mutate(grupo = 'Quality')

df_n_bids_others <- df_n_bids_all %>% 
  filter(!CNPJ_CPF %in% c('22.122.311/0001-66',
                          '11.221.523/0001-84',
                          '13.434.138/0001-40',
                          '06.946.072/0001-02')) %>% 
  mutate(grupo = 'Outros')

# Juntando todos
df_plot <- df_n_bids_others %>% 
  bind_rows(df_n_bids_jose, df_n_bids_ivy,
            df_n_bids_sao_luis, df_n_bids_quality) %>% 
  mutate(grupo = fct_relevel(grupo, c('Outros', 'Ivy', 'Quality',
                                      'São Luís', 'José'))) %>% 
  group_by(grupo, regime_juridico) %>% 
  mutate(mediana_n_bids = median(n_bids),
         mediana_bids_per_min = median(bids_per_min)) %>% 
  ungroup()

# Faceted density plot - n_bids -----------------------------------------------
df_plot %>% 
  ggplot() +
  geom_density(aes(x = n_bids, fill = grupo),
               alpha = 0.2) +
  geom_vline(aes(xintercept = mediana_n_bids, col = grupo),
             linetype = 'dotted') +
  scale_x_log10() +
  scale_y_continuous(labels = function(x) formatC(x, big.mark = '.',
                                                  decimal.mark = ',')) +
  facet_grid(grupo ~ regime_juridico) +
  guides(fill = FALSE, col = FALSE) +
  labs(
    x = 'Número de lances (escala logarítmica)',
    y = 'Densidade de probabilidade',
    title = 'Distribuição do número de lances por pregão',
    subtitle = 'Possíveis robôs vs. Outros, por regime jurídico',
    caption = 'Nota: as linhas verticais indicam a mediana de cada distribuição.'
    )

# ggsave('plots/bot_search/density_plot_robos_vs_outros_por_regime.png',
#        width = 8, height = 8)

# Faceted density plot - Bids per minute --------------------------------------
df_plot %>% 
  ggplot() +
  geom_density(aes(x = bids_per_min, fill = grupo),
               alpha = 0.2) +
  geom_vline(aes(xintercept = mediana_bids_per_min, col = grupo),
             linetype = 'dotted') +
  scale_x_log10() +
  scale_y_continuous(labels = function(x) formatC(x, big.mark = '.',
                                                  decimal.mark = ',')) +
  facet_grid(grupo ~ regime_juridico) +
  guides(fill = FALSE, col = FALSE) +
  labs(
    x = 'Número de lances (escala logarítmica)',
    y = 'Densidade de probabilidade',
    title = 'Distribuição do número de lances por minuto em cada pregão',
    subtitle = 'Possíveis robôs vs. Outros, por regime jurídico',
    caption = 'Nota: as linhas verticais indicam a mediana de cada distribuição.'
    )

# ggsave('plots/bot_search/density_plot_bids_per_minute_robos_vs_outros_por_regime.png',
#        width = 8, height = 8)


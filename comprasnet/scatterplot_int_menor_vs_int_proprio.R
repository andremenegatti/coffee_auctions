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
  filter(data_hora >= inicio_fase_aleatoria)

df_ints <- df_random %>% 
  filter(norm_inc_first < 0) %>% 
  filter(!is.na(intervalo_proprio), !is.na(intervalo_menor)) %>%
  PregoesBR::trim_df(vars = c('intervalo_menor', 'intervalo_proprio'),
                     perc = 0.5, tail = 'lower') %>% 
  mutate(regime_juridico = str_replace_all(regime_juridico, 'mini', 'míni') %>% 
           fct_relevel('Sem intervalo mínimo', 'Regra 20s',
                       'Regra 20s + Regra 3s')) %>%
  filter(intervalo_menor < 100, intervalo_proprio < 100) 


# Plotting --------------------------------------------------------------------
df_ints %>% 
  ggplot(aes(x = intervalo_proprio, y = intervalo_menor)) +
  geom_point(alpha = 0.05, shape = 1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~regime_juridico, nrow = 3) +
  geom_vline(xintercept = 20, col = 'darkred', alpha = 0.8) +
  geom_hline(yintercept = 3, col = 'darkred', alpha = 0.8) +
  labs(
    x = 'Segundos desde o último lance do mesmo fornecedor',
    y = 'Segundos desde o menor lance',
    title = 'Relação entre intervalos - Menor lance e lance próprio',
    subtitle = 'Comparação entre os diferentes regimes jurídicos',
    caption = 'Notas:
    i) Ambos os eixos estão em escala logarítmica;
    ii) Foram omitidas algumas observações com intervalos muito elevados ou baixos;
    iii) As linhas representam os intervalos mínimos de 20s e 3s.'
  )

ggsave('plots/scatterplot_int_menor_vs_int_proprio.png', width = 6, height = 7)

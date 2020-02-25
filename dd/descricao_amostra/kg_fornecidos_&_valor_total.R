library(tidyverse)
library(PregoesBR)

# Loading data ----------------------------------------------------------------
bec <- readRDS('data/bec_cafe_dd.rds')
cnet <- readRDS('data/cnet_cafe_dd.rds')
cnet_sp <- readRDS('data/cnet_sp_cafe_dd.rds')

# Data wrangling --------------------------------------------------------------
data_list <- list(bec, cnet, cnet_sp) %>%
  map(.f = ~ PregoesBR::trim_df(.x, 'win_bid_kg', perc = 2.5) %>%
        select(id_item, abertura_lances,
               inicio_ano, inicio_trimestre, inicio_bimestre, inicio_mes,
               win_bid_kg, quantidade, kg_fornecidos, kg_por_unid, num_forn_lances,
               comprasnet, sigla_uf, municipio, unidade_compradora,
               unidade_compradora_lasso, marca_vencedor_principais,
               futuro_defl, arab_defl,
               qualidade, qualidade2
        ) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(regime_juridico = case_when(
          abertura_lances < data_20s ~ 'Sem intervalo mínimo',
          abertura_lances >= data_20s & abertura_lances < data_3s ~ 'Regra 20s',
          abertura_lances >= data_3s ~ 'Regra 20s + Regra 3s'
        ) %>% fct_relevel('Sem intervalo mínimo', 'Regra 20s', 'Regra 20s + Regra 3s'))
  ) %>%
  set_names(c('bec', 'cnet', 'cnet_sp'))

df_stacked <-  data_list$bec %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(data_list$cnet %>% mutate(Grupo = "Tratamento")) %>%
  bind_rows(data_list$cnet_sp %>% mutate(Grupo = "Tratamento - SP"))

# Quantidade: Summary statistics ----------------------------------------------
summary_table_quantidade <- df_stacked %>%
  group_by(Grupo) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos, na.rm = TRUE),
            Mediana = median(kg_fornecidos, na.rm = TRUE),
            SD = sd(kg_fornecidos, na.rm = TRUE)) ; summary_table_quantidade

# Quantidade: Violinplot ------------------------------------------------------
df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000),
                labels = c('10', '100', '1 mil', '10 mil', '100 mil')) +
  geom_text(data = summary_table_quantidade,
             aes(x = Grupo, y = Mediana, label = Mediana),
             size = 3, nudge_x = 0.17, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Quilogramas fornecidos (escala logarítmica)',
    title = 'Distribuição da quantidade negociada nos pregões',
    subtitle = 'Comparação entre grupos de tratamento e controle',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  )

ggsave('plots/violinplot_quantidade.png', width = 6, height = 6)


# Valor total: Summary statistics ---------------------------------------------
summary_table_valor <- df_stacked %>%
  group_by(Grupo) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos * win_bid_kg, na.rm = TRUE),
            Mediana = median(kg_fornecidos * win_bid_kg, na.rm = TRUE),
            SD = sd(kg_fornecidos * win_bid_kg,
                    na.rm = TRUE)) ; summary_table_valor

# Valor total: Violinplot -----------------------------------------------------
df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos * win_bid_kg/ 1e+3)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000),
                labels = c('0,1', '1', '10', '100', '1000')) +
  geom_text(data = summary_table_valor,
            aes(x = Grupo, y = Mediana / 1e+3,
                label = round(Mediana/1e+3, digits = 2) %>% 
                  str_replace('\\.', ',')),
            size = 3, nudge_x = 0.17, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Valor total (milhares de reais, escala logarítmica)',
    title = 'Distribuição do valor negociado nos pregões',
    subtitle = 'Comparação entre grupos de tratamento e controle',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  )

ggsave('plots/violinplot_valor_total.png', width = 6, height = 6)

# Quantidade por regime juridico: Summary Statistics --------------------------
summary_table_quantidade_regime <- df_stacked %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos, na.rm = TRUE),
            Mediana = median(kg_fornecidos, na.rm = TRUE),
            Máximo = max(kg_fornecidos, na.rm = TRUE),
            SD = sd(kg_fornecidos,
                    na.rm = TRUE)) ; summary_table_quantidade_regime

# Quantidade por regime juridico: violinplot ----------------------------------
df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000),
                labels = c('10', '100', '1 mil', '10 mil', '100 mil')) +
  geom_text(data = summary_table_quantidade_regime,
            aes(x = Grupo, y = Mediana, label = Mediana),
            size = 2.5, nudge_x = 0.35, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Quilogramas fornecidos (escala logarítmica)',
    title =
      'Distribuição da quantidade negociada nos pregões',
    subtitle = 
      'Comparação entre grupos de tratamento e controle, nos diferentes regimes jurídicos',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  ) +
  theme(axis.text.x = element_text(angle = 25, size = 9, hjust = 1),
        axis.title.x = element_blank()) +
  facet_grid(. ~ regime_juridico)

ggsave('plots/violinplot_kg_fornecidos_regimes.png', width = 7, height = 6)

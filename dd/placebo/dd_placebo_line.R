library(tidyverse)
# Outros pacotes necessarios: broom, lfe, viridis

# Executando script auxiliar
source('helpers_placebo.R')

# Carregando base de dados
dd_sp <- readRDS('base_dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Opcional: Definindo tema para graficos mais bonitos
theme_set(my_theme())

# --------------------------------------------- #
#### ALTERANDO APENAS A DATA DA REGRA DOS 3s ####
# ------------------------------------------ -- #

# Usando a data verdadeira da regra dos 20s

# Definindo janela
bandwidth <- -180:180 # 180 dias antes e 180 depois
# Nota: como estamos variando apenas uma das datas, o problema fica computacionalmente mais leve
# Logo, fica facil usar uma janela maior

# Rodando modelos placebo
placebo_df_3s <-
  # Criando DF com coluna com datas alternativas da regra dos 3s
  tibble(data_3s_alt = bandwidth + data_3s) %>%
  # Adicionando colunas com dummies de tratamento considerando as datas alternativas
  mutate(
    treat1 = map(
      .x = data_3s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x, 1, 0)
    ),
    treat2 = map(
      .x = data_3s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances > .x, 1, 0)
    )
  ) %>%
  # Criando coluna de DFs com os resultados dos modelos
  mutate(
    model_summary = map2(
      .x = treat1,
      .y = treat2,
      .f = ~ lfe::felm(
        log_win_bid ~ .x + .y + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais
      ) %>%
        # Extraindo resultados de forma limpa e organizada usando pacote broom
        broom::tidy() %>%
        # Substituindo '.x' por 'treat1' e '.y' por 'treat2' nos DFs de resultados
        mutate(
          term = case_when(
            term == '.x' ~ 'treat1',
            term == '.y' ~ 'treat2',
            !term %in% c('.x', '.y') ~ term
          )
        )
    )
  ) %>%
  # Usando funcao definida no script auxiliar para extrair dados dos resultados
  # salvos na coluna 'model_summary' e criar colunas com coeficientes e erros padrao
  get_tidy_estimates()

# Grafico - Efeito Regra dos 3s
ggplot(data = placebo_df_3s) +
  geom_line(aes(x = data_3s_alt, y = treat2_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_3s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_3s_alt, y = treat2_upper), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_3s_alt, y = treat2_lower), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0) +
  labs(title = 'Teste Placebo - Efeito da Regra dos 3s',
       subtitle = 'Considerando datas alternativas para a regra dos 3s',
       caption = 'Notas:,
       1) Utilizou-se janela de 360 dias ao redor da verdadeira data da regra dos 3s;
       2) A data da regra dos 20s foi mantida constante;
       3) A linha vertical pontilhada indica a verdadeira data da regra dos 3s;
       4) As linhas hachuradas indicam o intervalo de confiança de 95%.',
       x = 'Data da regra dos 3s',
       y = 'Estimativa do efeito (coeficiente)')
# ggsave('placebo_efeito3s_20sfixa.png', height = 5, width = 8)

# Grafico - Efeito Regra dos 20s
ggplot(data = placebo_df_3s) +
  geom_line(aes(x = data_3s_alt, y = treat1_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_3s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_3s_alt, y = treat1_upper), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_3s_alt, y = treat1_lower), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0) +
  labs(title = 'Teste Placebo - Efeito da Regra dos 20s',
       subtitle = 'Considerando datas alternativas para a regra dos 3s',
       caption = 'Notas:,
       1) Utilizou-se janela de 360 dias ao redor da verdadeira data da regra dos 3s;
       2) A data da regra dos 20s foi mantida constante;
       3) A linha vertical pontilhada indica a verdadeira data da regra dos 3s;
       4) As linhas hachuradas indicam o intervalo de confiança de 95%.',
       x = 'Data da regra dos 3s',
       y = 'Estimativa do efeito (coeficiente)')
# ggsave('placebo_efeito20s_20sfixa.png', height = 5, width = 8)


# ---------------------------------------------- #
#### ALTERANDO APENAS A DATA DA REGRA DOS 20S ####
# ---------------------------------------------- #

# Usando a data verdadeira da regra dos 3s

# Definindo janela
bandwidth <- -180:180 # 180 dias antes e 180 depois
# Nota: como estamos variando apenas uma das datas, o problema fica computacionalmente mais leve
# Logo, fica facil usar uma janela maior

# Rodando modelos placebo
placebo_df_20s <-
  # Criando DF com coluna com datas alternativas da regra dos 20s
  tibble(data_20s_alt = bandwidth + data_20s) %>%
  # Adicionando coluna com dummy de tratamento considerando as datas alternativas
  # Como treat2 nao depende da regra dos 20s, aqui precisamos criar apenas a coluna treat1
  mutate(
    treat1 = map(
      .x = data_20s_alt,
      .f = ~ if_else(comprasnet == 1 & abertura_lances >= .x & abertura_lances < data_3s, 1, 0)
    )
  ) %>%
  # Criando coluna de DFs com os resultados dos modelos
  mutate(
    model_summary = map(
      .x = treat1,
      .f = ~ lfe::felm(
        log_win_bid ~ .x + treat2 + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais
      ) %>%
        # Extraindo resultados de forma limpa e organizada usando pacote broom
        broom::tidy() %>%
        # Substituindo '.x' por 'treat1' nos DFs de resultados
        mutate(term = ifelse(term == '.x', 'treat1', term))
    )
  ) %>%
  # Usando funcao definida no script auxiliar para extrair dados dos resultados
  # e criar colunas com coeficientes e erros padrao
  get_tidy_estimates()


# Grafico - Efeito Regra dos 20s
ggplot(data = placebo_df_20s) +
  geom_line(aes(x = data_20s_alt, y = treat1_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_20s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_20s_alt, y = treat1_upper), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_20s_alt, y = treat1_lower), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0) +
  labs(title = 'Teste Placebo - Efeito da Regra dos 20s',
       subtitle = 'Considerando datas alternativas para a regra dos 20s',
       caption = 'Notas:,
       1) Utilizou-se janela de 360 dias ao redor da verdadeira data da regra dos 20s;
       2) A data da regra dos 3s foi mantida constante;
       3) A linha vertical pontilhada indica a verdadeira data da regra dos 20s;
       4) As linhas hachuradas indicam o intervalo de confiança de 95%.',
       x = 'Data da regra dos 20s',
       y = 'Estimativa do efeito (coeficiente)')
# ggsave('placebo_efeito20s_3sfixa.png', height = 5, width = 8)


# Grafico - Efeito Regra dos 3s
ggplot(data = placebo_df_20s) +
  geom_line(aes(x = data_20s_alt, y = treat2_est), linetype = 'solid', color = 'black') +
  geom_vline(xintercept = data_20s, color = 'darkred', linetype = 'dotted') +
  geom_line(aes(x = data_20s_alt, y = treat2_upper), linetype = 'dashed', color = 'black') +
  geom_line(aes(x = data_20s_alt, y = treat2_lower), linetype = 'dashed', color = 'black') +
  geom_hline(yintercept = 0) +
  labs(title = 'Teste Placebo - Efeito da Regra dos 3s',
       subtitle = 'Considerando datas alternativas para a regra dos 20s',
       caption = 'Notas:,
       1) Utilizou-se janela de 360 dias ao redor da verdadeira data da regra dos 20s;
       2) A data da regra dos 3s foi mantida constante;
       3) A linha vertical pontilhada indica a verdadeira data da regra dos 20s;
       4) As linhas hachuradas indicam o intervalo de confiança de 95%.',
       x = 'Data da regra dos 20s',
       y = 'Estimativa do efeito (coeficiente)')
# ggsave('placebo_efeito3s_3sfixa.png', height = 5, width = 8)

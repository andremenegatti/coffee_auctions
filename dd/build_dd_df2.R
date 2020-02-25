build_dd_df2 <- function(df) {
  df %>%
    mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_pub_3s, 1, 0),
           treat_ant = ifelse(comprasnet == 1 & abertura_lances >= data_pub_3s & abertura_lances < data_3s, 1, 0),
           treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
           # Criando dummies/factors de tempo
           trimestre = factor(inicio_trimestre),
           bimestre = factor(inicio_bimestre),
           mes = factor(inicio_mes),
           # Criando factors que serao utilizados como controles
           ## Incluindo prefixo para facilitar identificacao
           unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
           marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais)),
           kg_por_unid = as.factor(str_c('_', kg_por_unid)) %>% relevel('_0.5'),
           # Criando variaveis de tendencias (inicio em 0)
           trend_mes = dense_rank(inicio_mes) - 1,
           trend_bimestre = dense_rank(inicio_bimestre) - 1,
           trend_trimestre = dense_rank(inicio_trimestre) - 1
    ) %>%
    # Criando variavel de regime juridico
    ## Necessaria para criacao de tendencias de tratamento a la Chimeli & Soares
    mutate(regime_juridico = case_when(
      abertura_lances < data_20s ~ 1,
      abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
      abertura_lances >= data_3s ~ 3
    ) %>%
      as.factor()
    ) %>%
    # Agrupando por regime juridico para criacao das tendencias por regime
    group_by(regime_juridico) %>%
    mutate(indice_mes_por_regime = dense_rank(inicio_mes) - 1,
           indice_bimestre_por_regime = dense_rank(inicio_bimestre) - 1,
           indice_trimestre_por_regime = dense_rank(inicio_trimestre) - 1) %>%
    ungroup() %>%
    # Criando tendencias a la Chimeli & Soares
    mutate(treat1_trend_mes = treat1 * indice_mes_por_regime,
           treat2_trend_mes = treat2 * indice_mes_por_regime,
           treat1_trend_bimestre = treat1 * indice_bimestre_por_regime,
           treat2_trend_bimestre = treat2 * indice_bimestre_por_regime,
           treat1_trend_trimestre = treat1 * indice_trimestre_por_regime,
           treat2_trend_trimestre = treat2 * indice_trimestre_por_regime) %>%
    # Transformando todas as variaveis de texto em factors
    mutate_if(is.character, as.factor) %>%
    # Criando variavel log_win_bid
    mutate(log_win_bid = log(win_bid_kg)) %>%
    # Criando factor comprasnet
    mutate(comprasnet_factor = factor(comprasnet, levels = c(1, 0), labels = c('cnet', 'bec')),
           bec = ifelse(comprasnet == 1, 0, 1)) %>%
    mutate(qualidade = relevel(qualidade, ref = 'TRADICIONAL'),
           qualidade2 = relevel(qualidade2, ref = 'TRADICIONAL'))
  
}

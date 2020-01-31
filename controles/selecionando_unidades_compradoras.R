library(PregoesBR)
library(fastDummies)
library(glmnet)
library(leaps)

# Seed para reproducibildiade
set.seed(42)

# Definindo  variaveis --------------------------------------------------------
# Percentual de outliers a ser removido antes da seleção: metade de cada lado
perc_outliers <- 5

# Sufixo para salvar arquivos rds ao final do script
# Sugestao: 'soft_trim' para remover 2,5% de outliers e 'hard_trim' para 5%
filename_end <- 'hard_trim'

# Importando dados ------------------------------------------------------------
bec_cafe <- readRDS('data/bec_cafe_dd.rds') %>%
  mutate(unidade_compradora = unidade_compradora %>% factor() %>% as.numeric())

cnet_cafe <- readRDS('data/cnet_cafe_dd.rds') %>%
  mutate(unidade_compradora = str_extract(id_item, '\\d{6}'))

cnet_cafe_sp <- readRDS('data/cnet_cafe_sp_dd.rds') %>%
  mutate(unidade_compradora = str_extract(id_item, '\\d{6}'))

# Montando DFs de "primeiro estagio" ------------------------------------------
data_list <- list(bec_cafe, cnet_cafe, cnet_cafe_sp) %>%
  map(.f = ~ .x %>%
        select(id_item, abertura_lances, win_bid_kg,
               unidade_compradora, comprasnet) %>%
        trim_df('win_bid_kg', perc = perc_outliers) %>%  # <<<<
        group_by(unidade_compradora) %>%
        mutate(n = n()) %>%
        arrange(n) %>%
        ungroup()
  ) %>%
  set_names(c('bec_cafe_trimmed', 'cnet_cafe_trimmed', 'sp_cnet_cafe_trimmed'))

attach(data_list)

# FWD E BWD STEPWISE SELECTION COMPRASNET SP ----------------------------------
# Criando DF com dummies
cnet_sp_dummies <- cbind(win_bid_kg = sp_cnet_cafe_trimmed$win_bid_kg,
                         dummify(sp_cnet_cafe_trimmed$unidade_compradora))

# Forward and backward stepwise regressions
sp_cnet_fwd <- regsubsets(win_bid_kg ~ ., data = cnet_sp_dummies,
                          method = 'forward',
                          nvmax = length(names(cnet_sp_dummies)) - 1)

sp_cnet_bwd <- regsubsets(win_bid_kg ~ ., data = cnet_sp_dummies,
                          method = 'backward',
                          nvmax = length(names(cnet_sp_dummies)) - 1)

# Salvando sumarios
sp_cnet_summary_fwd <- summary(sp_cnet_fwd)
sp_cnet_summary_bwd <- summary(sp_cnet_bwd)

# Graficos de avaliacao
model_assessment_plots(sp_cnet_summary_fwd) # Forward
model_assessment_plots(sp_cnet_summary_bwd) # Backward

# DF com resultados da selecao (adjR2, BIC, Cp)
sp_cnet_results  <- get_selection_results(step_model_forward = sp_cnet_fwd,
                                          step_model_backward = sp_cnet_bwd)

# Criando novas variaveis que identificam apenas UASGS selecionadas
sp_cnet_cafe_trimmed <- create_new_uasg_variables(sp_cnet_cafe_trimmed,
                                                  sp_cnet_results)

# FWD E BWD STEPWISE SELECTION COMPRASNET COMPLETA ----------------------------
# Criando DF com dummies
cnet_dummies <- cbind(win_bid_kg = cnet_cafe_trimmed$win_bid_kg,
                      dummify(cnet_cafe_trimmed$unidade_compradora))

# Forward and backward stepwise regressions
cnet_fwd <- regsubsets(win_bid_kg ~ .,
                       data = cnet_dummies,
                       method = 'forward',
                       nvmax = length(names(cnet_dummies)) - 1)

cnet_bwd <- regsubsets(win_bid_kg ~ .,
                       data = cnet_dummies,
                       method = 'backward',
                       nvmax = length(names(cnet_dummies)) - 1)

# Salvando sumarios
cnet_summary_fwd <- summary(cnet_fwd)
cnet_summary_bwd <- summary(cnet_bwd)

# Graficos de avaliacao
model_assessment_plots(cnet_summary_fwd) # Forward
model_assessment_plots(cnet_summary_bwd) # Backward

# DF com resultados da selecao (adjR2, BIC, Cp)
cnet_results <- get_selection_results(step_model_forward = cnet_fwd,
                                      step_model_backward = cnet_bwd,
                                      fwd_summary = cnet_summary_fwd,
                                      bwd_summary = cnet_summary_bwd)

#  Criando novas variaveis que identificam apenas UASGS selecionadas
cnet_cafe_trimmed <- create_new_uasg_variables(cnet_cafe_trimmed, cnet_results)

# FWD E BWD STEPWISE SELECTION BEC --------------------------------------------
# Criando DF com dummies
bec_dummies <- cbind(win_bid_kg = bec_cafe_trimmed$win_bid_kg,
                     dummify(bec_cafe_trimmed$unidade_compradora))

# Forward and backward stepwise regressions
bec_fwd <- regsubsets(win_bid_kg ~ .,
                      data = bec_dummies,
                      method = 'forward',
                      nvmax = length(names(bec_dummies)) - 1)

bec_bwd <- regsubsets(win_bid_kg ~ .,
                      data = bec_dummies,
                      method = 'backward',
                      nvmax = length(names(bec_dummies)) - 1)

# Salvando sumarios
bec_summary_fwd <- summary(bec_fwd)
bec_summary_bwd <- summary(bec_bwd)

# Graficos de avaliacao
model_assessment_plots(bec_summary_fwd)
model_assessment_plots(bec_summary_bwd)


# DF com resultados da selecao (adjR2, BIC, Cp)
bec_results <- get_selection_results(step_model_forward = bec_fwd,
                                     step_model_backward = bec_bwd)

# Nova coluna que identifica apenas UASGs selecionadas
bec_cafe_trimmed <- create_new_uasg_variables(bec_cafe_trimmed, bec_results)

# LASSO COMPRASNET SP ---------------------------------------------------------
# Variavel para gridsearch do lambda
grid <- 10 ^ seq(10, -2, length = 100)

# Voltando configuracoes de exibicao de graficos para o padrao
par(mfrow = c(1, 1))

# Fitting
sp_cnet_lasso <- glmnet(y = cnet_sp_dummies$win_bid_kg,
                        x = model.matrix(win_bid_kg ~ .,
                                         cnet_sp_dummies)[, -1],
                        alpha = 1,
                        lambda = grid)


# Grafico mostrando variaveis "zeradas" em funcao de lambda
plot(sp_cnet_lasso)

# Identificando melhor modelo por CV
cv_sp_cnet_lasso <- cv.glmnet(y = cnet_sp_dummies$win_bid_kg,
                              x = model.matrix(win_bid_kg ~ .,
                                               cnet_sp_dummies)[, -1],
                              alpha = 1,
                              lambda = grid)

# MSE vs log(lambda)
plot(cv_sp_cnet_lasso)

# Salvando lambda do melhor modelo
sp_cnet_bestlam = cv_sp_cnet_lasso$lambda.min

# Coeficientes do melhor modelo
sp_cnet_lasso_coef = predict(cv_sp_cnet_lasso,
                             type = "coefficients",
                             s = sp_cnet_bestlam)

# Variaveis com coeficiente nao-nulo (selecionadas)
sp_cnet_non_zero <-
  sp_cnet_lasso_coef[,1][sp_cnet_lasso_coef[,1] != 0] %>%
  names() %>% str_remove('\\.data_')

# Criando coluna que identifica apenas UASGS selecionadas
sp_cnet_cafe_trimmed <- sp_cnet_cafe_trimmed %>%
  mutate(lasso = ifelse(unidade_compradora %in% sp_cnet_non_zero,
                        unidade_compradora,
                        'Outra') %>% factor())

# LASSO COMPRASNET COMPLETA ---------------------------------------------------
# Fitting
cnet_lasso <- glmnet(y = cnet_dummies$win_bid_kg,
                     x = model.matrix(win_bid_kg ~ ., cnet_dummies)[, -1],
                     alpha = 1,
                     lambda = grid)

# Grafico mostrando variaveis "zeradas" em funcao de lambda
plot(cnet_lasso)

# Identificando melhor modelo por CV
cv_cnet_lasso <- cv.glmnet(y = cnet_dummies$win_bid_kg,
                           x = model.matrix(win_bid_kg ~ .,
                                            cnet_dummies)[, -1],
                           alpha = 1,
                           lambda = grid)

# MSE vs log(lambda)
plot(cv_cnet_lasso)

# Salvando lambda do melhor modelo
cnet_bestlam = cv_cnet_lasso$lambda.min

# Coeficientes do melhor modelo
cnet_lasso_coef = predict(cv_cnet_lasso,
                          type = "coefficients",
                          s = cnet_bestlam)

# Codigos das variaveis com coeficiente nao-nulo (selecionadas)
cnet_non_zero <- cnet_lasso_coef[,1][cnet_lasso_coef[,1] != 0] %>%
  names() %>% str_remove('\\.data_')

# Nova coluna que identifica apenas UASGs selecionadas
cnet_cafe_trimmed <- cnet_cafe_trimmed %>%
  mutate(lasso = ifelse(unidade_compradora %in% cnet_non_zero,
                        unidade_compradora,
                        'Outra') %>% factor())


# LASSO BEC -------------------------------------------------------------------
# Fitting
bec_lasso <- glmnet(y = bec_dummies$win_bid_kg,
                    x = model.matrix(win_bid_kg ~ ., bec_dummies)[, -1],
                    alpha = 1,
                    lambda = grid)


# Grafico mostrando variaveis "zeradas" em funcao de lambda
plot(bec_lasso)

# Identificando melhor modelo por CV
cv_bec_lasso <- cv.glmnet(y = bec_dummies$win_bid_kg,
                          x = model.matrix(win_bid_kg ~ .,
                                           bec_dummies)[, -1],
                          alpha = 1,
                          lambda = grid)

# MSE vs log(lambda)
plot(cv_bec_lasso)

# Salvando lambda do melhor modelo
bec_bestlam = cv_bec_lasso$lambda.min

# Coeficientes do melhor modelo
bec_lasso_coef = predict(bec_lasso, type = "coefficients", s = bec_bestlam)

# Codigos das variaveis com coeficiente nao-nulo (selecionadas)
bec_non_zero <- bec_lasso_coef[,1][bec_lasso_coef[,1] != 0] %>%
  names() %>% str_remove('\\.data_')

# Criando coluna que identifica apenas UASGS selecionadas
bec_cafe_trimmed <- bec_cafe_trimmed %>%
  mutate(lasso = ifelse(unidade_compradora %in% bec_non_zero,
                        unidade_compradora,
                        'Outra') %>% factor())

# Checando selecao ------------------------------------------------------------
map(.x = list(bec_cafe_trimmed, cnet_cafe_trimmed, sp_cnet_cafe_trimmed),
    .f = ~ select(.x, id_item, fwd_adjr2:lasso,
                  num_unidade_compradora = unidade_compradora) %>%
      checar_selecao()
    ) %>% set_names('bec', 'cnet_full', 'cnet_sp')

# Salvando resultados ---------------------------------------------------------
walk2(
  .x = list(bec_cafe_trimmed, cnet_cafe_trimmed, sp_cnet_cafe_trimmed),
  .y = str_c(c('bec', 'cnet', 'cnet_sp'), '_selected_uasgs_',
             filename_end, '.rds'),
  .f = ~ select(.x, id_item, num_unidade_compradora = unidade_compradora,
                fwd_adjr2:lasso) %>% saveRDS(.y)
)

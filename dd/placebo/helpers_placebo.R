# Datas relevantes
data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

# Funcoes para simplificar a extracao de coeficientes e erros padrao dos DFs de resultados
select_estimate <- function(df, treat_var) filter(df, term == treat_var) %>% select(estimate) %>% unlist()
select_se <- function(df, treat_var) filter(df, term == treat_var) %>% select(std.error) %>% unlist()

# get_tidy_estimates <- function(df) {
#   # Extraindo coeficientes e erros padrao de 'treat1' e 'treat2'
#   df %>%
#     mutate(
#       treat1_est = map_dbl(
#         .x = model_summary,
#         .f = ~ select_estimate(.x, 'treat1')
#       ),
#       treat2_est =  map_dbl(
#         .x = model_summary,
#         .f = ~ select_estimate(.x, 'treat2')
#       ),
#       treat1_std = map_dbl(
#         .x = model_summary,
#         .f = ~ select_se(.x, 'treat1')
#       ),
#       treat2_std = map_dbl(
#         .x = model_summary,
#         .f = ~ select_se(.x, 'treat2')
#       )
#     ) %>%
#     # Calculando limites inferior e superior dos intervalos de confianca
#     ## Nota: erros padrao convencionais
#     mutate(treat1_upper = treat1_est + 2*treat1_std,
#            treat1_lower = treat1_est - 2*treat1_std,
#            treat2_upper = treat2_est + 2*treat2_std,
#            treat2_lower = treat2_est - 2*treat2_std)
# }

get_tidy_estimates <- function(df) {
  # Extraindo coeficientes e erros padrao de 'treat1' e 'treat2'
  df %>%
    mutate(
      treat1_est = map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat1')
      ),
      treat2_est =  map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat2')
      ),
      treat_placebo_est = map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat_placebo')
      ),
      treat1_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat1')
      ),
      treat2_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat2')
      ),
      treat_placebo_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat_placebo')
      )
    ) %>%
    # Calculando limites inferior e superior dos intervalos de confianca
    ## Nota: erros padrao convencionais
    mutate(treat1_upper = treat1_est + 2*treat1_std,
           treat1_lower = treat1_est - 2*treat1_std,
           treat2_upper = treat2_est + 2*treat2_std,
           treat2_lower = treat2_est - 2*treat2_std,
           treat_placebo_upper = treat_placebo_est + 2*treat_placebo_std,
           treat_placebo_lower = treat_placebo_est - 2*treat_placebo_std)
}


# Tema para graficos no ggplot2
my_theme <- function() {
  theme_bw() +
    theme(text = element_text(family = 'serif'),
          strip.text = element_text(face = 'bold', size = 14),
          strip.background = element_rect(size = 1.2),
          panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          plot.title = element_text(face = 'bold', size = 16),
          plot.subtitle = element_text(size = 13),
          plot.caption = element_text(hjust = 0, size = 10))
}



clean_model_summary <- function(df) {

  df %>%
    broom::tidy() %>%
    filter(term %in% c('..1', '..2', '..3')) %>%
    mutate(
      term = case_when(
        term == '..1' ~ 'treat1',
        term == '..2' ~ 'treat2',
        term == '..3' ~ 'treat_placebo',
        !term %in% c('..1', '..2', '..3') ~ term
      )
    )

}

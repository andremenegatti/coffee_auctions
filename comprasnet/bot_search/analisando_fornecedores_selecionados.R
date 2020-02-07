library(tidyverse)

df_lances <- readRDS('data/cnet_lances.rds') %>% 
  filter(data_hora < '2016-01-01')

# "09.234.429/0001-18" --------------------------------------------------------
# Grande aumento no incremento após medidas restritivas
df_lances %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  filter(desconto_bruto < 0) %>%  # Apenas menor valor
  group_by(regime_juridico) %>%
  PregoesBR::get_summary_stats_by_period(var = norm_inc_first * 100,
                                         time_var = regime_juridico)

# O mesmo ocorre com incrementos entre lances proprios
df_lances %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(regime_juridico) %>%
  PregoesBR::get_summary_stats_by_period(var = incremento_lance_proprio,
                                         time_var = regime_juridico)

# Mas o efeito sobre intervalos é ambíguo
df_lances %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  filter(desconto_bruto < 0 ) %>% # Apenas menor valor
  group_by(regime_juridico) %>%
  PregoesBR::get_summary_stats_by_period(var = intervalo_menor_lance, # <<<
                                         time_var = regime_juridico)

df_lances %>%
  filter(CNPJ_CPF == "09.234.429/0001-18") %>%
  group_by(regime_juridico) %>%
  PregoesBR::get_summary_stats_by_period(var = intervalo_lance_anterior, # <<<<
                                         time_var = regime_juridico)
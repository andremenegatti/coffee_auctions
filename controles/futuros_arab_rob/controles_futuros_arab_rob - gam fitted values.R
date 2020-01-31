library(mgcv)
library(tidyverse)

# Futuros Bloomberg, precos cafe ESALQ
controles <- readRDS('controles_futuros_arab_rob.rds') %>%
  select(abertura_lances, arab, arab_defl, rob, rob_defl, arab_rob, arab_rob_defl, futuro, futuro_defl) %>%
  mutate(data_numero = as.numeric(abertura_lances))

gam_arab <- mgcv::gam(arab_defl ~ s(data_numero), data = controles, method = 'REML')
gam_rob <- mgcv::gam(rob_defl ~ s(data_numero), data = controles, method = 'REML')
gam_arab_rob <- mgcv::gam(arab_rob_defl ~ s(data_numero), data = controles, method = 'REML')
gam_futuros <- mgcv::gam(futuro_defl ~ s(data_numero), data = controles, method = 'REML')

controles$arab_fitted <- gam_arab$fitted.values
controles$rob_fitted <- gam_rob$fitted.values
controles$arab_rob_fitted <- gam_arab_rob$fitted.values
controles$futuro_fitted <- gam_futuros$fitted.values

controles$data_numero <- NULL

saveRDS(controles, 'controles_futuros_arab_rob.rds')

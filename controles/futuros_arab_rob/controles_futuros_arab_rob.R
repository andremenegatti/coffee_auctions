library(xts)
library(PregoesBR)

# Importando dados ------------------------------------------------------------
futuros <- readRDS('data/futuros.rds') %>%
  select(inicio_mes, futuro_media_mensal, data, futuro = valor)

precos_cafe_cepea <- readRDS('data/precos_cafe_cepea.rds') %>%
  select(inicio_mes, pr_mensal_arab, pr_mensal_rob,
         pr_arab_reais, pr_rob_reais, data)

bec_cafe <- readRDS('data/bec_cafe.rds') %>%
  mutate(abertura_lances = as.Date(dt_inicio),
         comprasnet = 0) %>%
  filter(abertura_lances >= '2011-03-01')

cnet_cafe <- readRDS('data/cnet_cafe.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))

# Interpolacao ----------------------------------------------------------------
# Criando DF com as datas de todos os pregoes, ordenadas
datas_pregoes <- bec_cafe %>%
  select(abertura_lances) %>%
  bind_rows(cnet_cafe %>% select(abertura_lances)) %>%
  distinct() %>%
  arrange(abertura_lances)

# Complementando DF criado com os precos disponiveis em cada data
datas_precos <- datas_pregoes %>%
  left_join(futuros %>%
              select(futuro, data),
            by = c('abertura_lances' = 'data')) %>%
  left_join(precos_cafe_cepea %>%
              select(pr_arab_reais, pr_rob_reais, data),
            by = c('abertura_lances' = 'data'))

# Para aproveitar os dados disponiveis ao maximo,
# vamos interpolar com base em todos os dados de precos
# para depois incluir apenas os dados faltantes nas datas dos pregoes
datas_precos_full <- datas_pregoes %>%
  full_join(futuros %>%
              select(futuro, data),
            by = c('abertura_lances' = 'data')) %>%
  full_join(precos_cafe_cepea %>%
              select(pr_arab_reais, pr_rob_reais, data),
            by = c('abertura_lances' = 'data')) %>%
  arrange(abertura_lances)

# Construindo objetos de series de tempo para facilitar interpolacao
futuros_xts <- as.xts(as.data.frame(datas_precos_full$futuro),
                      order.by = datas_precos_full$abertura_lances)

robusta_xts <- as.xts(as.data.frame(datas_precos_full$pr_rob_reais),
                      order.by = datas_precos_full$abertura_lances)

arabica_xts <- as.xts(as.data.frame(datas_precos_full$pr_arab_reais),
                      order.by = datas_precos_full$abertura_lances)

# Construindo DF com interpolacoes
df_interpol <- 
  tibble(
    abertura_lances = datas_precos_full$abertura_lances,
    # Incluindo NA na primeira linha para as colunas ficarem do mesmo tamanho
    # Primeira obs é NA: nao foi possivel interpolar futuros
    futuro_interp = c(NA, na.approx(futuros_xts) %>% as.numeric()),
    # Incluindo NA na ultima linha para as colunas ficarem do mesmo tamanho
    # Ultima obs é NA: nao foi possivel interpolar robusta e arabica
    rob_interp = c(na.approx(robusta_xts) %>% as.numeric(), NA),
    arab_interp = c(na.approx(arabica_xts) %>% as.numeric(), NA)
    )

# Joining
datas_precos2 <- datas_precos %>%
  select(abertura_lances, futuro, pr_arab_reais, pr_rob_reais) %>%
  left_join(df_interpol, by = 'abertura_lances')

# Para arab_interp e rob_interb, replicamos o valor da penultima
# obs na ultima observacao
datas_precos2$arab_interp[nrow(datas_precos2)] <- 
  datas_precos2$arab_interp[nrow(datas_precos2) - 1]

datas_precos2$rob_interp[nrow(datas_precos2)] <- 
  datas_precos2$rob_interp[nrow(datas_precos2) - 1]

# DF com indices do IPCA para deflacionar
df_ipca <- read_csv('data/ipca.csv') %>%
  mutate(inicio_mes = lubridate::ymd(inicio_mes))

# Definindo índice de referência para deflacionar valores
ipca_periodo_referencia <- 4493.17 # IPCA dez/2015

# Deflacionando e organizando DF
controles <- datas_precos2 %>%
  # Selecionando e renomeando variaveis
  select(abertura_lances, futuro = futuro_interp,
         rob = rob_interp, arab = arab_interp) %>%
  # Criando media arabica-robusta
  mutate(arab_rob = (arab + rob)/2) %>%
  # Criando variaveis de tempo
  # (apenas a de mes sera util, depois excluimos as demais)
  PregoesBR::create_time_variables() %>%
  # Joining indice IPCA
  left_join(df_ipca, by = 'inicio_mes') %>%
  # Deflacionando
  mutate(rob_defl = 
           PregoesBR::deflacionar(rob,
                                  indice_referencia = ipca_periodo_referencia,
                                  indice_no_periodo = ipca),
         arab_defl = 
           PregoesBR::deflacionar(arab,
                                  indice_referencia = ipca_periodo_referencia,
                                  indice_no_periodo = ipca),
         arab_rob_defl = 
           PregoesBR::deflacionar(arab_rob,
                                  indice_referencia = ipca_periodo_referencia,
                                  indice_no_periodo = ipca),
         futuro_defl = 
           PregoesBR::deflacionar(futuro,
                                  indice_referencia = ipca_periodo_referencia,
                                  indice_no_periodo = ipca)) %>%
  # Selecionando/reordenando colunas
  select(abertura_lances, inicio_mes, arab, arab_defl, rob, rob_defl,
         arab_rob, arab_rob_defl, futuro, futuro_defl)

# Lineplots para checar dados interpolados ------------------------------------
# Valores sem deflacionar
controles %>%
  select(arab, rob, arab_rob, futuro, abertura_lances) %>%
  gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
  ggplot() +
  geom_smooth(aes(x = abertura_lances, y = preco,
                  group = variavel, color = variavel)) +
  labs(x = 'Data', y = 'Preço',
       title = 'Variáveis de controle') +
  scale_color_discrete(
    name = 'Variável',
    labels = c('Arábica', 'Média arab-rob', 'Futuro', 'Robusta')
    )

# Valores deflacionados
controles %>%
  select(arab_defl, rob_defl, arab_rob_defl,
         futuro_defl, abertura_lances) %>%
  gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
  ggplot() +
  geom_smooth(aes(x = abertura_lances, y = preco,
                  group = variavel, color = variavel)) +
  labs(x = 'Data', y = 'Preço',
       title = 'Variáveis de controle deflacionadas') +
  scale_color_discrete(
    name = 'Variável',
    labels = c('Arábica', 'Média arab-rob', 'Futuro', 'Robusta')
    )

# GAM fitted values -----------------------------------------------------------
controles <- controles %>% 
  select(-inicio_mes) %>%
  # mgcg::gam nao lida automaticamente com datas
  mutate(data_numero = as.numeric(abertura_lances))

gam_arab <- mgcv::gam(arab_defl ~ s(data_numero),
                      data = controles, method = 'REML')

gam_rob <- mgcv::gam(rob_defl ~ s(data_numero),
                     data = controles, method = 'REML')

gam_arab_rob <- mgcv::gam(arab_rob_defl ~ s(data_numero),
                          data = controles, method = 'REML')

gam_futuros <- mgcv::gam(futuro_defl ~ s(data_numero),
                         data = controles, method = 'REML')

controles$arab_fitted <- gam_arab$fitted.values
controles$rob_fitted <- gam_rob$fitted.values
controles$arab_rob_fitted <- gam_arab_rob$fitted.values
controles$futuro_fitted <- gam_futuros$fitted.values

# Nao precisamos mais dessa coluna
controles$data_numero <- NULL

# Lineplots para checar GAM fitted values -------------------------------------
controles %>%
  select(arab_fitted, rob_fitted, arab_rob_fitted,
         futuro_fitted, abertura_lances) %>%
  gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
  ggplot() +
  geom_point(aes(x = abertura_lances, y = preco,
                  group = variavel, color = variavel),
             size = 0.5, alpha = 0.2, shape = 1) +
  labs(x = 'Data', y = 'Preço',
       title = 'Variáveis de controle deflacionadas') +
  scale_color_discrete(name = 'Variável',
                       labels = c('Arábica', 'Média arab-rob',
                                  'Futuro', 'Robusta'))
ggsave('smooth_controles_deflacionados.png')

# Salvando --------------------------------------------------------------------
saveRDS(controles, 'data/controles_futuros_arab_rob.rds')

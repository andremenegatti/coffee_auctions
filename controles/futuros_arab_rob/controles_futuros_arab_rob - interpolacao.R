library(xts)
library(PregoesBR)

#### ABRINDO BASES ####
futuros <- readRDS('Dados/futuros.rds') %>%
  select(inicio_mes, futuro_media_mensal, data, futuro = valor)

precos_cafe_cepea <- readRDS('Dados/precos_cafe_cepea.rds') %>%
  select(inicio_mes, pr_mensal_arab, pr_mensal_rob, pr_arab_reais, pr_rob_reais, data)

BEC_cafe <- readRDS('BEC/BEC_cafe_etapa4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 0) %>%
  rename(unidade_compradora = UNIDADE_COMPRADORA)

cnet_cafe <- readRDS('Comprasnet/cnet_cafe_01_v4.rds') %>%
  filter(abertura_lances >= '2011-03-01') %>%
  mutate(abertura_lances = as.Date(abertura_lances),
         comprasnet = 1, unidade_compradora = str_extract(id_item, '\\d{6}'))

# Criando DF com as datas de todos os pregoes, ordenadas
datas_pregoes <- BEC_cafe %>%
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
futuros_xts <- as.xts(as.data.frame(datas_precos_full$futuro), order.by = datas_precos_full$abertura_lances)
robusta_xts <- as.xts(as.data.frame(datas_precos_full$pr_rob_reais), order.by = datas_precos_full$abertura_lances)
arabica_xts <- as.xts(as.data.frame(datas_precos_full$pr_arab_reais), order.by = datas_precos_full$abertura_lances)

# Construindo DF com interpolacoes
df_interpol <- tibble(abertura_lances = datas_precos_full$abertura_lances,
                      # Incluindo NA na primeira linha para as colunas ficarem do mesmo tamanho
                      # (Como a primeira observacao era NA, nao foi possivel interpolar futuros)
                      futuro_interp = c(NA, na.approx(futuros_xts) %>% as.numeric()),
                      # Incluindo NA na ultima linha para as colunas ficarem do mesmo tamanho
                      # (Como a ultima observacao era NA, nao foi possivel interpolar robusta e arabica)
                      rob_interp = c(na.approx(robusta_xts) %>% as.numeric(), NA),
                      arab_interp = c(na.approx(arabica_xts) %>% as.numeric(), NA))

# Joining
datas_precos2 <- datas_precos %>%
  select(abertura_lances, futuro, pr_arab_reais, pr_rob_reais) %>%
  left_join(df_interpol, by = 'abertura_lances')

datas_precos2$arab_interp[nrow(datas_precos2)] <- datas_precos2$arab_interp[nrow(datas_precos2) - 1]
datas_precos2$rob_interp[nrow(datas_precos2)] <- datas_precos2$rob_interp[nrow(datas_precos2) - 1]

# DF com indices do IPCA para deflacionar
df_ipca <- read_csv('Dados/ipca.csv') %>%
  mutate(inicio_mes = lubridate::ymd(inicio_mes))

# Deflacionando e organizando DF
controles <- datas_precos2 %>%
  # Selecionando e renomeando variaveis
  select(abertura_lances, futuro = futuro_interp, rob = rob_interp, arab = arab_interp) %>%
  # Criando media arabica-robusta
  mutate(arab_rob = (arab + rob)/2) %>%
  # Criando variaveis de tempo (apenas a de mes sera util, depois excluimos as demais)
  create_time_variables() %>%
  # Joining indice IPCA
  left_join(df_ipca, by = 'inicio_mes') %>%
  # Deflacionando
  mutate(rob_defl = deflacionar(rob, indice_no_periodo = ipca),
         arab_defl = deflacionar(arab, indice_no_periodo = ipca),
         arab_rob_defl = deflacionar(arab_rob, indice_no_periodo = ipca),
         futuro_defl = deflacionar(futuro, indice_no_periodo = ipca)) %>%
  # Selecionando/reordenando colunas
  select(abertura_lances, inicio_mes, arab, arab_defl, rob, rob_defl, arab_rob, arab_rob_defl, futuro, futuro_defl)

# Visualizando evolucao precos cafe e futuros
controles %>%
  select(arab, rob, arab_rob, futuro, abertura_lances) %>%
  gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
  ggplot() +
  geom_smooth(aes(x = abertura_lances, y = preco, group = variavel, color = variavel)) +
  labs(x = 'Data', y = 'Preço', title = 'Variáveis de controle') +
  scale_color_discrete(name = 'Variável', labels = c('Arábica', 'Média arab-rob', 'Futuro', 'Robusta'))
ggsave('smooth_controles_nao_deflacionados.png')

# Visualizando evolucao precos cafe e futuros DEFLACIONADOS
controles %>%
  select(arab_defl, rob_defl, arab_rob_defl, futuro_defl, abertura_lances) %>%
  gather(key = 'variavel', value = 'preco', -abertura_lances) %>%
  ggplot() +
  geom_smooth(aes(x = abertura_lances, y = preco, group = variavel, color = variavel)) +
  labs(x = 'Data', y = 'Preço', title = 'Variáveis de controle deflacionadas') +
  scale_color_discrete(name = 'Variável', labels = c('Arábica', 'Média arab-rob', 'Futuro', 'Robusta'))
ggsave('smooth_controles_deflacionados.png')

saveRDS(controles, 'controles_futuros_arab_rob.rds')

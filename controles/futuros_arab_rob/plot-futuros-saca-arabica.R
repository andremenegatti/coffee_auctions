library(tidyverse)
library(plotly)

fut_cepea <- 
  readRDS("controles/futuros_arab_rob/controles_futuros_arab_rob.rds")

plot <- fut_cepea %>% 
  select(abertura_lances, futuro_defl, arab_defl) %>% 
  filter(abertura_lances >= '2011-03-01',
         abertura_lances < '2016-01-01') %>% 
  pivot_longer(cols = -abertura_lances,
               names_to = 'covariate', values_to = 'price') %>% 
  mutate(covariate = ifelse(covariate == 'arab_defl',
                            'Saca de café arábica', 'KC1:COM') %>% 
           fct_relevel('Saca de café arábica', 'KC1:COM')) %>% 
  ggplot() +
  geom_point(aes(x = abertura_lances, y = price, shape = covariate),
             alpha = .08) +
  geom_smooth(aes(x = abertura_lances, y = price, linetype = covariate),
              se = FALSE, col = 'black') +
  labs(
    x = 'Data',
    y = 'Preço em reais'
  ) +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .875)) ; plot

ggsave(plot = plot, width = 5.5, height = 4.5,
       filename = 'plots/futuros-e-saca-arabica.png')

ggplotly(plot)

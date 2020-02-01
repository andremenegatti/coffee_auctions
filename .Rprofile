data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

require(ggplot2)

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

theme_set(my_theme())

# ggplot2::theme_set(ggplot2::theme_bw())

# Opcao importante para lidar com milesimos de segundos
options(digits.secs = 3)

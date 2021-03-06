library(tidyverse)
library(plotly)

bid_plot <- function(data) {
  
  formatar_numero <- partial(formatC, decimal.mark = ',', big.mark = '.')
  
  my_plot <- data %>%
    plot_ly(x = ~ data_hora, y = ~ valor_lance, hoverinfo = "text",
            text = ~ paste(
              "<b>CNPJ:</b> ", CNPJ_CPF,
              "<br><b>Data/Hora:</b> ", data_hora,
              "<br><b>Lance:</b> R$", formatar_numero(x = valor_lance, digits = 4),
              "<br><b>Lance/kg:</b> R$",
              formatar_numero(x = valor_lance_corr_defl, digits = 8),
              "<br><b>Incremento total:</b>: R$",
              formatar_numero(x = -1*desconto_bruto, digits = 6),
              "<br><b>Incremento/kg:</b> R$",
              formatar_numero(x = -1 * increment, digits = 8),
              "<br><b>Intervalo lance anterior:</b> ",
              formatar_numero(x = intervalo_lance_anterior, digits = 4), "s",
              "<br><b>Intervalo menor lance:</b> ",
              formatar_numero(x = intervalo_menor_lance, digits = 4), "s",
              "<br><b>Intervalo lance proprio:</b> ",
              formatar_numero(x = intervalo_lance_proprio, digits = 4), "s"
            ))  %>%
    group_by(Fornecedor) %>% 
    add_markers(color = ~ Fornecedor, y = ~ valor_lance_corr_defl,
                opacity = 0, showlegend = FALSE,
                yaxis = "y2") %>% 
    add_markers(color = ~ Fornecedor,
                opacity = 0.8, showlegend = FALSE) %>%
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Valor total do lance (reais)'),
           yaxis2 = list(
             overlaying = "y",
             side = "right",
             title = "Valor/kg deflacionado (reais)",
             showgrid = FALSE),
           margin = list(
             l = 50,
             r = 60,
             b = 50,
             t = 50,
             pad = 4
             )) %>% 
    highlight(on = 'plotly_click', off = 'plotly_doubleclick')
  
  my_plot
  
}

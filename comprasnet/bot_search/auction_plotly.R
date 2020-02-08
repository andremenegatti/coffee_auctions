auction_plotly <- function(df) {
  
  formatar_numero <- partial(formatC, decimal.mark = ',', big.mark = '.')
  
  shared_bids <- SharedData$new(df, key = ~ Fornecedor)
  
  p1 <- shared_bids %>% 
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
    add_markers(color = ~ Fornecedor, opacity = 0.6,
                legendgroup = ~ Fornecedor, showlegend = FALSE) %>%
    add_markers(y = ~ valor_lance_corr_defl, yaxis = 'y2',
                color = ~ Fornecedor, opacity = 0,
                legendgroup = ~ Fornecedor, showlegend = FALSE) %>%
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Valor total do lance (reais)'),
           yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
                         title = "Valor/kg deflacionado (reais)")) 
  
  
  p2 <- shared_bids %>% 
    plot_ly(x = ~ data_hora, y = ~ desconto_bruto, hoverinfo = "text",
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
    add_markers(color = ~ Fornecedor, opacity = 0.6,
                legendgroup = ~ Fornecedor, showlegend = TRUE) %>%
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Diferença de valor em relação ao menor lance (reais)')) 
  

  subplot(p1, p2, nrows = 2,
          shareX = FALSE, titleX = FALSE,
          shareY = FALSE, titleY = TRUE) %>% 
    highlight(on = 'plotly_click', off = 'plotly_doubleclick') %>% 
    layout(legend = list(x = 100, y = 0.15))
  
}
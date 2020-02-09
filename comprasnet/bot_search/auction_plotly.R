auction_plotly <- function(df) {
  
  formatar_numero <- partial(formatC, decimal.mark = ',', big.mark = '.')
  
  shared_bids <- SharedData$new(df, key = ~ Fornecedor)
  
  p1 <- shared_bids %>% 
    plot_ly(x = ~ data_hora, y = ~ valor_lance_kg_defl, hoverinfo = "text",
            text = ~ paste(
              "<b>CNPJ:</b> ", CNPJ_CPF,
              "<br><b>Data/Hora:</b> ", data_hora,
              "<br><b>Lance:</b> R$", formatar_numero(x = valor_lance, digits = 8),
              "<br><b>Lance/kg:</b> R$",
              formatar_numero(x = round(valor_lance_kg_defl, digits = 8), digits = 8),
              "<br><b>Incremento (menor):</b> ",
              formatar_numero(x = round(incremento_menor * 100, digits = 8), digits = 5), "cent/kg",
              "<br><b>Incremento (anterior):</b> ",
              formatar_numero(x = round(incremento_anterior * 100, digits = 8), digits = 5), "cent/kg",
              "<br><b>Incremento (próprio):</b> ",
              formatar_numero(x = round(incremento_proprio * 100, digits = 5), digits = 5), "cent/kg",
              "<br><b>Intervalo menor lance:</b>  ",
              formatar_numero(x = intervalo_menor, digits = 4), "s",
              "<br><b>Intervalo lance anterior:</b> ",
              formatar_numero(x = intervalo_anterior, digits = 4), "s",
              "<br><b>Intervalo lance proprio:</b> ",
              formatar_numero(x = intervalo_proprio, digits = 4), "s"
            ))  %>%
    group_by(Fornecedor) %>% 
    add_markers(color = ~ Fornecedor, opacity = 0.6,
                legendgroup = ~ Fornecedor, showlegend = FALSE) %>%
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Valor do lance em R$/kg'))
  
  
  p2 <- shared_bids %>% 
    plot_ly(x = ~ data_hora, y = ~ incremento_menor * 100, hoverinfo = "text",
            text = ~ paste(
              "<b>CNPJ:</b> ", CNPJ_CPF,
              "<br><b>Data/Hora:</b> ", data_hora,
              "<br><b>Lance:</b> R$", formatar_numero(x = valor_lance, digits = 8),
              "<br><b>Lance/kg:</b> R$",
              formatar_numero(x = round(valor_lance_kg_defl, digits = 8), digits = 8),
              "<br><b>Incremento (menor):</b> ",
              formatar_numero(x = round(incremento_menor * 100, digits = 8), digits = 5), "cent/kg",
              "<br><b>Incremento (anterior):</b> ",
              formatar_numero(x = round(incremento_anterior * 100, digits = 8), digits = 5), "cent/kg",
              "<br><b>Incremento (próprio):</b> ",
              formatar_numero(x = round(incremento_proprio * 100, digits = 5), digits = 5), "cent/kg",
              "<br><b>Intervalo menor lance:</b>  ",
              formatar_numero(x = intervalo_menor, digits = 4), "s",
              "<br><b>Intervalo lance anterior:</b> ",
              formatar_numero(x = intervalo_anterior, digits = 4), "s",
              "<br><b>Intervalo lance proprio:</b> ",
              formatar_numero(x = intervalo_proprio, digits = 4), "s"
            ))  %>%
    group_by(Fornecedor) %>% 
    add_markers(color = ~ Fornecedor, opacity = 0.6,
                legendgroup = ~ Fornecedor, showlegend = TRUE) %>%
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Diferença menor lance (centavos/kg)')) 
  

  subplot(p1, p2, nrows = 2,
          shareX = FALSE, titleX = FALSE,
          shareY = FALSE, titleY = TRUE) %>% 
    highlight(on = 'plotly_click', off = 'plotly_doubleclick') %>% 
    layout(legend = list(x = 100, y = 0.15))
  
}

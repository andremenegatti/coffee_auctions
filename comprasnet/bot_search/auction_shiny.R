auction_plot <- function(df) {
  
  shared_bids <- SharedData$new(df, key = ~ Fornecedor)
  
  p1 <- bid_plot(shared_bids)
  p2 <- inc_plot(shared_bids)
  
  bscols(widths = c(9, 3),
         list(p1, p2),
         list(
           filter_checkbox(id = "Fornecedor",
                           label = "Participantes",
                           sharedData = shared_bids,
                           group = ~ Fornecedor)
         ))
  
}
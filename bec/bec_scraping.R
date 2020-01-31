library(jsonlite)
library(tidyverse)

# Desativando notacao cientifica para evitar erros nos links
options(scipen = 999)

# Link base
base_url <-
  "https://www.bec.sp.gov.br/BEC_API/API/pregao_encerrado/OC_encerrada/12022008/01042019"

# DF com todos os pregoes que nao foram anulados ou desertos
url_json <- fromJSON(base_url) %>%
  filter(SITUACAO=="ENCERRADO COM VENCEDOR")

# Error-handling function
read_json_safely <- function(url) {
  out <- tryCatch(
    expr = {
      fromJSON(url, flatten = TRUE)
    },
    error = function(cond) {
      message('Unable to reach URL')
      return(NA)
    }
  )
  return(out)
}

# Inicializando vetores e DFs
urls_oc <- rep(NA_character_, nrow(url_json))
dados <- tibble()
i <- 1
error_log <- vector('numeric')

# Scraping
while (i < nrow(url_json)) {

  oc <- url_json$OC[i]

  message(paste0(i, ': OC n. ', oc))

  urls_oc[i] <- paste(base_url, oc, sep = '/')

  auction_data <- read_json_safely(urls_oc[i])

  if (identical(auction_data, NA)) {
    next
    error_log <- c(error_log, i)
    if (length(error_log) > 5) {
      if (last(error_log) == error_log[(length(error_log) - 3)]) {
        break
      }
    }
  } else {
    dados <- bind_rows(dados, auction_data)
    i = i + 1
  }

}

saveRDS(dados, file = "dados.rds")


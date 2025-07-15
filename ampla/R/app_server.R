#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Load required libraries
  library(plotly)
  library(dplyr)
  library(lubridate)
  library(fs) # Para manipulação de arquivos e diretórios
  library(here) # Para gerenciamento de caminhos relativos ao projeto

  # Load real data from RDS files (same pattern as amplaShiny)
  # Carrega todos os arquivos RDS do diretório de dados
  dados_l <- readRDS(
    dir_ls(here("inst", "dados"), type = "file")
  )

  # Initialize modules for all URLs (they'll only render when UI calls them)
  g_barras.empilhadas.mes_server(
    "despesas_chart",
    dados = dados_l$ik$desp,
    total = "total.pago",
    data = "data.doc.pagto",
    comeco.titulo = "Despesas por"
  )

  g_barras.empilhadas.mes_server(
    "receitas_chart",
    dados = dados_l$ik$rec,
    total = "total",
    data = "data.pagamento",
    comeco.titulo = "Receitas por"
  )
}

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

  # Create demo data
  dados_l <- list(
    ik = list(
      desp = data.frame(
        data.doc.pagto = seq(Sys.Date() - 365, Sys.Date(), by = "week"),
        total.pago = runif(53, 1000, 50000),
        centro.negocio = sample(c("Estacao", "Prudencia", "Vila Sonia"), 53, replace = TRUE),
        categoria = sample(c("Materiais", "Mao de Obra", "Equipamentos"), 53, replace = TRUE),
        empresa = sample(c("Ampla", "Construtora XYZ"), 53, replace = TRUE),
        stringsAsFactors = FALSE
      ),
      rec = data.frame(
        data.recebimento = seq(Sys.Date() - 365, Sys.Date(), by = "week"),
        total.recebido = runif(53, 5000, 100000),
        centro.negocio = sample(c("Estacao", "Prudencia", "Vila Sonia"), 53, replace = TRUE),
        categoria = sample(c("Vendas", "Aluguel", "Servicos"), 53, replace = TRUE),
        empresa = sample(c("Ampla", "Cliente A"), 53, replace = TRUE),
        stringsAsFactors = FALSE
      )
    )
  )

  # Initialize modules for all URLs (they'll only render when UI calls them)
  g_barras.empilhadas.mes_server(
    "despesas_chart",
    dados = dados_l$ik$desp,
    filtro_periodo = reactive("ultimos_12"),
    data_inicial = reactive(Sys.Date() - 365),
    data_final = reactive(Sys.Date()),
    total = "total.pago",
    data = "data.doc.pagto",
    comeco.titulo = "Despesas por"
  )

  g_barras.empilhadas.mes_server(
    "receitas_chart",
    dados = dados_l$ik$rec,
    filtro_periodo = reactive("ultimos_12"),
    data_inicial = reactive(Sys.Date() - 365),
    data_final = reactive(Sys.Date()),
    total = "total.recebido",
    data = "data.recebimento",
    comeco.titulo = "Receitas por"
  )
}

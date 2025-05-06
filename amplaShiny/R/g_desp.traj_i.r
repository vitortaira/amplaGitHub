# UI for Despesas trajectory chart
g_desp.traj_i <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Despesas"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Empresa"
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

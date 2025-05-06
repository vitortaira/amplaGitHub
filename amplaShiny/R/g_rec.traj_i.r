# UI for Receitas trajectory chart
g_rec.traj_i <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Receitas"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Empreendimento"
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

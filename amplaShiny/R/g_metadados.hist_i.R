g_metadados.hist_i <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Metadados"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Arquivo_tipo"
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

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
    radioButtons(
      inputId = ns("filtro_periodo"),
      label = "Período:",
      choices = c(
        "Ano corrente"       = "ano_corrente",
        "Últimos 12 meses"   = "ultimos_12",
        "Desde o início"     = "desde_inicio",
        "Selecionar período" = "personalizado"
      ),
      selected = "ano_corrente"
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'personalizado'", ns("filtro_periodo")),
      dateInput(ns("data_inicial"), "Data inicial:"),
      dateInput(ns("data_final"), "Data final:")
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

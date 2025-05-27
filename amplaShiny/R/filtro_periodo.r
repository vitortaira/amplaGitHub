filtro_periodo_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("filtro_periodo"),
      label = NULL,
      choices = c(
        "Ano corrente"     = "ano_corrente",
        "Últimos 12 meses" = "ultimos_12",
        "Desde o início"   = "desde_inicio",
        "Personalizado"    = "personalizado"
      ),
      selected = "ano_corrente",
      inline = TRUE
    ),
    conditionalPanel(
      condition = paste0("input['", ns("filtro_periodo"), "'] == 'personalizado'"),
      dateInput(ns("data_inicial"), "Data inicial:"),
      dateInput(ns("data_final"), "Data final:")
    )
  )
}

filtro_periodo_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return ALL filter values as reactive
    return(list(
      filtro_periodo = reactive({
        input$filtro_periodo
      }),
      data_inicial = reactive({
        input$data_inicial
      }),
      data_final = reactive({
        input$data_final
      })
    ))
  })
}

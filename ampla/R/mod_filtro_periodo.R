# =============================================================================
# MÓDULO: filtro_periodo
# Filtro de período para análise temporal de dados
# =============================================================================

#' @import shiny

#' @export
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
      selected = "ultimos_12",
      inline = TRUE
    ),
    conditionalPanel(
      condition = paste0("input['", ns("filtro_periodo"), "'] == 'personalizado'"),
      div(
        style = "margin-top: 10px;",
        dateInput(
          ns("data_inicial"),
          "Data inicial:",
          value = Sys.Date() - 365,
          format = "yyyy-mm-dd",
          language = "pt"
        ),
        dateInput(
          ns("data_final"),
          "Data final:",
          value = Sys.Date(),
          format = "yyyy-mm-dd",
          language = "pt"
        )
      )
    )
  )
}

#' @export
filtro_periodo_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return ALL filter values as reactive with validation
    return(list(
      filtro_periodo = reactive({
        input$filtro_periodo
      }),
      data_inicial = reactive({
        if (is.null(input$data_inicial)) {
          return(Sys.Date() - 365)
        }
        # Ensure it's a proper Date object
        if (inherits(input$data_inicial, "Date")) {
          input$data_inicial
        } else {
          as.Date(input$data_inicial)
        }
      }),
      data_final = reactive({
        if (is.null(input$data_final)) {
          return(Sys.Date())
        }
        # Ensure it's a proper Date object
        if (inherits(input$data_final, "Date")) {
          input$data_final
        } else {
          as.Date(input$data_final)
        }
      })
    ))
  })
}

# =============================================================================
# MÓDULO: filtroPeriodo
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
        fluidRow(
          column(
            6,
            dateInput(
              ns("data_inicial"),
              "Data inicial:",
              value = Sys.Date() - 365,
              format = "yyyy-mm-dd",
              language = "pt",
              weekstart = 1,
              daysofweekdisabled = NULL
            )
          ),
          column(
            6,
            dateInput(
              ns("data_final"),
              "Data final:",
              value = Sys.Date(),
              format = "yyyy-mm-dd",
              language = "pt",
              weekstart = 1,
              daysofweekdisabled = NULL
            )
          )
        )
      )
    ),
    # JavaScript personalizado para sobrescrever abreviações de dias da semana
    tags$script(HTML("
      $(document).ready(function() {
        // Sobrescrever locale português do Bootstrap datepicker com dias de 3 caracteres
        if ($.fn.datepicker && $.fn.datepicker.dates && $.fn.datepicker.dates.pt) {
          $.fn.datepicker.dates.pt.daysMin = ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sáb'];
        }
      });
    "))
  )
}

#' @export
filtro_periodo_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Retornar TODOS os valores de filtro como reativo com validação
    return(list(
      filtro_periodo = reactive({
        input$filtro_periodo
      }),
      data_inicial = reactive({
        if (is.null(input$data_inicial)) {
          return(Sys.Date() - 365)
        }
        # Garantir que é um objeto Date adequado
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
        # Garantir que é um objeto Date adequado
        if (inherits(input$data_final, "Date")) {
          input$data_final
        } else {
          as.Date(input$data_final)
        }
      })
    ))
  })
}

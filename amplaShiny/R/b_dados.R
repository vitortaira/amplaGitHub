#' b_dados_ui
#'
#' A Shiny module UI for searching nested data frames by "Variável" or "Tabela", with optional date-range filter.
#' @param id Shiny module ID
b_dados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        # Choose search type: "Variável" or "Tabela"
        shiny::selectInput(ns("searchType"), "Tipo de busca:",
          choices = c("Variável", "Tabela")
        ),
        # Choices: either col names (if Variável) or table names (if Tabela)
        shiny::selectInput(ns("searchValue"), "Escolha:", choices = NULL),

        # Date range for filtering. We'll show/hide based on user selection.
        # Default is last 30 days. Adjust as needed.
        shiny::uiOutput(ns("periodo_ui"))
      ),
      shiny::column(
        width = 8,
        DT::DTOutput(ns("dt_result"))
      )
    )
  )
}

#' b_dados_server
#'
#' A Shiny module server for searching nested data frames by "Variável" or "Tabela", with optional date-range filter.
#' @param id Shiny module ID
#' @param dados_l A nested list containing tibbles/data.frames at its leaves.
b_dados_server <- function(id, dados_l) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Flatten the nested list: keep only data frames
    df_list <- shiny::reactive({
      dados_l %>%
        purrr::flatten() %>%
        purrr::keep(~ inherits(.x, "data.frame"))
    })

    # 2) Table names
    table_names <- shiny::reactive({
      names(df_list())
    })

    # 3) All column names across all tables
    col_names <- shiny::reactive({
      df_list() %>%
        purrr::map(names) %>%
        unlist() %>%
        unique() %>%
        sort()
    })

    # 4) When user changes searchType, update searchValue
    shiny::observeEvent(input$searchType,
      {
        if (input$searchType == "Tabela") {
          shiny::updateSelectInput(session, "searchValue", choices = table_names())
        } else {
          shiny::updateSelectInput(session, "searchValue", choices = col_names())
        }
      },
      ignoreInit = TRUE
    )

    # 5) Dynamically show date range only if "Variável" is selected AND chosen column is date/datetime
    output$periodo_ui <- shiny::renderUI({
      if (input$searchType == "Variável" && !is.null(input$searchValue) && input$searchValue != "") {
        # Check if the chosen variable is date/datetime in at least one table
        # We find the first table that has this column; if it's a date/datetime, we show the filter
        col_is_date <- FALSE
        df_list_filtered <- df_list() %>%
          purrr::keep(~ input$searchValue %in% names(.x))
        if (length(df_list_filtered) > 0) {
          # Inspect the first table that has that column
          first_df <- df_list_filtered[[1]]
          the_col <- first_df[[input$searchValue]]
          # If it's a Date, POSIXct, or POSIXlt, we show the dateRangeInput
          col_is_date <- inherits(the_col, "Date") || inherits(the_col, "POSIXt")
        }
        if (col_is_date) {
          shiny::dateRangeInput(
            ns("filtro_periodo"),
            "Filtrar período:",
            start = Sys.Date() - 30,
            end   = Sys.Date()
          )
        } else {
          # If not a date/datetime, no date range input
          NULL
        }
      } else {
        NULL
      }
    })

    # 6) Build a reactive data frame to display
    combined_data <- shiny::reactive({
      shiny::req(input$searchType, input$searchValue)

      # Searching by Tabela => show entire chosen table
      if (input$searchType == "Tabela") {
        return(df_list()[[input$searchValue]])
      }

      # Searching by Variável => gather rows from all tables that contain that col
      chosen_col <- input$searchValue
      df_list_filtered <- df_list() %>%
        purrr::keep(~ chosen_col %in% names(.x))

      if (length(df_list_filtered) == 0) {
        return(dplyr::tibble(Mensagem = "Nenhum data frame contém essa variável."))
      }

      # Combine all relevant rows & add .table_name column
      combined <- purrr::map2_df(
        df_list_filtered,
        names(df_list_filtered),
        ~ dplyr::mutate(.x, .table_name = .y)
      )

      # Check if user selected a date column AND a date range
      # If so, filter by that period
      req_var <- chosen_col
      if (req_var %in% names(combined)) {
        the_col <- combined[[req_var]]
        # If it's a date/datetime, see if user provided a range
        if ((inherits(the_col, "Date") || inherits(the_col, "POSIXt")) &&
          !is.null(input$filtro_periodo)) {
          start_date <- input$filtro_periodo[1]
          end_date <- input$filtro_periodo[2]
          # Filter rows that fall within [start_date, end_date]
          combined <- combined %>%
            dplyr::filter(the_col >= start_date & the_col <= end_date)
        }
      }
      combined
    })

    # 7) Render the resulting table
    output$dt_result <- DT::renderDT({
      shiny::req(combined_data())
      DT::datatable(combined_data())
    })
  })
}

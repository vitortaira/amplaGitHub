#' b_dados_ui
#'
#' A Shiny module UI for searching nested data frames by "Variável" or "Tabela".
#' @param id Shiny module ID
b_dados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::selectInput(ns("searchType"), "Tipo de busca:",
          choices = c("Variável", "Tabela")
        ),
        shiny::selectInput(ns("searchValue"), "Escolha:", choices = NULL)
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
#' A Shiny module server for searching nested data frames by "Variável" or "Tabela".
#' @param id Shiny module ID
#' @param dados_l A nested list containing tibbles/data.frames at its leaves.
b_dados_server <- function(id, dados_l) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Flatten the nested list; keep only data frames
    #    Named list of data frames, with paths as names
    df_list <- shiny::reactive({
      dados_l %>%
        purrr::flatten() %>%                # flatten one level; or flatten_dfr for data frames
        purrr::keep(~ inherits(.x, "data.frame"))  # keep only data frames/tibbles
    })

    # 2) Table names (keys in df_list)
    table_names <- shiny::reactive({
      names(df_list())
    })

    # 3) Collect all column names across all tables
    col_names <- shiny::reactive({
      df_list() %>%
        purrr::map(names) %>%  # each data frame's column names
        unlist() %>%           # unlist them into a single character vector
        unique()               # remove duplicates
    })

    # 4) Update the second dropdown based on user-chosen search type
    shiny::observeEvent(input$searchType,
      {
        if (input$searchType == "Tabela") {
          shiny::updateSelectInput(session, "searchValue",
            choices = table_names()
          )
        } else {
          # Variável
          shiny::updateSelectInput(session, "searchValue",
            choices = col_names()
          )
        }
      },
      ignoreInit = TRUE
    )

    # 5) Build a reactive data frame to display
    combined_data <- shiny::reactive({
      shiny::req(input$searchType, input$searchValue)
      if (input$searchType == "Tabela") {
        # Show the entire chosen table
        df_list()[[input$searchValue]]
      } else {
        # Show rows from all tables containing the chosen column
        chosen_col <- input$searchValue
        df_list_filtered <- df_list() %>%
          purrr::keep(~ chosen_col %in% names(.x))

        if (length(df_list_filtered) == 0) {
          dplyr::tibble(Mensagem = "Nenhum data frame contém essa variável.")
        } else {
          # Row-bind them, adding a .table_name column
          purrr::map2_df(
            df_list_filtered,
            names(df_list_filtered),
            ~ dplyr::mutate(.x, .table_name = .y)
          )
        }
      }
    })

    # 6) Render the resulting table
    output$dt_result <- DT::renderDT({
      shiny::req(combined_data())
      DT::datatable(combined_data())
    })
  })
}

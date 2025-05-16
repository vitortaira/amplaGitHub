# A module providing a 'Dados > Buscar' pane:
# It scans the nested list 'dados_list' for all tibbles (data.frames),
# exposes them by an identifying path, and lets the user browse columns.

# Helper function: Recursively collect paths to all tibbles in a nested list
find_tibble_paths <- function(x, prefix = character()) {
  if (is.data.frame(x)) {
    # Return the path joined by "$" (skip empty prefix)
    return(paste(prefix, collapse = "$"))
  }
  # If it's a list, recurse
  if (is.list(x)) {
    out <- c()
    for (nm in names(x)) {
      new_prefix <- c(prefix, nm)
      out <- c(out, find_tibble_paths(x[[nm]], new_prefix))
    }
    return(out)
  }
  # If it's something else, return nothing
  character()
}

# Helper function: Given a path like "foo$bar$df" and a list, return that tibble
get_tibble_by_path <- function(lst, path) {
  parts <- strsplit(path, "\\$")[[1]]
  obj <- lst
  for (p in parts) {
    obj <- obj[[p]]
  }
  obj
}

# UI module
b_dados_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Buscar"),
    fluidRow(
      column(
        4,
        selectInput(ns("tibble_path"), "Escolha um tibble:", choices = NULL),
        selectInput(ns("col_name"), "Escolha a variável:", choices = NULL),
        uiOutput(ns("col_filter_ui")) # Filter widget for chosen column
      ),
      column(
        8,
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}

# Server module
b_dados_server <- function(id, dados_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Identify all tibble paths in dados_list
    all_paths <- reactive({
      find_tibble_paths(dados_list)
    })

    # 2) Update the tibble_path dropdown
    observeEvent(all_paths(), {
      path_vec <- all_paths()
      if (length(path_vec) == 0) path_vec <- "Nenhum tibble encontrado"
      updateSelectInput(session, "tibble_path",
        choices = path_vec,
        selected = path_vec[1]
      )
    })

    # 3) The currently chosen tibble
    chosen_tibble <- reactive({
      req(input$tibble_path)
      get_tibble_by_path(dados_list, input$tibble_path)
    })

    # 4) Update column choices
    observeEvent(chosen_tibble(), {
      updateSelectInput(session, "col_name",
        choices = names(chosen_tibble()),
        selected = names(chosen_tibble())[1]
      )
    })

    # 5) Output filter UI if column is character/factor
    output$col_filter_ui <- renderUI({
      req(chosen_tibble(), input$col_name)
      col_data <- chosen_tibble()[[input$col_name]]
      if (is.character(col_data) || is.factor(col_data)) {
        selectInput(ns("col_filter"), "Filtrar valor:",
          choices = c("Todos", as.character(unique(col_data)))
        )
      } else {
        NULL
      }
    })

    # 6) Reactive: filter tibble by column value
    filtered_tibble <- reactive({
      req(chosen_tibble(), input$col_name)
      dat <- chosen_tibble()
      if (!is.null(input$col_filter) && input$col_filter != "Todos") {
        dat <- dat[dat[[input$col_name]] == input$col_filter, ]
      }
      dat
    })

    # 7) Preview
    output$preview <- DT::renderDataTable({
      DT::datatable(filtered_tibble())
    })
  })
}

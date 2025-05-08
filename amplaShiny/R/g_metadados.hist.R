g_metadados.hist_i <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Metadados"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = choices[1]
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

g_metadados.hist_o <- function(id, dados, filtro_periodo, data_inicial, data_final) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      req(input$variavel) # Ensure input$variavel is valid
      if (!is.null(dados$metadados)) {
        g_histo_metadados(
          data = dados$metadados,
          var = input$variavel, # Pass the selected variable
          title = "Histogram of Metadata"
        )
      } else {
        plot_ly() %>%
          add_annotations(
            text = "No metadata available",
            showarrow = FALSE
          )
      }
    })
  })
}

# Helper function for generating histograms
g_histo_metadados <- function(data,
                              var,
                              colour_by = NULL,
                              nbins = 30,
                              title = NULL,
                              xlab = NULL,
                              ylab = "Count",
                              bargap = 0.1,
                              unique_files = TRUE) {
  # capture symbols
  var_sym <- ensym(var)
  colour_sym <- if (!is.null(colour_by)) ensym(colour_by)

  # keep one row per Arquivo if requested
  if (unique_files) {
    data <- data %>% distinct(Arquivo, .keep_all = TRUE)
  }

  # prepare plot_ly args
  args <- list(
    data   = data,
    x      = ~ !!var_sym,
    type   = "histogram",
    nbinsx = nbins
  )

  if (!is.null(colour_by)) {
    args$color <- ~ !!colour_sym
  }

  plot_ly(!!!args) %>%
    layout(
      title  = list(text = title),
      xaxis  = list(title = xlab),
      yaxis  = list(title = ylab),
      bargap = bargap
    )
}

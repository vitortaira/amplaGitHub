g_metadados.hist_i <- function(id, choices) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Arquivo_tipo"
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
          plot_title = "Histogram of Metadata"
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
                              plot_title = NULL,
                              xlab = NULL,
                              ylab = "Count",
                              bargap = 0.1,
                              unique_files = TRUE) {
  # Validate data first
  if (is.null(data)) {
    return(plot_ly() %>% add_annotations(text = "No data provided", showarrow = FALSE))
  }

  # Ensure data is a proper data.frame (important fix)
  data <- as.data.frame(data)

  # Ensure the variable exists in the data
  if (!(var %in% names(data))) {
    return(plot_ly() %>% add_annotations(text = paste("Variable not found:", var), showarrow = FALSE))
  }

  # Extract the values directly - avoids complex tidy evaluation that might fail
  x_values <- data[[var]]

  # Create plot directly with extracted values
  p <- plot_ly(x = x_values, type = "histogram", nbinsx = nbins)

  # Safely handle title and labels
  plot_title <- if (is.null(plot_title)) "" else as.character(plot_title)
  xlab <- if (is.null(xlab)) var else as.character(xlab)

  # Layout with explicit type conversions
  plotly::layout(
    p,
    title = list(text = plot_title),
    xaxis = list(title = xlab),
    yaxis = list(title = as.character(ylab)),
    bargap = as.numeric(bargap)
  )
}

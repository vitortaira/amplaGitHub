# Server for Despesas trajectory chart
g_desp.traj_o <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    period <- reactive({
      req(input$filtro_periodo)
      today <- Sys.Date()
      switch(input$filtro_periodo,
        ano_corrente = list(start = floor_date(today, "year"), end = today),
        ultimos_12 = list(start = today %m-% months(12), end = today),
        desde_inicio = {
          dt <- as.Date(dados$desp$`Data Doc Pagto`)
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        personalizado = {
          req(input$data_inicial, input$data_final)
          list(start = input$data_inicial, end = input$data_final)
        }
      )
    })

    output$plot <- renderPlotly({
      req(input$variavel)
      pr <- period()
      df <- dados$desp %>%
        mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = floor_date(.dt, "month"),
          Var = as.character(.data[[input$variavel]])
        ) %>%
        summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop")

      # Compute global order of Var by total value (descending)
      var_levels <- df %>%
        group_by(Var) %>%
        summarise(Total = sum(Total, na.rm = TRUE)) %>%
        arrange(desc(Total)) %>%
        pull(Var)

      df <- df %>%
        mutate(Var = factor(Var, levels = var_levels))

      n <- length(unique(df$Var))
      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (n <= 8) pal8[1:n] else grDevices::colorRampPalette(pal8)(n)

      plot_ly(
        data   = df,
        x      = ~Mês,
        y      = ~Total,
        color  = ~Var,
        colors = pal,
        type   = "bar"
      ) %>%
        layout(
          barmode = "stack",
          autosize = TRUE,
          xaxis = list(
            tickformat = "%b %Y", # Format: "Jan 2025"
            type = "date",
            dtick = "M1", # Force monthly ticks
            ticklabelmode = "period", # Show the period (month) instead of exact date,
            ticklabeloverflow = "allow",
            tickmode = "array", # Use specific tick positions
            tickvals = ~Mês, # Set tick positions to match bar centers
            rangeslider = list(visible = input$filtro_periodo == "desde_inicio") # Optional: disable range slider
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

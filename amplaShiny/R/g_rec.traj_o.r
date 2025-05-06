# Server for Receitas trajectory chart (now uses external filtro_periodo and date inputs)

g_rec.traj_o <- function(id, dados, filtro_periodo, data_inicial, data_final) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for date range
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = floor_date(today, "year"), end = today),
        "ultimos_12" = list(start = today %m-% months(12), end = today),
        "desde_inicio" = {
          dt <- as.Date(dados$rec$`Data Pagto`)
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(data_inicial(), data_final())
          list(start = data_inicial(), end = data_final())
        }
      )
    })

    output$plot <- renderPlotly({
      req(input$variavel) # Keep the local 'variavel' input from g_rec.traj_i
      pr <- period()

      df <- dados$rec %>%
        mutate(.dt = as.Date(`Data Pagto`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = floor_date(.dt, "month"),
          Var = .data[[input$variavel]]
        ) %>%
        summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")

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
            tickformat        = "%b %Y",
            type              = "date",
            dtick             = "M1",
            ticklabelmode     = "period",
            ticklabeloverflow = "allow",
            tickmode          = "array",
            tickvals          = ~Mês,
            # Make the range slider visible only if filtro_periodo() == 'desde_inicio'
            rangeslider       = list(visible = (filtro_periodo() == "desde_inicio"))
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

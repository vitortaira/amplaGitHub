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
          Var = .data[[input$variavel]]
        ) %>%
        summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop") %>%
        mutate(Var = fct_reorder(as.character(Var), Total, sum))

      n <- length(unique(df$Var))
      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (n <= 8) pal8[1:n] else grDevices::colorRampPalette(pal8)(n)

      plot_ly(
        data   = df,
        x      = ~Mês,
        y      = ~Total,
        color  = ~Var,
        colors = pal, # <- supply your custom palette
        type   = "bar"
      ) %>%
        layout(barmode = "stack", autosize = TRUE) %>%
        config(displayModeBar = FALSE)
    })
  })
}

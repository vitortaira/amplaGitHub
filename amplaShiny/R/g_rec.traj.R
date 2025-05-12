# UI for Receitas trajectory chart
g_rec.traj_ui <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Receitas"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Empreendimento"
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

# Server for Receitas trajectory chart (now uses external filtro_periodo and date inputs)

g_rec.traj_server <- function(id, dados, filtro_periodo, data_inicial, data_final) {
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

    # Create dynamic chart title
    chart_title <- reactive({
      req(input$variavel, filtro_periodo())

      # Get variable name for display
      var_name <- input$variavel

      # Format title based on selected period
      period_text <- switch(filtro_periodo(),
        "ano_corrente" = "no Ano Corrente",
        "ultimos_12" = "nos Últimos 12 Meses",
        "desde_inicio" = "desde o Início",
        "personalizado" = {
          req(data_inicial(), data_final())
          sprintf(
            "de %s até %s",
            format(data_inicial(), "%d/%m/%Y"),
            format(data_final(), "%d/%m/%Y")
          )
        }
      )

      # Construct full title
      sprintf("Receitas por %s %s", var_name, period_text)
    })

    output$plot <- renderPlotly({
      req(input$variavel) # Keep the local 'variavel' input from g_rec.traj_ui
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
          title = list(
            text = chart_title(),
            font = list(size = 16)
          ),
          barmode = "stack",
          autosize = TRUE,
          xaxis = list(
            tickformat        = "%b %Y",
            type              = "date",
            dtick             = "M1",
            ticklabelmode     = "period",
            ticklabeloverflow = "allow",
            tickmode          = "array",
            tickvals          = unique(df$Mês),
            rangeslider       = list(visible = (filtro_periodo() == "desde_inicio"))
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

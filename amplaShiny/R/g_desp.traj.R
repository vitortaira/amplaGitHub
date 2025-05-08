# Server and UI for Despesas trajectory chart (modularized, concise, and well-documented)

# UI module for Despesas trajectory chart
g_desp.traj_ui <- function(id, choices) {
  ns <- NS(id)
  tagList(
    h2("Despesas"),
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "Empresa"
    ),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

# Server module for Despesas trajectory chart
g_desp.traj_server <- function(id, dados, filtro_periodo, data_inicial, data_final) {
  moduleServer(id, function(input, output, session) {
    # Compute date range based on selected period
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = lubridate::floor_date(today, "year"), end = today),
        "ultimos_12" = list(start = today %m-% months(12), end = today),
        "desde_inicio" = {
          dt <- as.Date(dados$desp$`Data Doc Pagto`)
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(data_inicial(), data_final())
          list(start = data_inicial(), end = data_final())
        }
      )
    })

    # Render stacked bar chart with dynamic grouping and color palette
    output$plot <- renderPlotly({
      req(input$variavel)
      pr <- period()

      df <- dados$desp %>%
        mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = lubridate::floor_date(.dt, "month"),
          Var = as.character(.data[[input$variavel]])
        ) %>%
        summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop")

      # Order factor levels by total descending for consistent coloring
      var_levels <- df %>%
        group_by(Var) %>%
        summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        pull(Var)

      df <- df %>% mutate(Var = factor(Var, levels = var_levels))

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
            tickformat = "%b %Y",
            type = "date",
            dtick = "M1",
            ticklabelmode = "period",
            ticklabeloverflow = "allow",
            tickmode = "array",
            tickvals = unique(df$Mês),
            rangeslider = list(visible = (filtro_periodo() == "desde_inicio"))
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

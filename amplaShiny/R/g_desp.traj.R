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
    ns <- session$ns

    # Compute date range based on selected period
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = floor_date(today, "year"), end = today),
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

    # Create dynamic chart title
    chart_title <- reactive({
      req(input$variavel, filtro_periodo())
      # Put the variable name inside single quotes
      var_name <- paste0("'", input$variavel, "'")
      period_text <- switch(filtro_periodo(),
        "ano_corrente" = "no ano corrente",
        "ultimos_12" = "nos últimos 12 meses",
        "desde_inicio" = "desde o início",
        "personalizado" = {
          req(data_inicial(), data_final())
          sprintf(
            "de %s até %s",
            format(data_inicial(), "%d/%m/%Y"),
            format(data_final(), "%d/%m/%Y")
          )
        }
      )
      sprintf("Trajetória das despesas por %s %s", var_name, period_text)
    })

    # Reactive data preparation
    df_data <- reactive({
      pr <- period()
      dados$desp %>%
        mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = floor_date(.dt, "month"),
          Var = as.character(.data[[input$variavel]])
        ) %>%
        summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop")
    })

    # Calculate monthly totals separately
    monthly_totals <- reactive({
      df_data() %>%
        group_by(Mês) %>%
        summarise(MonthTotal = sum(Total, na.rm = TRUE), .groups = "drop")
    })

    # Prepare final dataframe with percentages
    df_final <- reactive({
      df_data() %>%
        left_join(monthly_totals(), by = "Mês") %>%
        mutate(
          MonthTotal = ifelse(MonthTotal == 0, NA, as.numeric(MonthTotal)),
          Percentage = (Total / MonthTotal) * 100
        )
    })

    # Render stacked bar chart with dynamic grouping and color palette
    output$plot <- renderPlotly({
      # Ensure we have data before proceeding
      df <- df_final()
      req(df, nrow(df) > 0)

      # Calculate variable levels for consistent ordering and coloring
      var_levels <- df %>%
        group_by(Var) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        pull(Var)

      # Convert Var to ordered factor
      df <- df %>% mutate(Var = factor(Var, levels = var_levels))

      # Color palette
      n <- length(var_levels)
      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (n <= 8) pal8[1:n] else colorRampPalette(pal8)(n)

      # Build the stacked bar chart
      p <- plot_ly(source = "despPlot")
      for (i in seq_along(var_levels)) {
        lvl <- var_levels[i]
        sub_df <- dplyr::filter(df, Var == lvl)

        if (nrow(sub_df) > 0) {
          # Ensure data is numeric
          sub_df$MonthTotal <- as.numeric(sub_df$MonthTotal)
          sub_df$Percentage <- as.numeric(sub_df$Percentage)

          p <- p %>%
            add_trace(
              data = sub_df,
              x = ~Mês,
              y = ~Total,
              name = lvl,
              type = "bar",
              marker = list(color = pal[i]),
              key = lvl,
              # Use named fields in customdata instead of array
              customdata = lapply(1:nrow(sub_df), function(i) {
                list(
                  monthTotal = sub_df$MonthTotal[i],
                  percentage = sub_df$Percentage[i]
                )
              }),
              hovertemplate = paste0(
                "<b>%{fullData.name}</b><br>",
                "Valor: R$ %{y:,.2f}<br>",
                "Total do mês: R$ %{customdata.monthTotal:,.2f}<br>",
                "Percentual: %{customdata.percentage:.1f}%<br>",
                "<extra></extra>"
              )
            )
        }
      }
      p <- p %>%
        layout(
          title = list(text = chart_title(), font = list(size = 16)),
          barmode = "stack",
          xaxis = list(
            tickformat = "%b %Y",
            type = "date",
            tickvals = unique(df$Mês),
            rangeslider = list(visible = (filtro_periodo() == "desde_inicio"))
          ),
          autosize = TRUE
        ) %>%
        event_register("plotly_click") %>% # Register the click event here
        config(displayModeBar = FALSE)
      return(p)
    })

    # Observer for click events
    observeEvent(event_data("plotly_click", source = "despPlot"), {
      click_data <- event_data("plotly_click", source = "despPlot")
      if (!is.null(click_data)) {
        clicked_month <- as.POSIXct(click_data$x, origin = "1970-01-01")
        # Use the key set in each trace:
        clicked_var <- click_data$key

        # Filter the underlying dataset based on the clicked month and variable.
        detail_data <- dados$desp %>%
          mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
          filter(
            lubridate::floor_date(.dt, "month") ==
              lubridate::floor_date(clicked_month, "month"),
            as.character(.data[[input$variavel]]) == clicked_var
          )

        # If no rows match, show a message instead of crashing
        if (nrow(detail_data) == 0) {
          showModal(modalDialog(
            title = "No details found",
            "No matching data for the selected segment.",
            easyClose = TRUE
          ))
        } else {
          detail_data <- detail_data %>% select(-.dt)
          showModal(modalDialog(
            title = paste("Detalhes:", clicked_var, "-", format(clicked_month, "%b %Y")),
            DT::dataTableOutput(ns("detail_table")),
            size = "l",
            easyClose = TRUE
          ))
          output$detail_table <- DT::renderDataTable({
            DT::datatable(detail_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                language = list(
                  url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"
                )
              )
            )
          })
        }
      }
    })
  })
}

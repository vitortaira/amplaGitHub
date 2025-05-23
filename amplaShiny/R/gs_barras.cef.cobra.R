#### r
#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaShiny\R\gs_barras.cef.cobra.R

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
gs_barras.cef.cobra_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("empreend_select"),
      "EMPREENDIMENTO",
      choices = character(0),  # Will be updated in server
      selected = NULL
    ),
    # Placeholder for one or multiple plots
    uiOutput(ns("plot_container"))
  )
}

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
gs_barras.cef.cobra_server <- function(
  id,
  dados,            # normal data.frame
  filtro_periodo,
  data_inicial,
  data_final
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Gather distinct EMPREENDIMENTO values
    empreend_values <- sort(unique(dados[["EMPREENDIMENTO"]]))
    # Add "Mostrar todos" on top
    full_choices <- c("Mostrar todos", empreend_values)

    # 2) Populate the selectInput
    observe({
      updateSelectInput(
        session, "empreend_select",
        choices = full_choices,
        selected = empreend_values[1]  # or "Mostrar todos"
      )
    })

    # 3) Determine date range from filtro_periodo
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = floor_date(today, "year"), end = today),
        "ultimos_12"   = list(start = today %m-% months(12), end = today),
        "desde_inicio" = {
          all_dates <- as.Date(dados[["Data de consulta"]], origin = "1970-01-01")
          list(start = min(all_dates, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(data_inicial(), data_final())
          list(start = data_inicial(), end = data_final())
        }
      )
    })

    # 4) Define a function that produces the same stacked bar + line plot
    build_plot <- function(df_filtered) {
      req(nrow(df_filtered) > 0)

      # Summarize data by month
      pr <- period()
      df_summarized <- df_filtered %>%
        mutate(.dt = as.Date(`Data de consulta`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(Mês = floor_date(.dt, "month")) %>%
        summarise(
          PJ       = sum(`SALDO MUTUARIO (PJ)`, na.rm = TRUE),
          PF       = sum(`SALDO MUTUARIO (PF)`, na.rm = TRUE),
          Etapa    = sum(`MAXIMO LIB. ETAPA (PJ)`, na.rm = TRUE),
          Garantia = sum(`GARANTIA TERMINO OBRA`, na.rm = TRUE),
          .groups  = "drop"
        )

      req(nrow(df_summarized) > 0)

      # Build the plot
      p <- plot_ly(df_summarized, x = ~Mês) %>%
        add_trace(y = ~PJ, type = "bar", name = "SALDO MUTUARIO (PJ)", marker = list(color = "#66c2a5")) %>%
        add_trace(y = ~PF, type = "bar", name = "SALDO MUTUARIO (PF)", marker = list(color = "#fc8d62")) %>%
        add_trace(y = ~(-Etapa), type = "bar", name = "-MAXIMO LIB. ETAPA (PJ)", marker = list(color = "#8da0cb")) %>%
        add_trace(y = ~Garantia, type = "scatter", mode = "lines+markers",
          name = "GARANTIA TERMINO OBRA", yaxis = "y2", line = list(color = "black", width = 2)) %>%
        layout(
          barmode = "relative",
          xaxis = list(type = "date", tickformat = "%m-%Y", title = "Mês"),
          yaxis = list(title = "Valores"),
          yaxis2 = list(title = "Garantia", overlaying = "y", side = "right"),
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
          title = unique(df_filtered$EMPREENDIMENTO)[1],
          hovermode = "x",
          margin = list(b = 80)
        ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE)

      p
    }

    # 5) UI logic: single chart or multiple
    output$plot_container <- renderUI({
      req(input$empreend_select)
      if (input$empreend_select == "Mostrar todos") {
        # Return one plotlyOutput per distinct EMPREENDIMENTO
        tagList(
          lapply(seq_along(empreend_values), function(i) {
            plotlyOutput(ns(paste0("plot_", i)), height = "400px")
          })
        )
      } else {
        # Return just a single plot
        plotlyOutput(ns("plot_single"), height = "600px")
      }
    })

    # 6) Render the single plot if a specific EMPREENDIMENTO is chosen
    output$plot_single <- renderPlotly({
      req(input$empreend_select != "Mostrar todos")
      df_filtered <- dados %>%
        filter(EMPREENDIMENTO == input$empreend_select)
      build_plot(df_filtered)
    })

    # 7) Render multiple smaller plots if “Mostrar todos” is chosen
    observeEvent(input$empreend_select, {
      if (input$empreend_select == "Mostrar todos") {
        for (i in seq_along(empreend_values)) {
          local({
            idx <- i
            empVal <- empreend_values[idx]
            output[[paste0("plot_", idx)]] <- renderPlotly({
              df_filtered <- dados %>% filter(EMPREENDIMENTO == empVal)
              build_plot(df_filtered) %>% layout(height = 350)
            })
          })
        }
      }
    })
  })
}

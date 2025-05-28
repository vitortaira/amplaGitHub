#### r
#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaShiny\R\gs_barras.empilhadas.mes.R

#' @importFrom shiny NS tagList uiOutput moduleServer reactive req renderUI observe observeEvent plotlyOutput renderPlotly
#' @importFrom dplyr filter mutate group_by summarise
#' @importFrom lubridate floor_date %m-% months
#' @importFrom plotly plot_ly add_trace config

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
gs_barras.empilhadas.mes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Replace static selectInput with a placeholder
    uiOutput(ns("empreend_select_container")),

    # Keep the plot container
    uiOutput(ns("plot_container"))
  )
}

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
gs_barras.empilhadas.mes_server <- function(
    id,
    dados,
    filtro_periodo,
    data_inicial,
    data_final,
    positive = c("SALDO MUTUARIO (PJ)", "SALDO MUTUARIO (PF)"),
    negative = c("MAXIMO LIB. ETAPA (PJ)"),
    line = "GARANTIA TERMINO OBRA",
    date = "Data de consulta",
    ref_line_col = "VR CUSTO OBRA") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) CRITICAL FIX: Define empreend_values as a REACTIVE at module level
    empreend_values <- reactive({
      req(dados)
      sort(unique(as.character(dados$Empreendimento)))
    })

    # 2) Use empreend_values() with parentheses in renderUI
    output$empreend_select_container <- renderUI({
      selectInput(
        inputId = ns("empreend_select"),
        label = "Empreendimento(s):",
        choices = c("Mostrar todos", empreend_values()), # <-- note the parentheses
        selected = "Mostrar todos"
      )
    })

    # 3) Debug messages (can keep if needed)
    observe({
      message("DEBUGGING: nrow(dados) = ", nrow(dados))
      message("DEBUGGING: empreend_values length = ", length(empreend_values()))
    })

    # 4) Determine date range from filtro_periodo
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = floor_date(today, "year"), end = today),
        "ultimos_12" = list(start = today %m-% months(12), end = today),
        "desde_inicio" = {
          all_dates <- as.Date(dados[[date]], origin = "1970-01-01")
          list(start = min(all_dates, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(data_inicial(), data_final())
          list(start = data_inicial(), end = data_final())
        }
      )
    })

    # 5) Define a function that produces the same stacked bar + line plot
    build_plot <- function(df_filtered) {
      req(nrow(df_filtered) > 0)

      # Get the reference line value (assuming it's constant for the Empreendimento)
      ref_line_value <- mean(df_filtered[[ref_line_col]], na.rm = TRUE)

      # Summarize data by month
      pr <- period()
      df_summarized <- df_filtered %>%
        mutate(.dt = as.Date(df_filtered[[date]])) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(Mês = floor_date(.dt, "month"))

      # Build summarize expression dynamically
      summary_exprs <- list(.groups = "drop")

      # Add positive columns
      for (col in positive) {
        col_clean <- make.names(col)
        summary_exprs[[col_clean]] <- rlang::expr(sum(!!rlang::sym(col), na.rm = TRUE))
      }

      # Add negative columns
      for (col in negative) {
        col_clean <- make.names(col)
        summary_exprs[[col_clean]] <- rlang::expr(sum(!!rlang::sym(col), na.rm = TRUE))
      }

      # Add line column
      if (!is.null(line)) {
        line_clean <- make.names(line)
        summary_exprs[[line_clean]] <- rlang::expr(sum(!!rlang::sym(line), na.rm = TRUE))
      }

      # Apply summarization
      df_summarized <- df_summarized %>%
        summarise(!!!summary_exprs)

      req(nrow(df_summarized) > 0)

      # Color palettes and pretty names
      colors_positive <- c("#66c2a5", "#fc8d62", "8da0cb", "#e78ac3", "#a6d854")
      colors_negative <- c("#8da0cb", "#e78ac3", "#a6d854")
      pretty_names <- c(
        "SALDO MUTUARIO (PJ)" = "Saldo mutuário (PJ)",
        "SALDO MUTUARIO (PF)" = "Saldo mutuário (PF)",
        "MAXIMO LIB. ETAPA (PJ)" = "Valor máximo de liberação da etapa (PJ)",
        "GARANTIA TERMINO OBRA" = "Custo obra a incorrer",
        "VR CUSTO OBRA" = "Custo obra total"
      )
      # Build the plot
      p <- plot_ly(df_summarized, x = ~Mês)
      # Add positive traces
      for (i in seq_along(positive)) {
        col <- positive[i]
        col_clean <- make.names(col)
        color_idx <- (i - 1) %% length(colors_positive) + 1
        p <- p %>% add_trace(
          y = as.formula(paste0("~`", col_clean, "`")),
          type = "bar",
          name = pretty_names[[col]],
          marker = list(color = colors_positive[color_idx])
        )
      }
      # Add negative traces
      for (i in seq_along(negative)) {
        col <- negative[i]
        col_clean <- make.names(col)
        color_idx <- (i - 1) %% length(colors_negative) + 1
        p <- p %>% add_trace(
          y = as.formula(paste0("~(-`", col_clean, "`)")),
          type = "bar",
          name = pretty_names[[col]],
          marker = list(color = colors_negative[color_idx])
        )
      }
      # Add line trace if specified
      if (!is.null(line)) {
        line_clean <- make.names(line)
        p <- p %>% add_trace(
          y = as.formula(paste0("~`", line_clean, "`")),
          type = "scatter",
          mode = "lines+markers",
          name = pretty_names[[line]],
          line = list(color = "black", width = 2)
        )
      }
      # Layout with reference line
      p <- p %>%
        layout(
          barmode = "relative",
          xaxis = list(
            type = "date",
            tickformat = "%b-%Y",
            title = "Mês",
            tickmode = "array",
            tickvals = df_summarized$Mês,
            ticktext = format(df_summarized$Mês, "%b-%Y"),
            fixedrange = TRUE,
            ticklabelmode = "period",
            tickson = "boundaries",
            ticklabelposition = "inside",
            tickangle = 0
          ),
          yaxis = list(title = "Valores", fixedrange = TRUE),
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
          title = unique(df_filtered$Empreendimento)[1],
          hovermode = "x",
          margin = list(b = 80),
          shapes = list(
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              y0 = ref_line_value,
              y1 = ref_line_value,
              xref = "paper",
              yref = "y",
              line = list(
                color = "red",
                dash = "dash",
                width = 2
              )
            )
          ),
          annotations = list(
            list(
              x = 0,
              y = ref_line_value,
              xref = "paper",
              yref = "y",
              text = paste(pretty_names[[ref_line_col]], ":", format(ref_line_value, big.mark = ".", decimal.mark = ",")),
              showarrow = FALSE,
              xanchor = "left",
              bgcolor = "rgba(255, 255, 255, 0.8)",
              bordercolor = "red",
              borderwidth = 1,
              borderpad = 4
            )
          )
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = list(
            "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines", "sendDataToCloud", "toggleHover", "resetViews", "resetViewMapbox", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo", "plotlyLogo"
          ),
          modeBarButtonsToKeep = list("toImage"),
          toImageButtonOptions = list(format = "png"),
          scrollZoom = FALSE
        )

      p
    }

    # 6) UI logic: single chart or multiple
    output$plot_container <- renderUI({
      req(input$empreend_select)
      if (input$empreend_select == "Mostrar todos") {
        # Return one plotlyOutput per distinct Empreendimento
        tagList(
          lapply(seq_along(empreend_values()), function(i) { # <-- add parentheses
            plotlyOutput(ns(paste0("plot_", i)), height = "400px")
          })
        )
      } else {
        # Return just a single plot
        plotlyOutput(ns("plot_single"), height = "600px")
      }
    })

    # 7) Render the single plot if a specific Empreendimento is chosen
    output$plot_single <- renderPlotly({
      req(input$empreend_select != "Mostrar todos")
      df_filtered <- dados %>%
        filter(Empreendimento == input$empreend_select)
      build_plot(df_filtered)
    })

    # 8) Render multiple smaller plots if "Mostrar todos" is chosen
    observeEvent(input$empreend_select, {
      if (input$empreend_select == "Mostrar todos") {
        for (i in seq_along(empreend_values())) { # <-- add parentheses
          local({
            idx <- i
            empVal <- empreend_values()[idx] # <-- add parentheses
            output[[paste0("plot_", idx)]] <- renderPlotly({
              df_filtered <- dados %>% filter(Empreendimento == empVal)
              build_plot(df_filtered) %>% layout(height = 350)
            })
          })
        }
      }
    })
  })
}

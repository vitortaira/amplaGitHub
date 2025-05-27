# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
g_cronogramas_cef_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_cronogramas"), height = "600px")
}

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
g_cronogramas_cef_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Names of the 6 important dates in uppercase (wide format)
    nomes_datas <- c(
      "DATA DE ASSINATURA",
      "DATA INICIO OBRA",
      "DATA TERMINO SUSPENSIVA",
      "DATA TERMINO OBRA ORIGINAL",
      "DATA TERMINO OBRA ATUAL",
      "DT INICIO ROTINA ATRASO OBRA"
    )

    # Custom shapes and colors
    shapes <- c(
      "square", # DATA DE ASSINATURA (yellow square)
      "circle", # DATA INICIO OBRA (yellow circle)
      "square", # DATA TERMINO SUSPENSIVA (green square)
      "circle", # DATA TERMINO OBRA ORIGINAL (lightgreen circle)
      "circle", # DATA TERMINO OBRA ATUAL (green circle)
      "x" # DT INICIO ROTINA ATRASO OBRA (red x)
    )
    names(shapes) <- nomes_datas

    cores <- c(
      "DATA DE ASSINATURA" = "#FFD600", # yellow
      "DATA INICIO OBRA" = "#FFD600", # yellow
      "DATA TERMINO SUSPENSIVA" = "#43A047", # green
      "DATA TERMINO OBRA ORIGINAL" = "#B2FF59", # lightgreen
      "DATA TERMINO OBRA ATUAL" = "#43A047", # green
      "DT INICIO ROTINA ATRASO OBRA" = "#D50000" # red
    )

    # Pivot from wide to long format
    df_cron <- dados %>%
      mutate(EMPREENDIMENTO = factor(EMPREENDIMENTO)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(nomes_datas),
        names_to = "Marco",
        values_to = "Data"
      ) %>%
      filter(!is.na(Data))

    # Add jitter to overlapping points
    df_cron <- df_cron %>%
      group_by(EMPREENDIMENTO, Data) %>%
      mutate(jitter = (row_number() - 1) * 0.15) %>%
      ungroup()

    # Gray segments: min/max for each empreendimento
    linhas_base <- df_cron %>%
      group_by(EMPREENDIMENTO) %>%
      summarise(x0 = min(Data), x1 = max(Data), .groups = "drop")

    # Build plotly
    fig <- plot_ly()

    # Add gray segments
    fig <- fig %>%
      add_segments(
        data = linhas_base,
        x = ~x0, xend = ~x1,
        y = ~ as.numeric(EMPREENDIMENTO), yend = ~ as.numeric(EMPREENDIMENTO),
        line = list(color = "lightgray", width = 6),
        hoverinfo = "none",
        showlegend = FALSE
      )

    # Add one trace per Marco for correct color and symbol mapping
    for (marco in nomes_datas) {
      df_marco <- df_cron %>% filter(Marco == marco)
      if (nrow(df_marco) > 0) {
        fig <- fig %>%
          add_markers(
            data = df_marco,
            x = ~Data,
            y = ~ as.numeric(EMPREENDIMENTO) + jitter,
            name = marco,
            marker = list(
              size = 14,
              symbol = shapes[marco],
              color = cores[marco],
              line = list(width = 1, color = "black")
            ),
            hovertemplate = paste("<b>%{customdata}</b><br>", marco, "<br>%{text}<extra></extra>"),
            text = ~ paste(format(Data, "%d/%m/%Y")),
            customdata = ~EMPREENDIMENTO
          )
      }
    }

    fig <- fig %>%
      layout(
        title = "Cronogramas por Empreendimento",
        xaxis = list(
          title = "Data",
          type = "date",
          rangeslider = list(
            visible = TRUE,
            yaxis = list(range = c(0, 0)), # This disables the mini-chart!
            bgcolor = "white",
            thickness = 0.07,
            bordercolor = "#ddd",
            borderwidth = 1
          )
        ),
        yaxis = list(
          title = "Empreendimento",
          tickvals = seq_along(levels(df_cron$EMPREENDIMENTO)),
          ticktext = levels(df_cron$EMPREENDIMENTO),
          autorange = "reversed"
        ),
        legend = list(title = list(text = "Marco")),
        hoverlabel = list(namelength = -1),
        margin = list(l = 90, r = 10, t = 60, b = 40)
      ) %>%
      config(
        displayModeBar = TRUE,
        toImageButtonOptions = list(format = "png"),
        scrollZoom = FALSE,      # disables zoom with scroll
        doubleClick = FALSE,     # disables double click zoom
        staticPlot = FALSE,      # disables all interactivity if TRUE (set to FALSE to keep hover)
        modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "zoomIn2d", "zoomOut2d", "resetScale2d")
      )

    # Render
    output$plot_cronogramas <- renderPlotly({
      fig
    })
  })
}

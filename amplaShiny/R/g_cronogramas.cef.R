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

    nomes_datas <- c(
      "DATA DE ASSINATURA",
      "DATA INICIO OBRA",
      "DATA TERMINO SUSPENSIVA",
      "DATA TERMINO OBRA ORIGINAL",
      "DATA TERMINO OBRA ATUAL",
      "DT INICIO ROTINA ATRASO OBRA"
    )

    shapes <- c(
      "square", "circle", "square", "circle", "circle", "x"
    )
    names(shapes) <- nomes_datas

    cores <- c(
      "DATA DE ASSINATURA" = "#FFD600",
      "DATA INICIO OBRA" = "#FFD600",
      "DATA TERMINO SUSPENSIVA" = "#43A047",
      "DATA TERMINO OBRA ORIGINAL" = "#B2FF59",
      "DATA TERMINO OBRA ATUAL" = "#43A047",
      "DT INICIO ROTINA ATRASO OBRA" = "#D50000"
    )

    df_cron <- dados %>%
      mutate(Empreendimento = factor(EMPREENDIMENTO)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(nomes_datas),
        names_to = "Marco",
        values_to = "Data"
      ) %>%
      filter(!is.na(Data))

    # Add jitter to overlapping points
    df_cron <- df_cron %>%
      group_by(Empreendimento, Data) %>%
      mutate(jitter = (row_number() - 1) * 0.05) %>%
      ungroup()

    # Gray segments: min/max for each empreendimento
    linhas_base <- df_cron %>%
      group_by(Empreendimento) %>%
      summarise(x0 = min(Data), x1 = max(Data), .groups = "drop")

    # Highlighted segments: from DATA INICIO OBRA to DATA TERMINO OBRA ATUAL
    destaques <- dados %>%
      filter(!is.na(`DATA INICIO OBRA`), !is.na(`DATA TERMINO OBRA ATUAL`)) %>%
      mutate(Empreendimento = factor(EMPREENDIMENTO, levels = levels(df_cron$EMPREENDIMENTO))) %>%
      select(Empreendimento, x0 = `DATA INICIO OBRA`, x1 = `DATA TERMINO OBRA ATUAL`)

    fig <- plot_ly()

    # Add gray segments (full project duration)
    fig <- fig %>%
      add_segments(
        data = linhas_base,
        x = ~x0, xend = ~x1,
        y = ~ as.numeric(Empreendimento), yend = ~ as.numeric(Empreendimento),
        line = list(color = "lightgray", width = 6),
        hoverinfo = "none",
        showlegend = FALSE
      )

    # Add highlighted segments (obra period)
    fig <- fig %>%
      add_segments(
        data = destaques,
        x = ~x0, xend = ~x1,
        y = ~ as.numeric(Empreendimento), yend = ~ as.numeric(Empreendimento),
        line = list(color = "#D50000", width = 6), # red and thinner
        hoverinfo = "none",
        name = "Período de obra",
        showlegend = TRUE
      )

    # Custom legend labels
    legendas <- c(
      "DATA DE ASSINATURA" = "Assinatura do contrato",
      "DATA INICIO OBRA" = "Início da obra",
      "DATA TERMINO SUSPENSIVA" = "Término da suspensiva do contrato",
      "DATA TERMINO OBRA ORIGINAL" = "Término original da obra",
      "DATA TERMINO OBRA ATUAL" = "Término atual da obra",
      "DT INICIO ROTINA ATRASO OBRA" = "Início de rotina de atraso da obra"
    )

    # Add one trace per Marco for correct color and symbol mapping
    for (marco in nomes_datas) {
      df_marco <- df_cron %>% filter(Marco == marco)
      if (nrow(df_marco) > 0) {
        fig <- fig %>%
          add_markers(
            data = df_marco,
            x = ~Data,
            y = ~ as.numeric(Empreendimento) + jitter,
            name = legendas[marco], # Use custom legend label
            marker = list(
              size = 14,
              symbol = shapes[marco],
              color = cores[marco],
              line = list(width = 1, color = "black")
            ),
            hovertemplate = paste("<b>%{customdata}</b><br>", legendas[marco], "<br>%{text}<extra></extra>"),
            text = ~ paste(format(Data, "%d/%m/%Y")),
            customdata = ~Empreendimento
          )
      }
    }

    fig <- fig %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Data",
          type = "date",
          dragmode = "false",
          fixedrange = TRUE,
          rangeslider = list(
            visible = TRUE,
            yaxis = list(range = c(0, 0)),
            bgcolor = "#f5f5f5",
            thickness = 0.25, # more separation
            bordercolor = "#bbb",
            borderwidth = 1
            # pad removed, as it is not respected by plotly.js in R
          )
        ),
        yaxis = list(
          title = list(text = "Empreendimento", standoff = 50),
          fixedrange = TRUE,
          tickvals = seq_along(levels(df_cron$Empreendimento)),
          ticktext = levels(df_cron$Empreendimento),
          autorange = "reversed",
          automargin = TRUE,
          standoff = 30 # Space between y label and tick labels
        ),
        legend = list(title = list(text = "Datas")),
        hoverlabel = list(namelength = -1),
        margin = list(l = 200, r = 10, t = 10, b = 40) # Larger left margin
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToAdd = list("toImage"),
        toImageButtonOptions = list(format = "png"),
        scrollZoom = FALSE
      )

    output$plot_cronogramas <- renderPlotly({
      fig
    })
  })
}

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
      "data.assinatura",
      "data.inicio.obra",
      "data.termino.suspensiva",
      "data.termino.obra.original",
      "data.termino.obra.atual",
      "data.inicio.rotina.atraso.obra"
    )

    shapes <- c(
      "square", "circle", "square", "circle", "circle", "x"
    )
    names(shapes) <- nomes_datas

    cores <- c(
      "data.assinatura" = "#FFD600",
      "data.inicio.obra" = "#FFD600",
      "data.termino.suspensiva" = "#43A047",
      "data.termino.obra.original" = "#B2FF59",
      "data.termino.obra.atual" = "#43A047",
      "data.inicio.rotina.atraso.obra" = "#D50000"
    )

    df_cron <- dados %>%
      mutate(empreendimento = factor(empreendimento)) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(nomes_datas),
        names_to = "marco",
        values_to = "data"
      ) %>%
      filter(!is.na(data))

    # Add jitter to overlapping points
    df_cron <- df_cron %>%
      group_by(empreendimento, data) %>%
      mutate(jitter = (row_number() - 1) * 0.05) %>%
      ungroup()

    # Gray segments: min/max for each empreendimento
    linhas_base <- df_cron %>%
      group_by(empreendimento) %>%
      summarise(x0 = min(data), x1 = max(data), .groups = "drop")

    # Highlighted segments: from data.inicio.obra to data.termino.obra.atual
    destaques <- dados %>%
      filter(!is.na(`data.inicio.obra`), !is.na(`data.termino.obra.atual`)) %>%
      mutate(
        empreendimento = factor(empreendimento,
        levels = levels(df_cron$empreendimento))
      ) %>%
      select(empreendimento, x0 = `data.inicio.obra`, x1 = `data.termino.obra.atual`)

    fig <- plot_ly()

    # Add gray segments (full project duration)
    fig <- fig %>%
      add_segments(
        data = linhas_base,
        x = ~x0, xend = ~x1,
        y = ~ as.numeric(empreendimento), yend = ~ as.numeric(empreendimento),
        line = list(color = "lightgray", width = 6),
        hoverinfo = "none",
        showlegend = FALSE
      )

    # Add highlighted segments (obra period)
    fig <- fig %>%
      add_segments(
        data = destaques,
        x = ~x0, xend = ~x1,
        y = ~ as.numeric(empreendimento), yend = ~ as.numeric(empreendimento),
        line = list(color = "#D50000", width = 6), # red and thinner
        hoverinfo = "none",
        name = "Período de obra",
        showlegend = TRUE
      )

    # Custom legend labels
    legendas <- c(
      "data.assinatura" = "Assinatura do contrato",
      "data.inicio.obra" = "Início da obra",
      "data.termino.suspensiva" = "Término da suspensiva do contrato",
      "data.termino.obra.original" = "Término original da obra",
      "data.termino.obra.atual" = "Término atual da obra",
      "data.inicio.rotina.atraso.obra" = "Início de rotina de atraso da obra"
    )

    # Add one trace per marco for correct color and symbol mapping
    for (marco in nomes_datas) {
      df_marco <- df_cron %>% filter(marco == marco)
      if (nrow(df_marco) > 0) {
        fig <- fig %>%
          add_markers(
            data = df_marco,
            x = ~data,
            y = ~ as.numeric(empreendimento) + jitter,
            name = legendas[marco], # Use custom legend label
            marker = list(
              size = 14,
              symbol = shapes[marco],
              color = cores[marco],
              line = list(width = 1, color = "black")
            ),
            hovertemplate = paste("<b>%{customdata}</b><br>", legendas[marco], "<br>%{text}<extra></extra>"),
            text = ~ paste(format(data, "%d/%m/%Y")),
            customdata = ~empreendimento
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
          tickvals = seq_along(levels(df_cron$empreendimento)),
          ticktext = levels(df_cron$empreendimento),
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

# Server and UI for Despesas trajectory chart (modularized, concise, and well-documented)

# ----------------------------
#        UI MODULE
# ----------------------------
g_barras.empilhadas.mes_ui <- function(
    id,
    choices,
    total = "total.pago", # default column name
    data = "data.doc.pagto", # default date column name
    comeco.titulo = "Despesas" # default chart title prefix
    ) {
  ns <- NS(id)
  tagList(
    # Let user pick stacking variable
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = "empresa"
    ),
    # Placeholder for the checkbox (conditionally shown in the server)
    uiOutput(ns("checkbox_wrapper")),
    plotlyOutput(
      ns("plot"),
      height = "600px"
    )
  )
}

# ----------------------------
#       SERVER MODULE
# ----------------------------
g_barras.empilhadas.mes_server <- function(
    id,
    dados,
    filtro_periodo,
    data_inicial,
    data_final,
    max_unicos_i = 20,
    total = "total.pago",
    data = "data.doc.pagto",
    comeco.titulo = "Despesas") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Map each ID to its matching Plotly source string:
    plotly_source <- reactive({
      switch(id,
        "g_barras.empilhadas.mes.desp"   = "g_barras.empilhadas.mes.click.desp",
        "g_barras.empilhadas.mes.extcef" = "g_barras.empilhadas.mes.click.extcef",
        "g_barras.empilhadas.mes.rec"    = "g_barras.empilhadas.mes.click.rec"
      )
    })

    detail_rv <- reactiveVal(NULL)
    top_vars_rv <- reactiveVal(NULL)

    # 1) Reactive for date range (unchanged)
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = floor_date(today, "year"), end = today),
        "ultimos_12" = list(start = today %m-% months(12), end = today),
        "desde_inicio" = {
          dt <- as.Date(dados[[data]], origin = "1970-01-01")
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(data_inicial(), data_final())
          list(start = data_inicial(), end = data_final())
        }
      )
    })

    # 2) Chart title, using comeco.titulo
    chart_title <- reactive({
      req(input$variavel, filtro_periodo())
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
      # Combine the static prefix with the variable and date info
      sprintf("%s %s %s", comeco.titulo, var_name, period_text)
    })

    # 3) Reactive data: group by month + stacking variable
    df_data <- reactive({
      pr <- period()
      req(dados)
      dados %>%
        mutate(.dt = as.Date(.data[[data]])) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = floor_date(.dt, "month"),
          Var = as.character(.data[[input$variavel]])
        ) %>%
        summarise(
          Total = sum(.data[[total]], na.rm = TRUE),
          .groups = "drop"
        )
    })

    # 4) Conditionally show or hide the checkbox
    output$checkbox_wrapper <- renderUI({
      d <- df_data()
      req(d)
      if (n_distinct(d$Var) <= max_unicos_i) {
        return(NULL) # hide checkbox completely
      }
      checkboxInput(
        inputId = ns("show_all_cats"),
        label = div(
          style = "white-space: normal; word-wrap: break-word; max-width: 300px;",
          sprintf("Mostrar todas as categorias de '%s'", input$variavel)
        ),
        value = FALSE
      )
    })

    # 5) Possibly group categories into “Outros”
    df_reduced <- reactive({
      d <- df_data()
      req(d)

      # If user wants all or not many categories => do nothing
      if (n_distinct(d$Var) <= max_unicos_i || isTRUE(input$show_all_cats)) {
        top_vars_rv(NULL)
        return(d)
      }

      # Identify top (max_unicos_i - 1) categories
      totals_by_var <- d %>%
        group_by(Var) %>%
        summarise(TotalVar = sum(Total), .groups = "drop") %>%
        arrange(desc(TotalVar))

      top_vars <- totals_by_var$Var[seq_len(max_unicos_i - 1)]
      top_vars_rv(top_vars)

      # Lump the rest into "Outros"
      d %>%
        mutate(
          Var = ifelse(Var %in% top_vars, Var, "Outros")
        ) %>%
        group_by(Mês, Var) %>%
        summarise(Total = sum(Total), .groups = "drop")
    })

    # Summaries
    monthly_totals <- reactive({
      df_reduced() %>%
        group_by(Mês) %>%
        summarise(MonthTotal = sum(Total, na.rm = TRUE), .groups = "drop")
    })

    df_final <- reactive({
      df_reduced() %>%
        left_join(monthly_totals(), by = "Mês") %>%
        mutate(
          MonthTotal = ifelse(MonthTotal == 0, NA, MonthTotal),
          Percentage = 100 * Total / MonthTotal
        )
    })

    # 6) Render the stacked bar chart
    output$plot <- renderPlotly({
      df <- df_final()
      req(df, nrow(df) > 0)

      var_levels <- df %>%
        group_by(Var) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        pull(Var)

      df <- df %>% mutate(Var = factor(Var, levels = var_levels))

      n <- length(var_levels)
      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (n <= 8) pal8[1:n] else colorRampPalette(pal8)(n)

      p <- plot_ly(source = plotly_source()) %>%
        event_register("plotly_click")
      for (i in seq_along(var_levels)) {
        lvl <- var_levels[i]
        sub_df <- dplyr::filter(df, Var == lvl)
        if (nrow(sub_df) > 0) {
          p <- p %>%
            add_trace(
              data = sub_df,
              x = ~Mês,
              y = ~Total,
              name = lvl,
              type = "bar",
              marker = list(color = pal[i]),
              key = lvl,
              customdata = lapply(seq_len(nrow(sub_df)), function(row_i) {
                list(
                  monthTotal = sub_df$MonthTotal[row_i],
                  percentage = sub_df$Percentage[row_i]
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
      p %>%
        layout(
          title = list(
            text = chart_title(),
            font = list(size = 16),
            x = 0, # moves title to the left
            xanchor = "left" # anchor title at left
          ),
          barmode = "stack",
          xaxis = list(
            tickformat = "%m-%Y",
            type = "date",
            tickvals = unique(df$Mês)
          ),
          autosize = TRUE
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo    = FALSE
        )
    })

    # 7) Handle clicking any bar segment, including “Outros”
    observeEvent(event_data("plotly_click", source = plotly_source()), {
      click_data <- event_data("plotly_click", source = plotly_source())
      if (is.null(click_data)) {
        return()
      }

      clicked_month <- as.POSIXct(click_data$x, origin = "1970-01-01")
      clicked_var <- click_data$key
      tv <- top_vars_rv()

      # If user clicked “Outros,” retrieve all categories not in tv
      if (!is.null(tv) && clicked_var == "Outros") {
        detail_data <- dados %>%
          mutate(.dt = as.Date(.data[[data]])) %>%
          filter(
            floor_date(.dt, "month") == floor_date(clicked_month, "month"),
            !(.data[[input$variavel]] %in% tv)
          )
      } else {
        # Normal category
        detail_data <- dados %>%
          mutate(.dt = as.Date(.data[[data]])) %>%
          filter(
            floor_date(.dt, "month") == floor_date(clicked_month, "month"),
            .data[[input$variavel]] == clicked_var
          )
      }

      detail_rv(detail_data)

      if (nrow(detail_data) == 0) {
        showModal(modalDialog(
          title = "No details found",
          "No matching data for the selected segment.",
          easyClose = TRUE,
          footer = modalButton("Fechar")
        ))
      } else {
        detail_data <- dplyr::select(detail_data, -.dt)
        showModal(modalDialog(
          title = paste("Detalhes:", clicked_var, "-", format(clicked_month, "%b %Y")),
          div( # Wrap the table in a 100%-width div
            DT::dataTableOutput(ns("detail_table")),
            style = "width: 100%; overflow-x: auto;"
          ),
          size = "l",
          easyClose = TRUE,
          footer = tagList(
            downloadButton(ns("download_detail"), "Baixar XLSX"),
            modalButton("Fechar")
          )
        ))
        output$detail_table <- DT::renderDataTable({
          DT::datatable(
            detail_data,
            filter = "none",
            options = list(
              pageLength = 25,
              scrollX = TRUE,
              autoWidth = TRUE,
              language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"),
              columnDefs = list(
                list(
                  targets = "_all",
                  className = "dt-nowrap"
                )
              ),
              initComplete = htmlwidgets::JS("
                  function(settings, json) {
                    var api = this.api();
                    // For each column, attach a click that creates a multi-select
                    api.columns().every(function() {
                      var column = this;
                      var $header = $(column.header());

                      $header.off('click').on('click', function(e) {
                        e.stopPropagation();
                        if ($header.find('select').length) return;

                        // Store the original column name
                        var colName = $header.text();

                        // Clear header
                        $header.empty();

                        // Add the column name in its own block
                        $header.append(
                          $('<div>')
                            .css({'font-weight': 'bold', 'margin-bottom': '6px'})
                            .text(colName)
                        );

                        // Create multi-select, appended below the column name
                        var $select = $('<select multiple style=\"width:95%\"/>')
                          .appendTo($header)
                          .on('click', function(e) { e.stopPropagation(); })
                          .on('change', function() {
                            var vals = $(this).val() || [];
                            if (vals.length) {
                              var pattern = vals.map($.fn.dataTable.util.escapeRegex).join('|');
                              column.search(pattern, true, false).draw();
                            } else {
                              column.search('', true, false).draw();
                            }
                          });

                        // Populate with unique sorted values
                        column.data().unique().sort().each(function(d) {
                          if(d) $select.append($('<option>').val(d).text(d));
                        });

                        // Wait a moment, then turn it into a Select2 for a nicer UI
                        setTimeout(function() {
                          $select.select2({
                            width: 'auto',
                            dropdownParent: $('body'),
                            placeholder: 'Selecione...',
                            allowClear: true
                          });
                        }, 0);
                      });
                    });
                  }
                ")
            ),
            class = "stripe hover cell-border dt-nowrap" # <-- added "dt-nowrap"
          )
        })
      }
    })

    # 8) Download for the details
    output$download_detail <- downloadHandler(
      filename = function() {
        paste0("detalhes_", format(Sys.Date(), "%Y%m%d"), "_", input$variavel, ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(detail_rv(), path = file)
      }
    )
  })
}

tags$style(HTML("
  .dt-nowrap {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 200px; /* Adjust as desired */
  }
"))

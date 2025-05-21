# Server and UI for Despesas trajectory chart (modularized, concise, and well-documented)

# ----------------------------
#        UI MODULE
# ----------------------------
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
    # Placeholder for checkbox (conditionally shown in the server)
    uiOutput(ns("checkbox_wrapper")),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

# ----------------------------
#       SERVER MODULE
# ----------------------------
g_desp.traj_server <- function(id,
                               dados,
                               filtro_periodo,
                               data_inicial,
                               data_final,
                               max_unicos_i = 20) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Example: store detail data for modal
    detail_rv <- reactiveVal(NULL)

    # 1) Date range logic (unchanged)
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

    # 2) Chart title (unchanged)
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
      sprintf("Despesas empilhadas por %s %s", var_name, period_text)
    })

    # 3) Base data, grouped by Mês and Var
    df_data <- reactive({
      pr <- period()
      req(dados$desp)
      dados$desp %>%
        mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
        filter(.dt >= pr$start, .dt <= pr$end) %>%
        group_by(
          Mês = floor_date(.dt, "month"),
          Var = as.character(.data[[input$variavel]])
        ) %>%
        summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop")
    })

    # 4) Conditionally render the checkbox only if n_distinct() > max_unicos_i
    output$checkbox_wrapper <- renderUI({
      d <- df_data()
      req(d)
      if (n_distinct(d$Var) <= max_unicos_i) {
        return(NULL) # hide checkbox
      }

      # Show checkbox if categories exceed max_unicos_i
      checkboxInput(
        inputId = ns("show_all_cats"),
        label = div(
          style = "white-space: normal; word-wrap: break-word; max-width: 300px;",
          sprintf("Mostrar todas as categorias de '%s'", input$variavel)
        ),
        value = FALSE # default: grouped
      )
    })

    # 5) Possibly group categories when user hasn't checked "show_all_cats"
    df_reduced <- reactive({
      d <- df_data()
      req(d)

      # If categories <= max_unicos_i, or user wants all => do nothing
      if (n_distinct(d$Var) <= max_unicos_i || isTRUE(input$show_all_cats)) {
        return(d)
      }

      # Otherwise, group everything after top (max_unicos_i - 1) as "Outros"
      totals_by_var <- d %>%
        group_by(Var) %>%
        summarise(TotalVar = sum(Total), .groups = "drop") %>%
        arrange(desc(TotalVar))

      top_vars <- totals_by_var$Var[seq_len(max_unicos_i - 1)]
      d %>%
        mutate(Var = ifelse(Var %in% top_vars, Var, "Outros")) %>%
        group_by(Mês, Var) %>%
        summarise(Total = sum(Total), .groups = "drop")
    })

    # 6) Summaries, final table, plot, etc. (unchanged)
    monthly_totals <- reactive({
      df_reduced() %>%
        group_by(Mês) %>%
        summarise(MonthTotal = sum(Total, na.rm = TRUE), .groups = "drop")
    })

    df_final <- reactive({
      df_reduced() %>%
        left_join(monthly_totals(), by = "Mês") %>%
        mutate(
          MonthTotal = ifelse(MonthTotal == 0, NA, as.numeric(MonthTotal)),
          Percentage = (Total / MonthTotal) * 100
        )
    })

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

      p <- plot_ly(source = "despPlot")
      for (i in seq_along(var_levels)) {
        lvl <- var_levels[i]
        sub_df <- dplyr::filter(df, Var == lvl)

        if (nrow(sub_df) > 0) {
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
      p %>%
        layout(
          title = list(text = chart_title(), font = list(size = 16)),
          barmode = "stack",
          xaxis = list(
            tickformat = "%m-%Y",
            type = "date",
            tickvals = unique(df$Mês),
            rangeslider = list(visible = (filtro_periodo() == "desde_inicio"))
          ),
          autosize = TRUE
        ) %>%
        event_register("plotly_click") %>%
        config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo    = FALSE
        ) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var btn = el.querySelector('.modebar-btn[data-title=\"Download plot as a png\"]');
            if (btn) {
              btn.setAttribute('data-title', 'Baixar gráfico como PNG');
            }
          }
        ")
    })

    # 7) Observer for plotly clicks, detail modals, downloadHandler, etc. (unchanged)
    observeEvent(event_data("plotly_click", source = "despPlot"), {
      click_data <- event_data("plotly_click", source = "despPlot")
      if (!is.null(click_data)) {
        clicked_month <- as.POSIXct(click_data$x, origin = "1970-01-01")
        clicked_var <- click_data$key

        detail_data <- dados$desp %>%
          mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
          filter(
            lubridate::floor_date(.dt, "month") ==
              lubridate::floor_date(clicked_month, "month"),
            as.character(.data[[input$variavel]]) == clicked_var
          )

        detail_rv(detail_data)

        if (nrow(detail_data) == 0) {
          showModal(modalDialog(
            title = "No details found",
            "No matching data for the selected segment.",
            easyClose = TRUE,
            footer = modalButton("Fechar")
          ))
        } else {
          detail_data <- detail_data %>% select(-.dt)
          showModal(modalDialog(
            title = paste("Detalhes:", clicked_var, "-", format(clicked_month, "%b %Y")),
            div(
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
                pageLength = 10,
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
                    api.columns().every(function() {
                      var column = this;
                      var $header = $(column.header());

                      $header.off('click').on('click', function(e) {
                        e.stopPropagation();
                        if ($header.find('select').length) return;

                        var colName = $header.text();

                        $header.empty();

                        $header.append(
                          $('<div>')
                            .css({'font-weight': 'bold', 'margin-bottom': '6px'})
                            .text(colName)
                        );

                        var $select = $('<select multiple style=\"width:95%\" />')
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

                        column.data().unique().sort().each(function(d) {
                          if(d) $select.append($('<option>').val(d).text(d));
                        });

                        setTimeout(function () {
                          $select.select2({
                            dropdownParent: $('body'),
                            width: 'auto',
                            placeholder: 'Selecione...',
                            allowClear: true
                          });
                        }, 0);
                      });
                    });
                  }
                ")
              ),
              class = "stripe hover cell-border"
            )
          })
        }
      }
    })

    output$download_detail <- downloadHandler(
      filename = function() {
        paste0(
          "detalhes_",
          format(Sys.Date(), "%Y%m%d"), "_",
          input$variavel, ".xlsx"
        )
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
    max-width: 200px;
  }
"))

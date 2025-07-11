# =============================================================================
# MÓDULO: g_barras.empilhadas.mes
# Gráfico de barras empilhadas por mês para análise de despesas e receitas
# =============================================================================

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import lubridate
#' @importFrom RColorBrewer brewer.pal

# ----------------------------
#        UI MODULE
# ----------------------------
g_barras.empilhadas.mes_ui <- function(
    id,
    choices,
    total = "total.pago",
    data = "data.doc.pagto",
    comeco.titulo = "Despesas") {
  ns <- NS(id)
  tagList(
    # Let user pick stacking variable
    selectInput(
      inputId = ns("variavel"),
      label = "Empilhar barras por:",
      choices = choices,
      selected = names(choices)[1]
    ),
    # Placeholder for the checkbox (conditionally shown in the server)
    uiOutput(ns("checkbox_wrapper")),
    # Static chart with download button
    div(
      style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
      plotlyOutput(
        ns("plot"),
        height = "600px"
      )
    ),
    # Download button
    downloadButton(ns("download_chart"), "Baixar Dados (XLSX)", class = "btn-primary")
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

    # Define source_id for plotly events
    source_id <- paste0(id, "_click")

    # Reactive values for click handling
    detail_rv <- reactiveVal(NULL)
    top_vars_rv <- reactiveVal(NULL)

    # Map each ID to its matching Plotly source string:
    plotly_source <- reactive({
      paste0(id, "_click")
    })

    # 1) Reactive for date range
    period <- reactive({
      req(filtro_periodo())
      today <- Sys.Date()
      switch(filtro_periodo(),
        "ano_corrente" = list(start = as.Date(paste0(format(today, "%Y"), "-01-01")), end = today),
        "ultimos_12" = list(start = as.Date(format(today - 365, "%Y-%m-%d")), end = today),
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
      # Make sure we have all required inputs
      pr <- period()
      req(dados, pr, pr$start, pr$end, input$variavel)

      # Use non-standard evaluation instead of .data pronoun
      dt_var <- data
      total_var <- total
      group_var <- input$variavel

      # Instead of creating a temporary column in a pipe, use direct methods
      # Get dates as Date objects directly
      date_col <- as.Date(dados[[dt_var]])

      # Filter the data directly without using a temporary column
      date_mask <- date_col >= pr$start & date_col <= pr$end
      filtered_data <- dados[date_mask, ]

      # Get the group variable values
      group_values <- as.character(filtered_data[[group_var]])

      # Calculate month dates using base R instead of lubridate
      # Convert to first day of month
      month_dates <- as.Date(paste0(format(date_col[date_mask], "%Y-%m"), "-01"))

      # Create a data frame for aggregation
      agg_data <- data.frame(
        mes = month_dates,
        var = group_values,
        value = filtered_data[[total_var]]
      )

      # Perform the aggregation - use explicit column references
      result <- aggregate(
        agg_data$value,
        by = list(mes = agg_data$mes, var = agg_data$var),
        FUN = sum,
        na.rm = TRUE
      )
      colnames(result)[3] <- "total" # Rename the aggregated column

      result
    })

    # 4) Conditionally show or hide the checkbox
    output$checkbox_wrapper <- renderUI({
      d <- df_data()
      req(d)
      distinct_vars <- length(unique(d$var))
      if (distinct_vars <= max_unicos_i) {
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

    # 5) Possibly group categories into "Outros"
    df_reduced <- reactive({
      d <- df_data()
      req(d)

      # If user wants all or not many categories => do nothing
      distinct_vars <- length(unique(d$var))
      if (distinct_vars <= max_unicos_i || isTRUE(input$show_all_cats)) {
        top_vars_rv(NULL)
        return(d)
      }

      # Identify top (max_unicos_i - 1) categories
      # Use aggregate instead of dplyr
      totals_by_var <- aggregate(
        d$total,
        by = list(var = d$var),
        FUN = sum
      )
      colnames(totals_by_var)[2] <- "totalvar"
      totals_by_var <- totals_by_var[order(totals_by_var$totalvar, decreasing = TRUE), ]

      top_vars <- totals_by_var$var[seq_len(max_unicos_i - 1)]
      top_vars_rv(top_vars)

      # Lump the rest into "Outros"
      # Create new var column with "Outros" for non-top values
      d$var <- ifelse(d$var %in% top_vars, d$var, "Outros")

      # Aggregate again to combine "Outros" categories
      result <- aggregate(
        d$total,
        by = list(mes = d$mes, var = d$var),
        FUN = sum
      )
      colnames(result)[3] <- "total"

      result
    })

    # Summaries
    monthly_totals <- reactive({
      reduced <- df_reduced()
      # Use aggregate instead of dplyr for more reliable behavior
      result <- aggregate(
        reduced$total,
        by = list(mes = reduced$mes),
        FUN = sum,
        na.rm = TRUE
      )
      colnames(result)[2] <- "monthtotal"
      result
    })

    df_final <- reactive({
      reduced <- df_reduced()
      monthlies <- monthly_totals()

      # Manual join and transform without dplyr
      result <- merge(reduced, monthlies, by = "mes", all.x = TRUE)

      # Add percentage calculation
      result$monthtotal <- ifelse(result$monthtotal == 0, NA, result$monthtotal)
      result$percentage <- 100 * result$total / result$monthtotal

      result
    })

    # 6) Render the stacked bar chart
    output$plot <- renderPlotly({
      df <- df_final()
      req(df, nrow(df) > 0)

      # Prepare factor levels and palette
      var_totals <- aggregate(df$total, by = list(var = df$var), FUN = sum)
      colnames(var_totals)[2] <- "total_sum"
      var_levels <- var_totals$var[order(var_totals$total_sum, decreasing = TRUE)]
      df$var <- factor(df$var, levels = var_levels)
      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (length(var_levels) <= 8) pal8[seq_along(var_levels)] else colorRampPalette(pal8)(length(var_levels))

      # Build plotly chart with click event registration
      p <- plot_ly(
        data = df,
        x = ~mes,
        y = ~total,
        color = ~var,
        colors = pal,
        type = "bar",
        source = source_id,
        key = ~var,
        text = ~ paste0(
          "Total do mês: R$ ", formatC(monthtotal, format = "f", big.mark = ",", digits = 2),
          "<br>Percentual: ", round(percentage, 1), "%"
        ),
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "Valor: R$ %{y:,.2f}<br>",
          "%{text}<br>",
          "<extra></extra>"
        )
      ) %>%
        plotly::layout(
          title = list(text = chart_title(), font = list(size = 16), x = 0, xanchor = "left"),
          barmode = "stack",
          xaxis = list(tickformat = "%m-%Y", type = "date", tickvals = unique(df$mes)),
          autosize = TRUE
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo = FALSE
        )

      # Register events and return plot
      tryCatch(
        {
          plotly::event_register(p, "plotly_click")
        },
        error = function(e) {
          # Silently ignore if already registered
        }
      )
      p
    })

    # Handle clicking any bar segment, including "Outros"
    # Use observe to continuously monitor for click events
    observe({
      # Try to get click data - the event should already be registered by the plot
      click_data <- event_data("plotly_click", source = source_id)

      if (!is.null(click_data)) {
        clicked_month <- as.Date(click_data$x, origin = "1970-01-01")
        clicked_var <- click_data$key
        tv <- top_vars_rv()

        if (!is.null(tv) && clicked_var == "Outros") {
          dt_var <- data
          group_var <- input$variavel

          # Direct approach without temporary columns
          date_col <- as.Date(dados[[dt_var]])
          month_match <- floor_date(date_col, "month") == floor_date(clicked_month, "month")
          cat_values <- dados[[group_var]]
          cat_match <- !(cat_values %in% tv)

          # Filter using logical masks
          detail_data <- dados[month_match & cat_match, ]
        } else {
          # Normal category
          dt_var <- data
          group_var <- input$variavel

          # Direct approach without temporary columns
          date_col <- as.Date(dados[[dt_var]])
          month_match <- floor_date(date_col, "month") == floor_date(clicked_month, "month")
          cat_match <- dados[[group_var]] == clicked_var

          # Filter using logical masks
          detail_data <- dados[month_match & cat_match, ]
        }

        detail_rv(detail_data)

        if (nrow(detail_data) == 0) {
          showModal(modalDialog(
            title = "Sem detalhes",
            "Não há dados disponíveis para o segmento selecionado.",
            easyClose = TRUE,
            footer = modalButton("Fechar")
          ))
        } else {
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
                pageLength = 25,
                scrollX = TRUE,
                autoWidth = TRUE,
                language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"),
                columnDefs = list(
                  list(
                    targets = "_all",
                    className = "dt-nowrap"
                  )
                )
              ),
              class = "stripe hover cell-border dt-nowrap"
            )
          })
        }
      }
    })

    # Download for the details
    output$download_detail <- downloadHandler(
      filename = function() {
        paste0("detalhes_", format(Sys.Date(), "%Y%m%d"), "_", input$variavel, ".xlsx")
      },
      content = function(file) {
        data_to_write <- detail_rv()
        req(data_to_write)
        writexl::write_xlsx(data_to_write, path = file)
      }
    )

    # Download for the main chart data
    output$download_chart <- downloadHandler(
      filename = function() {
        paste0(id, "_dados_", format(Sys.Date(), "%Y%m%d"), "_", input$variavel, ".xlsx")
      },
      content = function(file) {
        # Get the final data used for the chart
        data_to_write <- df_final()
        req(data_to_write)

        # Format the date for better readability in Excel
        data_export <- data.frame(
          mes = format(data_to_write$mes, "%Y-%m-%d"),
          variavel = data_to_write$var,
          valor = data_to_write$total,
          total_mes = data_to_write$monthtotal,
          percentual = data_to_write$percentage
        )

        writexl::write_xlsx(data_export, path = file)
      }
    )
  })
}

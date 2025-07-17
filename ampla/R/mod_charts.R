# -*- coding: utf-8 -*-
# =============================================================================
# MODULO: g_barras.empilhadas.mes
# Grafico de barras empilhadas por mes para analise de despesas e receitas
# =============================================================================

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import lubridate
#' @importFrom DT datatable renderDataTable dataTableOutput
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
    # Professional clean CSS design
    tags$style(HTML("
      /* Clean, professional design without amateur shadows and borders */
      .main-content {
        background-color: #fafafa;
        min-height: 100vh;
      }

      /* Fixed tabs at top */
      .nav-tabs {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 1000 !important;
        background-color: white !important;
        border-bottom: 1px solid #e5e5e5 !important;
        margin: 0 !important;
        padding: 0 20px !important;
        height: 48px !important;
      }

      .nav-tabs .nav-link {
        border: none !important;
        color: #666 !important;
        font-weight: 500 !important;
        padding: 12px 20px !important;
      }

      .nav-tabs .nav-link.active {
        background-color: white !important;
        color: #333 !important;
        border-bottom: 2px solid #007bff !important;
      }

      /* Fixed parameters section */
      .parameters-section {
        background-color: white;
        padding: 20px;
        margin: 0;
        border-bottom: 1px solid #e5e5e5;
        position: fixed !important;
        top: 48px !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 999 !important;
      }

      /* Fixed title with proper padding and readability - NO SHADOW */
      .chart-title {
        background-color: white;
        padding: 15px 20px;
        margin: 0;
        border-bottom: 1px solid #e5e5e5;
        font-size: 18px;
        font-weight: 600;
        color: #333;
        position: fixed !important;
        top: calc(48px + var(--params-height, 140px)) !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 998 !important;
        /* Removed box-shadow */
      }

      /* Add top margin to main content to account for fixed elements */
      .main-content-wrapper {
        margin-top: calc(48px + var(--params-height, 140px) + 58px);
        background-color: #fafafa;
      }

      /* Chart containers without ugly borders */
      .chart-container {
        background-color: white;
        margin: 0;
        padding: 20px;
        border-bottom: 1px solid #f0f0f0;
      }

      /* Remove all amateur shadows and borders */
      .form-control, .form-select {
        border: 1px solid #ddd !important;
        box-shadow: none !important;
      }

      .form-control:focus, .form-select:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 2px rgba(0,123,255,0.25) !important;
      }

      /* Clean content area */
      .tab-content {
        background-color: white;
        margin: 0;
        padding: 0;
      }

      .tab-pane {
        margin: 0;
        padding: 0;
        background-color: white;
      }
    ")),

    # Simple JavaScript to update CSS custom property for params height
    tags$script(HTML("
      $(document).ready(function() {
        function updateParamsHeight() {
          var paramsHeight = $('.parameters-section').outerHeight() || 140;
          document.documentElement.style.setProperty('--params-height', paramsHeight + 'px');
        }

        // Update on load and resize
        updateParamsHeight();
        $(window).on('resize', updateParamsHeight);

        // Update after potential content changes
        setTimeout(updateParamsHeight, 1000);
      });
    ")),

    # Professional wrapper
    div(
      class = "main-content",

      # Clean tab navigation
      tabsetPanel(
        id = ns("chart_tabs"),
        type = "tabs",

        # Charts tab
        tabPanel(
          title = "Graficos",
          value = "graficos",

          # Clean parameters section
          div(
            class = "parameters-section",

            # Main parameters title
            h4("Parametros", style = "margin: 0 0 20px 0; font-weight: 600; color: #333; font-size: 20px; border-bottom: 1px solid #e5e5e5; padding-bottom: 10px;"),

            # Period filter
            div(
              style = "margin-bottom: 20px;",
              h5("Periodo", style = "margin: 0 0 10px 0; font-weight: 600; color: #333;"),
              filtro_periodo_module_ui(ns("filtro"))
            ),

            # Company filter
            div(
              style = "margin-bottom: 20px;",
              h5("Empresa(s)", style = "margin: 0 0 10px 0; font-weight: 600; color: #333;"),
              uiOutput(ns("empresa_selector"))
            ),

            # Variable selection - will be dynamically updated based on company selection
            div(
              style = "margin-bottom: 20px;",
              h5("Empilhar barras por", style = "margin: 0 0 10px 0; font-weight: 600; color: #333;"),
              uiOutput(ns("variavel_selector"))
            ),

            # Checkbox wrapper
            uiOutput(ns("checkbox_wrapper"))
          ),

          # Chart title - Fixed approach with proper rendering
          div(
            class = "chart-title",
            h4(textOutput(ns("charts_title")), style = "margin: 0; font-size: 18px; font-weight: 600; color: #333;")
          ),

          # Charts content wrapper with proper spacing
          div(
            class = "main-content-wrapper",

            # Charts content - clean design
            div(
              class = "chart-container",
              plotlyOutput(ns("plot"), height = "500px")
            ),
            div(
              class = "chart-container",
              plotlyOutput(ns("line_plot"), height = "500px")
            ),
            div(
              class = "chart-container",
              plotlyOutput(ns("percent_plot"), height = "500px")
            )
          )
        ),

        # Statistics tab
        tabPanel(
          title = "Estatisticas",
          value = "estatisticas",
          div(
            class = "chart-container",
            style = "text-align: center;",
            h4("Estatisticas", style = "color: #333; margin-bottom: 10px;"),
            p("Em desenvolvimento...", style = "color: #666;")
          )
        ),

        # Data tab
        tabPanel(
          title = "Dados",
          value = "dados",
          div(
            class = "chart-container",
            DT::dataTableOutput(ns("data_table"))
          )
        ),

        # Metadata tab
        tabPanel(
          title = "Metadados",
          value = "metadados",
          div(
            class = "chart-container",
            style = "text-align: center;",
            h4("Metadados", style = "color: #333; margin-bottom: 10px;"),
            p("Em desenvolvimento...", style = "color: #666;")
          )
        )
      )
    )
  )
}

# ----------------------------
#       SERVER MODULE
# ----------------------------
g_barras.empilhadas.mes_server <- function(
    id,
    dados,
    max_unicos_i = 20,
    total = "total.pago",
    data = "data.doc.pagto",
    comeco.titulo = "Despesas") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize the filtro_periodo module
    filtroVals <- filtro_periodo_module_server("filtro")

    # Define source_id for plotly events
    source_id <- paste0(id, "_click") # Reactive values for click handling
    detail_rv <- reactiveVal(NULL)
    top_vars_rv <- reactiveVal(NULL)

    # Map each ID to its matching Plotly source string:
    plotly_source <- reactive({
      paste0(id, "_click")
    })

    # 1) Reactive for date range
    period <- reactive({
      req(filtroVals$filtro_periodo())
      today <- Sys.Date()
      switch(filtroVals$filtro_periodo(),
        "ano_corrente" = list(start = as.Date(paste0(format(today, "%Y"), "-01-01")), end = today),
        "ultimos_12" = list(start = as.Date(format(today - 365, "%Y-%m-%d")), end = today),
        "desde_inicio" = {
          dt <- as.Date(dados[[data]], origin = "1970-01-01")
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(filtroVals$data_inicial(), filtroVals$data_final())
          # Robust date parsing using lubridate as final fallback
          start_date <- tryCatch(
            {
              if (inherits(filtroVals$data_inicial(), "Date")) {
                filtroVals$data_inicial()
              } else {
                as.Date(filtroVals$data_inicial())
              }
            },
            error = function(e) {
              tryCatch(
                {
                  as.Date(filtroVals$data_inicial(), format = "%d/%m/%Y")
                },
                error = function(e2) {
                  tryCatch(
                    {
                      as.Date(filtroVals$data_inicial(), format = "%Y-%m-%d")
                    },
                    error = function(e3) {
                      lubridate::dmy(filtroVals$data_inicial())
                    }
                  )
                }
              )
            }
          )

          end_date <- tryCatch(
            {
              if (inherits(filtroVals$data_final(), "Date")) {
                filtroVals$data_final()
              } else {
                as.Date(filtroVals$data_final())
              }
            },
            error = function(e) {
              tryCatch(
                {
                  as.Date(filtroVals$data_final(), format = "%d/%m/%Y")
                },
                error = function(e2) {
                  tryCatch(
                    {
                      as.Date(filtroVals$data_final(), format = "%Y-%m-%d")
                    },
                    error = function(e3) {
                      lubridate::dmy(filtroVals$data_final())
                    }
                  )
                }
              )
            }
          )

          list(start = start_date, end = end_date)
        }
      )
    })

    # 2) Chart title, using comeco.titulo
    chart_title <- reactive({
      req(input$variavel, filtroVals$filtro_periodo())
      var_name <- paste0("'", input$variavel, "'")
      period_text <- switch(filtroVals$filtro_periodo(),
        "ano_corrente" = "no ano corrente",
        "ultimos_12" = "nos ultimos 12 meses",
        "desde_inicio" = "desde o inicio",
        "personalizado" = {
          req(filtroVals$data_inicial(), filtroVals$data_final())
          sprintf(
            "de %s ate %s",
            format(filtroVals$data_inicial(), "%d/%m/%Y"),
            format(filtroVals$data_final(), "%d/%m/%Y")
          )
        }
      )

      # Add empresa info if specific empresa is selected
      empresa_text <- ""
      if (!is.null(input$empresa) && input$empresa != "todas") {
        empresa_text <- paste0(" - ", input$empresa)
      }

      # Combine the static prefix with the variable, date info, and empresa
      sprintf("%s %s %s%s", comeco.titulo, var_name, period_text, empresa_text)
    })

    # Output for the single charts title
    output$charts_title <- renderText({
      chart_title()
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

      # Apply empresa filter if selected
      if (!is.null(input$empresa) && input$empresa != "todas" && "empresa" %in% names(filtered_data)) {
        empresa_mask <- filtered_data$empresa == input$empresa
        filtered_data <- filtered_data[empresa_mask & !is.na(empresa_mask), ]
      }

      # Get the group variable values
      group_values <- as.character(filtered_data[[group_var]])

      # Calculate month dates using base R instead of lubridate
      # Convert to first day of month
      month_dates <- as.Date(paste0(format(as.Date(filtered_data[[dt_var]]), "%Y-%m"), "-01"))

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
          style = "white-space: normal; word-wrap: break-word; max-width: 300px; font-weight: bold;",
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

      # Prepare factor levels and palette (sorted by total value)
      # Calculate totals by variable and sort by value
      totals_by_var <- aggregate(df$total, by = list(var = df$var), FUN = sum)
      colnames(totals_by_var)[2] <- "total_var"
      totals_by_var <- totals_by_var[order(totals_by_var$total_var, decreasing = TRUE), ]
      var_levels <- totals_by_var$var
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
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "Mes: %{x|%m-%Y}<br>",
          "Total da categoria: R$ %{y:,.2f}<br>",
          "Total mensal: R$ %{text:,.2f}<br>",
          "% Categoria no mes: %{customdata:.1f}%<br>",
          "<extra></extra>"
        ),
        text = ~monthtotal,
        textposition = "none",
        customdata = ~percentage
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = {
            # Add range slider for "desde_inicio" but keep charts fixed
            show_rangeslider <- !is.null(filtroVals$filtro_periodo()) && filtroVals$filtro_periodo() == "desde_inicio"

            xaxis_config <- list(
              title = "Mes",
              tickformat = "%m-%Y",
              type = "date",
              tickvals = unique(df$mes),
              fixedrange = TRUE # Always disable zoom/pan
            )

            if (show_rangeslider) {
              xaxis_config$rangeslider <- list(visible = TRUE)
            }

            xaxis_config
          },
          yaxis = list(title = "Valor (R$)", fixedrange = TRUE),
          autosize = TRUE
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo = FALSE
        )

      p
    })

    # Line chart output - grouped by stacking variable
    output$line_plot <- renderPlotly({
      df <- df_final()
      req(df, input$variavel)

      # Aggregate data by month and stacking variable
      line_data <- df %>%
        group_by(mes, var) %>%
        summarise(total_grupo = sum(total), .groups = "drop") %>%
        arrange(mes, var)

      # Add monthly totals and percentages for hover info
      line_monthly_totals <- df %>%
        group_by(mes) %>%
        summarise(monthtotal = sum(total), .groups = "drop")

      line_data <- line_data %>%
        left_join(line_monthly_totals, by = "mes") %>%
        mutate(percentage = 100 * total_grupo / monthtotal)

      # Get colors for consistency with bar chart (alphabetical order, 'Outros' last)
      all_vars <- unique(line_data$var)
      outros_vars <- all_vars[all_vars == "Outros"]
      non_outros_vars <- sort(all_vars[all_vars != "Outros"])
      unique_vars <- c(non_outros_vars, outros_vars)
      colors <- RColorBrewer::brewer.pal(min(length(unique_vars), 11), "Spectral")
      if (length(unique_vars) > 11) {
        colors <- rep(colors, length.out = length(unique_vars))
      }

      # Create line chart with multiple series
      p_line <- plot_ly(source = paste0(source_id, "_line"))

      for (i in seq_along(unique_vars)) {
        var_data <- line_data[line_data$var == unique_vars[i], ]

        p_line <- p_line %>%
          add_trace(
            data = var_data,
            x = ~mes,
            y = ~total_grupo,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = colors[i], width = 2),
            marker = list(color = colors[i], size = 6),
            name = unique_vars[i],
            key = unique_vars[i],
            hovertemplate = paste0(
              "<b>%{fullData.name}</b><br>",
              "Mes: %{x|%m-%Y}<br>",
              "Total da categoria: R$ %{y:,.2f}<br>",
              "Total mensal: R$ %{text:,.2f}<br>",
              "% Categoria no mes: %{customdata:.1f}%<br>",
              "<extra></extra>"
            ),
            text = ~monthtotal,
            customdata = ~percentage
          )
      }

      # Configure x-axis with range slider for "desde_inicio" but always fixed
      show_rangeslider <- !is.null(filtroVals$filtro_periodo()) && filtroVals$filtro_periodo() == "desde_inicio"

      xaxis_config <- list(
        title = "Mes",
        tickformat = "%m-%Y",
        type = "date",
        fixedrange = TRUE # Always disable zoom/pan
      )

      if (show_rangeslider) {
        xaxis_config$rangeslider <- list(visible = TRUE)
      }

      p_line <- p_line %>%
        plotly::layout(
          xaxis = xaxis_config,
          yaxis = list(title = "Valor (R$)", fixedrange = TRUE),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = -0.2)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo = FALSE
        )

      p_line
    })

    # 100% bar chart output - shows distribution over selected period
    output$percent_plot <- renderPlotly({
      df <- df_reduced()
      req(df, input$variavel)

      # Aggregate data by stacking variable over the entire selected period
      period_data <- df %>%
        group_by(var) %>%
        summarise(total_periodo = sum(total), .groups = "drop") %>%
        arrange(desc(total_periodo)) %>%
        mutate(
          percentage = 100 * total_periodo / sum(total_periodo),
          cumulative = cumsum(percentage)
        )

      # Calculate total_geral once for all rows
      total_geral_value <- sum(period_data$total_periodo)
      period_data$total_geral <- total_geral_value

      # Set factor levels to maintain descending order by total (sorted by totals)
      period_data$var <- factor(period_data$var, levels = period_data$var)

      # Get colors for consistency with other charts (by total order)
      unique_vars <- levels(period_data$var)
      colors <- RColorBrewer::brewer.pal(min(length(unique_vars), 11), "Spectral")
      if (length(unique_vars) > 11) {
        colors <- rep(colors, length.out = length(unique_vars))
      }

      # Create 100% stacked bar chart (horizontal single bar)
      p_percent <- plot_ly(
        data = period_data,
        x = ~percentage,
        y = rep("Distribuicao", nrow(period_data)), # Single row
        color = ~var,
        colors = colors,
        type = "bar",
        orientation = "h",
        source = paste0(source_id, "_percent"),
        key = ~var,
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "Valor da categoria: R$ %{customdata:,.2f}<br>",
          "Valor total do periodo: R$ ", format(total_geral_value, big.mark = ",", decimal.mark = ".", nsmall = 2), "<br>",
          "Percentual da categoria: %{x:.1f}%<br>",
          "<extra></extra>"
        ),
        customdata = ~total_periodo
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title = "Percentual (%)",
            range = c(0, 100),
            ticksuffix = "%",
            fixedrange = TRUE # Always disable zoom/pan for 100% chart
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE,
            fixedrange = TRUE # Always disable zoom/pan for 100% chart
          ),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = -0.2),
          margin = list(l = 50, r = 50, t = 30, b = 100)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo = FALSE
        )

      p_percent
    })

    # Handle clicking any bar segment, including "Outros"
    # Simplified approach that relies on proper reactive dependencies
    observe({
      # Only proceed if we have the basic requirements for plots to exist
      req(input$variavel, df_data(), nrow(df_data()) > 0)

      click_data <- NULL
      source_type <- NULL

      # Try to get click data from each source, with proper error handling
      # Bar chart
      tryCatch(
        {
          click_data_bars <- event_data("plotly_click", source = source_id)
          if (!is.null(click_data_bars)) {
            click_data <- click_data_bars
            source_type <- "bars"
          }
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # Line chart (only if no bar click found)
      if (is.null(click_data)) {
        tryCatch(
          {
            click_data_line <- event_data("plotly_click", source = paste0(source_id, "_line"))
            if (!is.null(click_data_line)) {
              click_data <- click_data_line
              source_type <- "line"
            }
          },
          error = function(e) NULL,
          warning = function(w) NULL
        )
      }

      # Percent chart (only if no other clicks found)
      if (is.null(click_data)) {
        tryCatch(
          {
            click_data_percent <- event_data("plotly_click", source = paste0(source_id, "_percent"))
            if (!is.null(click_data_percent)) {
              click_data <- click_data_percent
              source_type <- "percent"
            }
          },
          error = function(e) NULL,
          warning = function(w) NULL
        )
      }

      if (!is.null(click_data) && !is.null(source_type)) {
        clicked_var <- click_data$key
        tv <- top_vars_rv()

        # For 100% chart, we need to show all data for that category (no specific month)
        if (source_type == "percent") {
          if (!is.null(tv) && clicked_var == "Outros") {
            dt_var <- data
            group_var <- input$variavel

            # Filter by period and exclude top variables
            pr <- period()
            date_col <- as.Date(dados[[dt_var]])
            date_mask <- date_col >= pr$start & date_col <= pr$end
            cat_values <- dados[[group_var]]
            cat_match <- !(cat_values %in% tv)

            # Filter using logical masks
            detail_data <- dados[date_mask & cat_match, ]
          } else {
            # Normal category - show all data for this category in the period
            dt_var <- data
            group_var <- input$variavel

            pr <- period()
            date_col <- as.Date(dados[[dt_var]])
            date_mask <- date_col >= pr$start & date_col <= pr$end
            cat_match <- dados[[group_var]] == clicked_var

            # Filter using logical masks
            detail_data <- dados[date_mask & cat_match, ]
          }

          modal_title <- paste("Detalhes:", clicked_var, "- Periodo completo")
        } else {
          # For bars and line charts, filter by specific month
          clicked_month <- as.Date(click_data$x, origin = "1970-01-01")

          if (!is.null(tv) && clicked_var == "Outros") {
            dt_var <- data
            group_var <- input$variavel

            # Direct approach without temporary columns
            date_col <- as.Date(dados[[dt_var]])
            # Convert dates to first day of month for comparison
            month_match <- as.Date(paste0(format(date_col, "%Y-%m"), "-01")) == as.Date(paste0(format(clicked_month, "%Y-%m"), "-01"))
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
            # Convert dates to first day of month for comparison
            month_match <- as.Date(paste0(format(date_col, "%Y-%m"), "-01")) == as.Date(paste0(format(clicked_month, "%Y-%m"), "-01"))
            cat_match <- dados[[group_var]] == clicked_var

            # Filter using logical masks
            detail_data <- dados[month_match & cat_match, ]
          }

          modal_title <- paste("Detalhes:", clicked_var, "-", format(clicked_month, "%b %Y"))
        }

        detail_rv(detail_data)

        if (nrow(detail_data) == 0) {
          showModal(modalDialog(
            title = "Sem detalhes",
            "Nao ha dados disponiveis para o segmento selecionado.",
            easyClose = TRUE,
            footer = modalButton("Fechar")
          ))
        } else {
          showModal(modalDialog(
            title = modal_title,
            div(
              DT::dataTableOutput(ns("detail_table")),
              style = "width: 100%; overflow-x: auto;"
            ),
            size = "l",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Fechar")
            )
          ))

          output$detail_table <- DT::renderDataTable({
            # Create a copy with Portuguese column names
            detail_formatted <- detail_data

            # Translate column names to Portuguese
            col_names <- names(detail_formatted)
            name_mapping <- c(
              "data.doc.pagto" = "Data Pagamento",
              "data.pagamento" = "Data Pagamento",
              "total.pago" = "Valor Pago",
              "total" = "Valor Total",
              "descricao" = "Descricao",
              "fornecedor" = "Fornecedor",
              "centro.custo" = "Centro de Custo",
              "conta.contabil" = "Conta Contabil",
              "natureza" = "Natureza",
              "tipo.documento" = "Tipo Documento",
              "numero.documento" = "Numero Documento",
              "observacoes" = "Observacoes",
              "banco" = "Banco",
              "agencia" = "Agencia",
              "conta" = "Conta",
              "empreendimento" = "Empreendimento",
              "classificacao" = "Classificacao"
            )

            # Apply the mapping
            for (i in seq_along(col_names)) {
              if (col_names[i] %in% names(name_mapping)) {
                col_names[i] <- name_mapping[col_names[i]]
              }
            }
            names(detail_formatted) <- col_names

            DT::datatable(
              detail_formatted,
              filter = "none",
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                autoWidth = TRUE,
                dom = "Bfrtip",
                buttons = list(
                  list(extend = "copy", text = "Copiar"),
                  list(extend = "csv", text = "Baixar CSV"),
                  list(extend = "excel", text = "Baixar Excel")
                ),
                language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"),
                columnDefs = list(
                  list(
                    targets = "_all",
                    className = "dt-nowrap"
                  )
                )
              ),
              extensions = c("Buttons"),
              class = "stripe hover cell-border dt-nowrap",
              rownames = FALSE
            )
          })
        }
      }
    })

    # Data table output for the "Dados" tab - shows filtered data
    output$data_table <- DT::renderDataTable({
      # Use the filtered data respecting both period and empresa filters
      req(dados)

      # Apply the same filters as in df_data
      pr <- period()
      req(pr, pr$start, pr$end)

      # Filter by date
      dt_var <- data
      date_col <- as.Date(dados[[dt_var]])
      date_mask <- date_col >= pr$start & date_col <= pr$end
      filtered_data <- dados[date_mask, ]

      # Apply empresa filter if selected
      if (!is.null(input$empresa) && input$empresa != "todas" && "empresa" %in% names(filtered_data)) {
        empresa_mask <- filtered_data$empresa == input$empresa
        filtered_data <- filtered_data[empresa_mask & !is.na(empresa_mask), ]
      }

      # Create a copy with Portuguese column names
      data_formatted <- filtered_data

      # Translate column names to Portuguese for common financial columns
      col_names <- names(data_formatted)

      # Create a mapping of English to Portuguese column names
      name_mapping <- c(
        "data.doc.pagto" = "Data Pagamento",
        "data.pagamento" = "Data Pagamento",
        "total.pago" = "Valor Pago",
        "total" = "Valor Total",
        "descricao" = "Descricao",
        "fornecedor" = "Fornecedor",
        "centro.custo" = "Centro de Custo",
        "conta.contabil" = "Conta Contabil",
        "natureza" = "Natureza",
        "tipo.documento" = "Tipo Documento",
        "numero.documento" = "Numero Documento",
        "observacoes" = "Observacoes",
        "banco" = "Banco",
        "agencia" = "Agencia",
        "conta" = "Conta",
        "empreendimento" = "Empreendimento",
        "classificacao" = "Classificacao",
        "empresa" = "Empresa"
      )

      # Apply the mapping
      for (i in seq_along(col_names)) {
        if (col_names[i] %in% names(name_mapping)) {
          col_names[i] <- name_mapping[col_names[i]]
        }
      }

      names(data_formatted) <- col_names

      DT::datatable(
        data_formatted,
        filter = "none",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = "Copiar"),
            list(extend = "csv", text = "Baixar CSV"),
            list(extend = "excel", text = "Baixar Excel")
          ),
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"
          ),
          columnDefs = list(
            list(
              targets = "_all",
              className = "dt-nowrap"
            )
          )
        ),
        extensions = c("Buttons"),
        class = "stripe hover cell-border dt-nowrap",
        rownames = FALSE
      )
    })

    # Dynamic UI outputs for empresa and variavel selectors
    output$empresa_selector <- renderUI({
      req(dados)

      # Get unique empresa values (assuming there's an "empresa" column)
      if ("empresa" %in% names(dados)) {
        empresas <- sort(unique(dados$empresa))
        empresas <- empresas[!is.na(empresas)]
        choices <- c("Todas" = "todas", setNames(empresas, empresas))
      } else {
        choices <- c("Todas" = "todas")
      }

      selectInput(
        ns("empresa"),
        label = NULL,
        choices = choices,
        selected = "todas",
        width = "100%"
      )
    })

    # Initial variavel selector (will be updated by observeEvent when empresa changes)
    output$variavel_selector <- renderUI({
      req(dados)

      # Base variable choices with proper Portuguese names (ASCII-safe)
      base_choices <- c(
        "Empresa" = "empresa",
        "Centro" = "centro.custo",
        "Credor" = "fornecedor",
        "Agente Financeiro" = "banco",
        "Empreendimento" = "empreendimento",
        "Classificacao" = "classificacao"
      )

      # Filter choices based on what exists in the data
      available_choices <- base_choices[base_choices %in% names(dados)]

      # Initial default: "Empresa" if available (since default empresa is "todas")
      default_selection <- if ("empresa" %in% available_choices) {
        "empresa"
      } else if ("centro.custo" %in% available_choices) {
        "centro.custo"
      } else if (length(available_choices) > 0) {
        available_choices[1]
      } else {
        NULL
      }

      selectInput(
        ns("variavel"),
        label = NULL,
        choices = available_choices,
        selected = default_selection,
        width = "100%"
      )
    })

    # Observer to handle empresa changes and update variavel selection
    observeEvent(input$empresa,
      {
        # When empresa selection changes, update the variavel choices dynamically
        req(dados, input$empresa)

        # Debug information (remove in production)
        cat("Empresa changed to:", input$empresa, "\n")

        # Base variable choices with proper Portuguese names (ASCII-safe)
        base_choices <- c(
          "Empresa" = "empresa",
          "Centro" = "centro.custo",
          "Credor" = "fornecedor",
          "Agente Financeiro" = "banco",
          "Empreendimento" = "empreendimento",
          "Classificacao" = "classificacao"
        )

        # Filter choices based on what exists in the data
        available_choices <- base_choices[base_choices %in% names(dados)]

        # Core logic: If a specific empresa is selected, remove "Empresa" from choices
        if (input$empresa != "todas") {
          available_choices <- available_choices[available_choices != "empresa"]
        }

        # Default selection logic:
        # - "Todas" empresas -> Default to "Empresa" (if available)
        # - Specific empresa -> Default to "Centro" (exclude "Empresa")
        default_selection <- if (input$empresa == "todas") {
          # When "Todas" is selected, prefer "Empresa" if it exists in data and choices
          if ("empresa" %in% available_choices) {
            "empresa"
          } else if ("centro.custo" %in% available_choices) {
            "centro.custo"
          } else if (length(available_choices) > 0) {
            available_choices[1]
          } else {
            NULL
          }
        } else {
          # When specific empresa is selected, prefer "Centro" (empresa is excluded from choices)
          if ("centro.custo" %in% available_choices) {
            "centro.custo"
          } else if ("fornecedor" %in% available_choices) {
            "fornecedor"
          } else if (length(available_choices) > 0) {
            available_choices[1]
          } else {
            NULL
          }
        }

        cat("Available choices:", paste(names(available_choices), collapse = ", "), "\n")
        cat("Default selection:", default_selection, "\n")

        # Use updateSelectInput for more reliable updates
        updateSelectInput(
          session = session,
          inputId = "variavel",
          choices = available_choices,
          selected = default_selection
        )
      },
      ignoreInit = TRUE
    )

    # Download for the original data - now handled by DT buttons
    # output$download_chart <- downloadHandler(
    #   filename = function() {
    #     paste0(id, "_dados_originais_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    #   },
    #   content = function(file) {
    #     # Use the original data instead of processed chart data
    #     req(dados)
    #     writexl::write_xlsx(dados, path = file)
    #   }
    # )
  })
}

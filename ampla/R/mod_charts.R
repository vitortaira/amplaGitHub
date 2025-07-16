# =============================================================================
# MÓDULO: g_barras.empilhadas.mes
# Gráfico de barras empilhadas por mês para análise de despesas e receitas
# =============================================================================

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import lubridate
#' @import DT
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
    # Add CSS for proper sticky behavior
    tags$style(HTML("
      body {
        overflow-y: auto !important;
      }
      .input-controls-container {
        background-color: #f9f9f9 !important;
        backdrop-filter: blur(5px);
      }
      .tab-content {
        overflow: visible !important;
      }
    ")),

    # All inputs grouped in one box - with sticky positioning
    div(
      class = "input-controls-container",
      style = "border: 1px solid #e0e0e0; border-radius: 4px; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; position: sticky; top: 0; z-index: 1000; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",

      # Main title for the parameters section
      h3("Parâmetros", style = "margin-top: 0; margin-bottom: 15px; font-weight: bold;"),

      # Period filter section
      div(
        h5("Período:", style = "margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
        filtro_periodo_module_ui(ns("filtro")),
        style = "margin-bottom: 15px;"
      ),

      # Variable selection
      div(
        h5("Empilhar barras por:", style = "margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
        selectInput(
          inputId = ns("variavel"),
          label = NULL,
          choices = choices,
          selected = names(choices)[1]
        ),
        style = "margin-bottom: 10px;"
      ),

      # Checkbox wrapper
      uiOutput(ns("checkbox_wrapper"))
    ),

    # Tab navigation for different views
    tabsetPanel(
      id = ns("chart_tabs"),
      type = "tabs",

      # Charts tab
      tabPanel(
        title = "Gráficos",
        value = "graficos",
        div(
          style = "margin-top: 15px;",

          # Single title for all charts - with sticky positioning
          div(
            style = "margin-bottom: 20px; text-align: left; position: sticky; top: 200px; z-index: 999; background-color: white; padding: 10px 0; border-bottom: 1px solid #e0e0e0;",
            h4(textOutput(ns("charts_title")), style = "margin: 0; font-size: 16px; font-weight: bold;")
          ),

          # Stacked bar chart
          div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-bottom: 20px;",
            plotlyOutput(
              ns("plot"),
              height = "500px"
            )
          ),

          # Line chart
          div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-bottom: 20px;",
            plotlyOutput(
              ns("line_plot"),
              height = "500px"
            )
          ),

          # 100% bar chart
          div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px;",
            plotlyOutput(
              ns("percent_plot"),
              height = "500px"
            )
          )
        )
      ),

      # Statistics tab (placeholder)
      tabPanel(
        title = "Estatísticas",
        value = "estatisticas",
        div(
          style = "padding: 20px; text-align: center;",
          h4("Estatísticas"),
          p("Em desenvolvimento...")
        )
      ),

      # Data tab
      tabPanel(
        title = "Dados",
        value = "dados",
        div(
          style = "padding: 15px;",
          # Data table with built-in download buttons
          DT::dataTableOutput(ns("data_table"))
        )
      ),

      # Metadata tab (placeholder)
      tabPanel(
        title = "Metadados",
        value = "metadados",
        div(
          style = "padding: 20px; text-align: center;",
          h4("Metadados"),
          p("Em desenvolvimento...")
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
        "ultimos_12" = "nos últimos 12 meses",
        "desde_inicio" = "desde o início",
        "personalizado" = {
          req(filtroVals$data_inicial(), filtroVals$data_final())
          sprintf(
            "de %s até %s",
            format(filtroVals$data_inicial(), "%d/%m/%Y"),
            format(filtroVals$data_final(), "%d/%m/%Y")
          )
        }
      )
      # Combine the static prefix with the variable and date info
      sprintf("%s %s %s", comeco.titulo, var_name, period_text)
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

      # Prepare factor levels and palette (alphabetical order)
      var_levels <- sort(unique(df$var))
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
          "Mês: %{x|%m-%Y}<br>",
          "Total da categoria: R$ %{y:,.2f}<br>",
          "Total mensal: R$ %{text:,.2f}<br>",
          "% Categoria no mês: %{customdata:.1f}%<br>",
          "<extra></extra>"
        ),
        text = ~monthtotal,
        textposition = "none",
        customdata = ~percentage
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = {
            # Check if we should add range slider
            show_rangeslider <- !is.null(filtroVals$filtro_periodo()) && filtroVals$filtro_periodo() == "desde_inicio"

            xaxis_config <- list(
              title = "Mês",
              tickformat = "%m-%Y",
              type = "date",
              tickvals = unique(df$mes)
            )

            if (show_rangeslider) {
              xaxis_config$rangeslider <- list(visible = TRUE)
              xaxis_config$rangeselector <- list(
                buttons = list(
                  list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                  list(count = 1, label = "1a", step = "year", stepmode = "backward"),
                  list(count = 2, label = "2a", step = "year", stepmode = "backward"),
                  list(step = "all")
                )
              )
            }

            xaxis_config
          },
          yaxis = list(title = "Valor (R$)"),
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
              "Mês: %{x|%m-%Y}<br>",
              "Total da categoria: R$ %{y:,.2f}<br>",
              "Total mensal: R$ %{text:,.2f}<br>",
              "% Categoria no mês: %{customdata:.1f}%<br>",
              "<extra></extra>"
            ),
            text = ~monthtotal,
            customdata = ~percentage
          )
      }

      # Check if we should add range slider (when "desde o inicio" is selected)
      show_rangeslider <- !is.null(filtroVals$filtro_periodo()) && filtroVals$filtro_periodo() == "desde_inicio"

      # Configure x-axis based on period selection
      xaxis_config <- list(
        title = "Mês",
        tickformat = "%m-%Y",
        type = "date"
      )

      if (show_rangeslider) {
        xaxis_config$rangeslider <- list(visible = TRUE)
        xaxis_config$rangeselector <- list(
          buttons = list(
            list(count = 6, label = "6m", step = "month", stepmode = "backward"),
            list(count = 1, label = "1a", step = "year", stepmode = "backward"),
            list(count = 2, label = "2a", step = "year", stepmode = "backward"),
            list(step = "all")
          )
        )
      }

      p_line <- p_line %>%
        plotly::layout(
          xaxis = xaxis_config,
          yaxis = list(title = "Valor (R$)"),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0, y = -0.2)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtons = list(list("toImage")),
          displaylogo = FALSE
        )

      # Register events for line chart
      tryCatch(
        {
          plotly::event_register(p_line, "plotly_click")
        },
        error = function(e) {
          # Silently ignore if already registered
        }
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
        y = rep("Distribuição", nrow(period_data)), # Single row
        color = ~var,
        colors = colors,
        type = "bar",
        orientation = "h",
        source = paste0(source_id, "_percent"),
        key = ~var,
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "Valor da categoria: R$ %{customdata:,.2f}<br>",
          "Valor total do período: R$ ", format(total_geral_value, big.mark = ",", decimal.mark = ".", nsmall = 2), "<br>",
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
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE
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

      # Register events for 100% chart
      tryCatch(
        {
          plotly::event_register(p_percent, "plotly_click")
        },
        error = function(e) {
          # Silently ignore if already registered
        }
      )

      p_percent
    })

    # Handle clicking any bar segment, including "Outros"
    # Use observe to continuously monitor for click events from all three charts
    observe({
      # Try to get click data from all three charts
      click_data_bars <- event_data("plotly_click", source = source_id)
      click_data_line <- event_data("plotly_click", source = paste0(source_id, "_line"))
      click_data_percent <- event_data("plotly_click", source = paste0(source_id, "_percent"))

      # Use whichever chart was clicked
      click_data <- NULL
      if (!is.null(click_data_bars)) {
        click_data <- click_data_bars
      } else if (!is.null(click_data_line)) {
        click_data <- click_data_line
      } else if (!is.null(click_data_percent)) {
        click_data <- click_data_percent
      }

      if (!is.null(click_data)) {
        clicked_var <- click_data$key
        tv <- top_vars_rv()

        # For 100% chart, we need to show all data for that category (no specific month)
        if (!is.null(click_data_percent)) {
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

          modal_title <- paste("Detalhes:", clicked_var, "- Período completo")
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
            "Não há dados disponíveis para o segmento selecionado.",
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
              "descricao" = "Descrição",
              "fornecedor" = "Fornecedor",
              "centro.custo" = "Centro de Custo",
              "conta.contabil" = "Conta Contábil",
              "natureza" = "Natureza",
              "tipo.documento" = "Tipo Documento",
              "numero.documento" = "Número Documento",
              "observacoes" = "Observações",
              "banco" = "Banco",
              "agencia" = "Agência",
              "conta" = "Conta",
              "empreendimento" = "Empreendimento",
              "classificacao" = "Classificação"
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

    # Data table output for the "Dados" tab - shows original data
    output$data_table <- DT::renderDataTable({
      # Use the original data instead of processed chart data
      req(dados)

      # Create a copy of original data with Portuguese column names
      data_formatted <- dados

      # Translate column names to Portuguese for common financial columns
      col_names <- names(data_formatted)

      # Create a mapping of English to Portuguese column names
      name_mapping <- c(
        "data.doc.pagto" = "Data Pagamento",
        "data.pagamento" = "Data Pagamento",
        "total.pago" = "Valor Pago",
        "total" = "Valor Total",
        "descricao" = "Descrição",
        "fornecedor" = "Fornecedor",
        "centro.custo" = "Centro de Custo",
        "conta.contabil" = "Conta Contábil",
        "natureza" = "Natureza",
        "tipo.documento" = "Tipo Documento",
        "numero.documento" = "Número Documento",
        "observacoes" = "Observações",
        "banco" = "Banco",
        "agencia" = "Agência",
        "conta" = "Conta",
        "empreendimento" = "Empreendimento",
        "classificacao" = "Classificação"
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

# -*- coding: utf-8 -*-
# =============================================================================
# SUBMODULO: graficosBarrasEmpilhadas
# Grafico de barras empilhadas por mes baseado no modulo original completo
# Preserva TODAS as funcionalidades: cliques, modais, downloads, multiplos graficos, etc.
# =============================================================================

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import lubridate
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom RColorBrewer brewer.pal

#' Interface do usuario do submodulo de graficos de barras empilhadas
#'
#' @param id Identificador unico do modulo
#' @param choices Lista de opcoes para agrupamento dos dados
#' @param total Nome da coluna com valores totais
#' @param data Nome da coluna com datas
#' @param comecoTitulo Inicio do titulo do grafico
#' @return Lista de elementos da interface
#' @export
sm_grafico_barras_empilhadas_ui <- function(
    id,
    choices,
    total = "total.pago",
    data = "data.doc.pagto",
    comecoTitulo = "Analise") {
  ns <- NS(id)

  tagList(
    # Professional clean CSS design
    tags$style(HTML("
      .main-content {
        background-color: #fafafa;
        min-height: 100vh;
      }
      .nav-tabs {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 1000 !important;
        background-color: white !important;
        border-bottom: none !important;
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
      .parameters-section {
        background-color: white;
        padding: 15px 20px 10px 20px;
        margin: 0;
        border: 2px solid #dc3545;
        border-bottom: 2px solid #dc3545;
        border-radius: 8px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        position: fixed !important;
        top: 48px !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 999 !important;
      }
      .chart-title-container {
        background-color: white;
        padding: 8px 20px;
        margin: 0;
        border: 1px solid #e5e5e5;
        border-top: none;
        border-radius: 0 0 8px 8px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        position: fixed !important;
        top: calc(48px + var(--params-height, 100px)) !important;
        left: 0 !important;
        right: 0 !important;
        z-index: 998 !important;
      }
      .chart-title {
        background-color: transparent !important;
        background: none !important;
        padding: 0 !important;
        margin: 0 !important;
        border: none !important;
        font-size: 16px !important;
        font-weight: 600 !important;
        color: #333 !important;
        text-align: center !important;
        display: block !important;
        width: 100% !important;
        box-shadow: none !important;
        outline: none !important;
        height: auto !important;
        line-height: 1.1 !important;
      }
      .checkbox-wrapper {
        margin: 8px 0 0 0 !important;
        padding: 0 !important;
        background-color: transparent !important;
      }
      .checkbox-wrapper .form-group {
        margin: 0 !important;
      }
      .checkbox-wrapper .checkbox {
        margin: 0 !important;
        padding: 0 !important;
      }
      .checkbox-wrapper label {
        margin: 0 !important;
        padding-left: 25px !important;
        display: block !important;
        position: relative !important;
        font-weight: 600 !important;
        font-size: 14px !important;
        color: #333 !important;
        line-height: 1.4 !important;
      }
      /* Override Bootstrap checkbox styling */
      .checkbox-wrapper .form-check-label,
      .checkbox-wrapper .checkbox label,
      .checkbox-wrapper label.form-check-label {
        font-weight: 600 !important;
        font-size: 14px !important;
        color: #333 !important;
      }
      /* Override inline styles on checkbox text div */
      .checkbox-wrapper label div,
      .checkbox-wrapper label span div {
        font-weight: 600 !important;
        font-size: 14px !important;
        color: #333 !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      .checkbox-wrapper input[type='checkbox'] {
        position: absolute !important;
        left: 0 !important;
        top: 2px !important;
        margin: 0 !important;
      }
      .main-content-wrapper {
        margin-top: calc(48px + var(--params-height, 100px) + var(--title-height, 35px));
        background-color: #fafafa;
      }
      .chart-container {
        background-color: white;
        margin: 0;
        padding: 20px;
        border-bottom: 1px solid #f0f0f0;
      }
      .form-control, .form-select {
        border: 1px solid #ddd !important;
        box-shadow: none !important;
        height: 38px !important;
        padding: 6px 50px 6px 12px !important;
        font-size: 14px !important;
        line-height: 1.428571429 !important;
        vertical-align: top !important;
        margin: 0 !important;
        box-sizing: border-box !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 2px rgba(0,123,255,0.25) !important;
      }
      /* Ensure select inputs have consistent styling */
      select.form-control, select.form-select {
        height: 38px !important;
        padding: 6px 60px 6px 12px !important;
        line-height: 1.428571429 !important;
        vertical-align: top !important;
        width: 100% !important;
        min-width: 120px !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
        white-space: nowrap !important;
        -webkit-appearance: none !important;
        -moz-appearance: none !important;
        appearance: none !important;
        background-image: linear-gradient(45deg, transparent 50%, #666 50%), linear-gradient(135deg, #666 50%, transparent 50%) !important;
        background-position: calc(100% - 25px) calc(50% + 2px), calc(100% - 20px) calc(50% + 2px) !important;
        background-size: 5px 5px, 5px 5px !important;
        background-repeat: no-repeat !important;
      }
      /* Extra specific rule for variavel selector */
      #chart_tabs-graficos-filtros-variavel {
        padding-right: 60px !important;
      }
      /* Responsive adjustments for small screens */
      @media (max-width: 768px) {
        .col-md-6 {
          margin-bottom: 12px !important;
          padding-left: 0 !important;
          padding-right: 0 !important;
        }
        .row {
          flex-direction: column !important;
        }
        .form-control, .form-select, select.form-control, select.form-select {
          min-width: 150px !important;
        }
        .empresa-filter-button {
          min-width: 150px !important;
          width: 100% !important;
        }
        /* Custom dropdown responsive styling */
        .variavel-selector-wrapper {
          width: 100% !important;
        }
        /* Ensure both fields have same width on mobile */
        .empresa-filter-container,
        .variavel-selector-wrapper {
          max-width: none !important;
          width: 100% !important;
        }
      }
      /* Excel-like popup menu styling */
      .empresa-filter-container {
        position: relative;
        width: 100%;
        display: flex;
        align-items: stretch;
      }
      .empresa-filter-button {
        width: 100% !important;
        text-align: left !important;
        background-color: white !important;
        border: 1px solid #ddd !important;
        padding: 6px 12px !important;
        font-size: 14px !important;
        color: #333 !important;
        border-radius: 4px !important;
        display: flex !important;
        justify-content: space-between !important;
        align-items: center !important;
        height: 38px !important;
        box-sizing: border-box !important;
        margin: 0 !important;
        line-height: 1.428571429 !important;
        vertical-align: top !important;
      }
      .empresa-filter-button:hover {
        background-color: #f8f9fa !important;
        border-color: #007bff !important;
      }
      .empresa-filter-button:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 2px rgba(0,123,255,0.25) !important;
        outline: none !important;
      }
      .empresa-filter-dropdown {
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        right: 0 !important;
        background-color: white !important;
        border: 1px solid #ddd !important;
        border-top: none !important;
        border-radius: 0 0 4px 4px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15) !important;
        z-index: 1050 !important;
        max-height: 200px !important;
        overflow-y: auto !important;
        display: none;
      }
      .empresa-filter-dropdown.show {
        display: block !important;
      }
      .empresa-filter-header {
        padding: 8px 12px !important;
        border-bottom: 1px solid #f0f0f0 !important;
        background-color: #f8f9fa !important;
        display: flex !important;
        gap: 8px !important;
      }
      .empresa-filter-header button {
        font-size: 11px !important;
        padding: 2px 6px !important;
        border-radius: 3px !important;
      }
      .empresa-filter-item {
        padding: 6px 12px !important;
        border-bottom: 1px solid #f8f9fa !important;
        cursor: pointer !important;
        display: flex !important;
        align-items: center !important;
        gap: 8px !important;
        font-size: 13px !important;
      }
      .empresa-filter-item:hover {
        background-color: #f8f9fa !important;
      }
      .empresa-filter-item input[type='checkbox'] {
        margin: 0 !important;
        transform: scale(0.9) !important;
      }
      .empresa-count {
        color: #666 !important;
        font-size: 12px !important;
        font-weight: normal !important;
      }
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

    # JavaScript for dynamic height and popup menu
    tags$script(HTML("
      $(document).ready(function() {
        function updateContainerHeights() {
          var paramsHeight = $('.parameters-section').outerHeight() || 100;
          var titleHeight = $('.chart-title-container').outerHeight() || 35;
          document.documentElement.style.setProperty('--params-height', paramsHeight + 'px');
          document.documentElement.style.setProperty('--title-height', titleHeight + 'px');
        }
        updateContainerHeights();
        $(window).on('resize', updateContainerHeights);
        setTimeout(updateContainerHeights, 500);
        setTimeout(updateContainerHeights, 1000);
        setTimeout(updateContainerHeights, 1500);

        // Force remove checkbox margins and compress spacing
        function compressSpacing() {
          $('.checkbox-wrapper').css({
            'margin-top': '8px',
            'margin-bottom': '0px',
            'padding': '0px'
          });
          $('.checkbox-wrapper *').css({
            'margin': '0px',
            'padding': '0px'
          });
          // Compress parameters section spacing
          $('.parameters-section > div').css({
            'margin-bottom': '12px'
          });
          $('.parameters-section > div:last-child').css({
            'margin-bottom': '0px'
          });
        }

        setTimeout(compressSpacing, 300);
        setTimeout(compressSpacing, 800);
        setTimeout(compressSpacing, 1200);

        // Update heights when content changes
        var observer = new MutationObserver(function() {
          updateContainerHeights();
          setTimeout(compressSpacing, 50);
        });
        observer.observe(document.querySelector('.parameters-section') || document.body, {
          childList: true,
          subtree: true,
          attributes: true
        });
        observer.observe(document.querySelector('.chart-title-container') || document.body, {
          childList: true,
          subtree: true,
          attributes: true
        });
      });

      // Empresa popup menu functions
      function toggleEmpresaDropdown(dropdownId) {
        var dropdown = document.getElementById(dropdownId);
        if (dropdown) {
          dropdown.classList.toggle('show');
        }
        // Close dropdown when clicking outside
        $(document).off('click.empresaDropdown').on('click.empresaDropdown', function(e) {
          if (!$(e.target).closest('.empresa-filter-container').length) {
            $('.empresa-filter-dropdown').removeClass('show');
            $(document).off('click.empresaDropdown');
          }
        });
      }

      function selectAllEmpresas(nsPrefix) {
        $('.empresa-filter-item input[type=checkbox]').prop('checked', true);
        updateEmpresaSelection(nsPrefix);
      }

      function deselectAllEmpresas(nsPrefix) {
        $('.empresa-filter-item input[type=checkbox]').prop('checked', false);
        updateEmpresaSelection(nsPrefix);
      }

      function toggleEmpresaCheck(nsPrefix, empresa) {
        var checkboxId = nsPrefix + 'empresa_check_' + empresa.replace(/[^A-Za-z0-9]/g, '_');
        var checkbox = document.getElementById(checkboxId);
        if (checkbox) {
          checkbox.checked = !checkbox.checked;
          updateEmpresaSelection(nsPrefix);
        }
      }

      function updateEmpresaSelection(nsPrefix) {
        var selected = [];
        $('.empresa-filter-item input[type=checkbox]:checked').each(function() {
          var label = $(this).siblings('label').text();
          selected.push(label);
        });

        // Update hidden input
        var hiddenInput = document.getElementById(nsPrefix + 'empresa');
        if (hiddenInput) {
          hiddenInput.value = selected.join(',');
          $(hiddenInput).trigger('change');
        }

        // Update button text
        var displayText = '';
        if (selected.length === 0) {
          displayText = 'Nenhuma empresa selecionada';
        } else if (selected.length === 1) {
          displayText = selected[0];
        } else if (selected.length <= 3) {
          displayText = selected.join(', ');
        } else {
          displayText = selected.length + ' empresas selecionadas';
        }

        var displaySpan = document.getElementById(nsPrefix + 'empresa_display');
        if (displaySpan) {
          displaySpan.textContent = displayText;
        }
      }
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
          title = "Gráficos",
          value = "graficos",

          # Clean parameters section
          div(
            class = "parameters-section",
            h4("Parâmetros", style = "margin: 0 0 15px 0; font-weight: 600; color: #333; font-size: 18px;"),

            # Period filter
            div(
              style = "margin-bottom: 12px;",
              h5("Período", style = "margin: 0 0 6px 0; font-weight: 600; color: #333; font-size: 14px;"),
              sm_filtro_periodo_ui(ns("filtro"))
            ),

            # Company filter and Variable selection in same row
            div(
              style = "margin-bottom: 12px;",
              div(
                class = "row",
                style = "align-items: flex-start; margin: 0; display: flex;",
                div(
                  class = "col-md-6 col-sm-12",
                  style = "display: flex; flex-direction: column; align-items: stretch; margin-bottom: 8px; padding-right: 10px;",
                  h5("Empresa(s)", style = "margin: 0 0 6px 0; font-weight: 600; color: #333; font-size: 14px; height: 20px; line-height: 20px;"),
                  div(
                    style = "flex: 1; display: flex; align-items: stretch; min-width: 0;",
                    uiOutput(ns("empresa_selector"))
                  )
                ),
                div(
                  class = "col-md-6 col-sm-12",
                  style = "display: flex; flex-direction: column; align-items: stretch; margin-bottom: 8px; padding-left: 10px;",
                  h5("Empilhar barras por", style = "margin: 0 0 6px 0; font-weight: 600; color: #333; font-size: 14px; height: 20px; line-height: 20px;"),
                  div(
                    style = "flex: 1; display: flex; align-items: stretch; min-width: 0;",
                    uiOutput(ns("variavel_selector"))
                  )
                )
              )
            ),

            # Checkbox wrapper
            div(
              class = "checkbox-wrapper",
              uiOutput(ns("checkbox_wrapper"))
            )
          ),

          # Chart title container - positioned after parameters
          div(
            class = "chart-title-container",
            h4(textOutput(ns("charts_title")), class = "chart-title")
          ),

          # Charts content wrapper
          div(
            class = "main-content-wrapper",
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
          title = "Estatísticas",
          value = "estatisticas",
          div(
            class = "chart-container",
            style = "text-align: center;",
            h4("Estatísticas", style = "color: #333; margin-bottom: 10px;"),
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

#' Servidor do submodulo de graficos de barras empilhadas
#'
#' @param id Identificador unico do modulo
#' @param dados Dados para analise
#' @param filtro_periodo Reativo com tipo de filtro de periodo
#' @param data_inicial Reativo com data inicial (para filtro personalizado)
#' @param data_final Reativo com data final (para filtro personalizado)
#' @param choices Lista de opcoes para agrupamento
#' @param max_unicos_i Maximo de categorias unicas antes de agrupar em "Outros"
#' @param total Nome da coluna com valores totais
#' @param data Nome da coluna com datas
#' @param comecoTitulo Inicio do titulo do grafico
#' @return Funcao do servidor do modulo
#' @export
sm_grafico_barras_empilhadas_server <- function(
    id,
    dados,
    choices,
    max_unicos_i = 20,
    total = "total.pago",
    data = "data.doc.pagto",
    comecoTitulo = "Analise") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize internal period filter
    periodo_filtro <- sm_filtro_periodo_server("filtro")

    # Verificar se dados e reativo ou estatico
    dados_final <- reactive({
      if (shiny::is.reactive(dados)) {
        dados()
      } else {
        dados
      }
    })

    # Define source_id for plotly events
    source_id <- paste0(id, "_click")
    detail_rv <- reactiveVal(NULL)
    top_vars_rv <- reactiveVal(NULL)

    # 1) Reactive for date range - use internal filter
    period <- reactive({
      req(periodo_filtro$filtro_periodo())
      today <- Sys.Date()
      switch(periodo_filtro$filtro_periodo(),
        "ano_corrente" = list(start = as.Date(paste0(format(today, "%Y"), "-01-01")), end = today),
        "ultimos_12" = list(start = as.Date(format(today - 365, "%Y-%m-%d")), end = today),
        "desde_inicio" = {
          df <- dados_final()
          req(df)
          dt <- as.Date(df[[data]], origin = "1970-01-01")
          list(start = min(dt, na.rm = TRUE), end = today)
        },
        "personalizado" = {
          req(periodo_filtro$data_inicial(), periodo_filtro$data_final())
          start_date <- tryCatch(
            {
              if (inherits(periodo_filtro$data_inicial(), "Date")) {
                periodo_filtro$data_inicial()
              } else {
                as.Date(periodo_filtro$data_inicial())
              }
            },
            error = function(e) {
              tryCatch(
                {
                  as.Date(periodo_filtro$data_inicial(), format = "%d/%m/%Y")
                },
                error = function(e2) {
                  lubridate::dmy(periodo_filtro$data_inicial())
                }
              )
            }
          )

          end_date <- tryCatch(
            {
              if (inherits(periodo_filtro$data_final(), "Date")) {
                periodo_filtro$data_final()
              } else {
                as.Date(periodo_filtro$data_final())
              }
            },
            error = function(e) {
              tryCatch(
                {
                  as.Date(periodo_filtro$data_final(), format = "%d/%m/%Y")
                },
                error = function(e2) {
                  lubridate::dmy(periodo_filtro$data_final())
                }
              )
            }
          )

          list(start = start_date, end = end_date)
        }
      )
    })

    # 2) Chart title - PROPER DYNAMIC VERSION
    chart_title <- reactive({
      # Require variable selection
      req(input$variavel)

      # Get the display name for the variable from choices
      var_display_name <- names(choices)[choices == input$variavel]
      if (length(var_display_name) == 0) {
        var_display_name <- input$variavel
      }

      # Get period text using internal filter
      period_text <- tryCatch(
        {
          req(periodo_filtro$filtro_periodo())
          periodo_value <- periodo_filtro$filtro_periodo()

          switch(periodo_value,
            "ano_corrente" = "no ano corrente",
            "ultimos_12" = "nos últimos 12 meses",
            "desde_inicio" = "desde o início",
            "personalizado" = {
              req(periodo_filtro$data_inicial(), periodo_filtro$data_final())
              sprintf(
                "de %s até %s",
                format(periodo_filtro$data_inicial(), "%d/%m/%Y"),
                format(periodo_filtro$data_final(), "%d/%m/%Y")
              )
            }
          )
        },
        error = function(e) {
          "nos últimos 12 meses"
        }
      )

      # Get empresa text for multi-select
      empresa_text <- ""
      if (!is.null(input$empresa) && length(input$empresa) > 0 && input$empresa != "") {
        selected_empresas <- if (is.character(input$empresa)) {
          strsplit(input$empresa, ",")[[1]]
        } else {
          input$empresa
        }

        if (length(selected_empresas) > 0 && selected_empresas[1] != "") {
          if (length(selected_empresas) == 1) {
            empresa_text <- sprintf(" - %s", selected_empresas[1])
          } else if (length(selected_empresas) <= 3) {
            empresa_text <- sprintf(" - %s", paste(selected_empresas, collapse = ", "))
          } else {
            empresa_text <- sprintf(" - %d empresas selecionadas", length(selected_empresas))
          }
        }
      }

      # Combine all parts
      sprintf("%s por %s %s%s", comecoTitulo, var_display_name, period_text, empresa_text)
    })

    output$charts_title <- renderText({
      chart_title()
    })

    # 3) Reactive data: group by month + stacking variable
    df_data <- reactive({
      pr <- period()
      df <- dados_final()
      req(df, pr, pr$start, pr$end, input$variavel)

      dt_var <- data
      total_var <- total
      group_var <- input$variavel

      date_col <- as.Date(df[[dt_var]])
      date_mask <- date_col >= pr$start & date_col <= pr$end
      filtered_data <- df[date_mask, ]

      # Handle multi-select empresa filtering
      if (!is.null(input$empresa) && length(input$empresa) > 0 && input$empresa != "" && "empresa" %in% names(filtered_data)) {
        selected_empresas <- if (is.character(input$empresa)) {
          strsplit(input$empresa, ",")[[1]]
        } else {
          input$empresa
        }
        if (length(selected_empresas) > 0 && selected_empresas[1] != "") {
          empresa_mask <- filtered_data$empresa %in% selected_empresas
          filtered_data <- filtered_data[empresa_mask & !is.na(empresa_mask), ]
        }
      }

      group_values <- as.character(filtered_data[[group_var]])
      month_dates <- as.Date(paste0(format(as.Date(filtered_data[[dt_var]]), "%Y-%m"), "-01"))

      agg_data <- data.frame(
        mes = month_dates,
        var = group_values,
        value = filtered_data[[total_var]]
      )

      result <- aggregate(
        agg_data$value,
        by = list(mes = agg_data$mes, var = agg_data$var),
        FUN = sum,
        na.rm = TRUE
      )
      colnames(result)[3] <- "total"
      result
    })

    # 4) Checkbox for showing all categories
    output$checkbox_wrapper <- renderUI({
      d <- df_data()
      req(d)

      distinct_vars <- length(unique(d$var))
      if (distinct_vars <= max_unicos_i) {
        return(NULL)
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

    # 5) Group categories into "Outros"
    df_reduced <- reactive({
      d <- df_data()
      req(d)

      distinct_vars <- length(unique(d$var))
      if (distinct_vars <= max_unicos_i || isTRUE(input$show_all_cats)) {
        top_vars_rv(NULL)
        return(d)
      }

      totals_by_var <- aggregate(d$total, by = list(var = d$var), FUN = sum)
      colnames(totals_by_var)[2] <- "totalvar"
      totals_by_var <- totals_by_var[order(totals_by_var$totalvar, decreasing = TRUE), ]

      top_vars <- totals_by_var$var[seq_len(max_unicos_i - 1)]
      top_vars_rv(top_vars)

      d$var <- ifelse(d$var %in% top_vars, d$var, "Outros")

      result <- aggregate(d$total, by = list(mes = d$mes, var = d$var), FUN = sum)
      colnames(result)[3] <- "total"
      result
    })

    # Monthly totals
    monthly_totals <- reactive({
      reduced <- df_reduced()
      result <- aggregate(reduced$total, by = list(mes = reduced$mes), FUN = sum, na.rm = TRUE)
      colnames(result)[2] <- "monthtotal"
      result
    })

    df_final <- reactive({
      reduced <- df_reduced()
      monthlies <- monthly_totals()
      result <- merge(reduced, monthlies, by = "mes", all.x = TRUE)
      result$monthtotal <- ifelse(result$monthtotal == 0, NA, result$monthtotal)
      result$percentage <- 100 * result$total / result$monthtotal
      result
    })

    # 6) Render the stacked bar chart
    output$plot <- renderPlotly({
      df <- df_final()
      req(df, nrow(df) > 0)

      totals_by_var <- aggregate(df$total, by = list(var = df$var), FUN = sum)
      colnames(totals_by_var)[2] <- "total_var"
      totals_by_var <- totals_by_var[order(totals_by_var$total_var, decreasing = TRUE), ]
      var_levels <- totals_by_var$var
      df$var <- factor(df$var, levels = var_levels)

      pal8 <- RColorBrewer::brewer.pal(8, "Set2")
      pal <- if (length(var_levels) <= 8) pal8[seq_along(var_levels)] else colorRampPalette(pal8)(length(var_levels))

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
          xaxis = list(
            title = "Mes",
            tickformat = "%m-%Y",
            type = "date",
            tickvals = unique(df$mes),
            fixedrange = TRUE
          ),
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

    # Line chart output
    output$line_plot <- renderPlotly({
      df <- df_final()
      req(df, input$variavel)

      line_data <- df %>%
        group_by(mes, var) %>%
        summarise(total_grupo = sum(total), .groups = "drop") %>%
        arrange(mes, var)

      line_monthly_totals <- df %>%
        group_by(mes) %>%
        summarise(monthtotal = sum(total), .groups = "drop")

      line_data <- line_data %>%
        left_join(line_monthly_totals, by = "mes") %>%
        mutate(percentage = 100 * total_grupo / monthtotal)

      all_vars <- unique(line_data$var)
      outros_vars <- all_vars[all_vars == "Outros"]
      non_outros_vars <- sort(all_vars[all_vars != "Outros"])
      unique_vars <- c(non_outros_vars, outros_vars)
      colors <- RColorBrewer::brewer.pal(min(length(unique_vars), 11), "Spectral")
      if (length(unique_vars) > 11) {
        colors <- rep(colors, length.out = length(unique_vars))
      }

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

      p_line <- p_line %>%
        plotly::layout(
          xaxis = list(
            title = "Mes",
            tickformat = "%m-%Y",
            type = "date",
            fixedrange = TRUE
          ),
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

    # 100% bar chart output
    output$percent_plot <- renderPlotly({
      df <- df_reduced()
      req(df, input$variavel)

      period_data <- df %>%
        group_by(var) %>%
        summarise(total_periodo = sum(total), .groups = "drop") %>%
        arrange(desc(total_periodo)) %>%
        mutate(
          percentage = 100 * total_periodo / sum(total_periodo),
          cumulative = cumsum(percentage)
        )

      total_geral_value <- sum(period_data$total_periodo)
      period_data$total_geral <- total_geral_value
      period_data$var <- factor(period_data$var, levels = period_data$var)

      unique_vars <- levels(period_data$var)
      colors <- RColorBrewer::brewer.pal(min(length(unique_vars), 11), "Spectral")
      if (length(unique_vars) > 11) {
        colors <- rep(colors, length.out = length(unique_vars))
      }

      p_percent <- plot_ly(
        data = period_data,
        x = ~percentage,
        y = rep("Distribuicao", nrow(period_data)),
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
            fixedrange = TRUE
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE,
            fixedrange = TRUE
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

    # Handle clicking any bar segment
    observe({
      req(input$variavel, df_data(), nrow(df_data()) > 0)

      click_data <- NULL
      source_type <- NULL

      # Try to get click data from each source
      tryCatch(
        {
          click_data_bars <- event_data("plotly_click", source = source_id)
          if (!is.null(click_data_bars)) {
            click_data <- click_data_bars
            source_type <- "bars"
          }
        },
        error = function(e) NULL
      )

      if (is.null(click_data)) {
        tryCatch(
          {
            click_data_line <- event_data("plotly_click", source = paste0(source_id, "_line"))
            if (!is.null(click_data_line)) {
              click_data <- click_data_line
              source_type <- "line"
            }
          },
          error = function(e) NULL
        )
      }

      if (is.null(click_data)) {
        tryCatch(
          {
            click_data_percent <- event_data("plotly_click", source = paste0(source_id, "_percent"))
            if (!is.null(click_data_percent)) {
              click_data <- click_data_percent
              source_type <- "percent"
            }
          },
          error = function(e) NULL
        )
      }

      if (!is.null(click_data) && !is.null(source_type)) {
        clicked_var <- click_data$key
        tv <- top_vars_rv()
        df <- dados_final()

        if (source_type == "percent") {
          if (!is.null(tv) && clicked_var == "Outros") {
            dt_var <- data
            group_var <- input$variavel
            pr <- period()
            date_col <- as.Date(df[[dt_var]])
            date_mask <- date_col >= pr$start & date_col <= pr$end
            cat_values <- df[[group_var]]
            cat_match <- !(cat_values %in% tv)
            detail_data <- df[date_mask & cat_match, ]
          } else {
            dt_var <- data
            group_var <- input$variavel
            pr <- period()
            date_col <- as.Date(df[[dt_var]])
            date_mask <- date_col >= pr$start & date_col <= pr$end
            cat_match <- df[[group_var]] == clicked_var
            detail_data <- df[date_mask & cat_match, ]
          }
          modal_title <- paste("Detalhes:", clicked_var, "- Periodo completo")
        } else {
          clicked_month <- as.Date(click_data$x, origin = "1970-01-01")

          if (!is.null(tv) && clicked_var == "Outros") {
            dt_var <- data
            group_var <- input$variavel
            date_col <- as.Date(df[[dt_var]])
            month_match <- as.Date(paste0(format(date_col, "%Y-%m"), "-01")) == as.Date(paste0(format(clicked_month, "%Y-%m"), "-01"))
            cat_values <- df[[group_var]]
            cat_match <- !(cat_values %in% tv)
            detail_data <- df[month_match & cat_match, ]
          } else {
            dt_var <- data
            group_var <- input$variavel
            date_col <- as.Date(df[[dt_var]])
            month_match <- as.Date(paste0(format(date_col, "%Y-%m"), "-01")) == as.Date(paste0(format(clicked_month, "%Y-%m"), "-01"))
            cat_match <- df[[group_var]] == clicked_var
            detail_data <- df[month_match & cat_match, ]
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
            detail_formatted <- detail_data
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
                  list(targets = "_all", className = "dt-nowrap")
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

    # Data table output for the "Dados" tab
    output$data_table <- DT::renderDataTable({
      df <- dados_final()
      req(df)

      pr <- period()
      req(pr, pr$start, pr$end)

      dt_var <- data
      date_col <- as.Date(df[[dt_var]])
      date_mask <- date_col >= pr$start & date_col <= pr$end
      filtered_data <- df[date_mask, ]

      # Handle multi-select empresa filtering
      if (!is.null(input$empresa) && length(input$empresa) > 0 && input$empresa != "" && "empresa" %in% names(filtered_data)) {
        selected_empresas <- if (is.character(input$empresa)) {
          strsplit(input$empresa, ",")[[1]]
        } else {
          input$empresa
        }
        if (length(selected_empresas) > 0 && selected_empresas[1] != "") {
          empresa_mask <- filtered_data$empresa %in% selected_empresas
          filtered_data <- filtered_data[empresa_mask & !is.na(empresa_mask), ]
        }
      }

      data_formatted <- filtered_data
      col_names <- names(data_formatted)

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
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"),
          columnDefs = list(
            list(targets = "_all", className = "dt-nowrap")
          )
        ),
        extensions = c("Buttons"),
        class = "stripe hover cell-border dt-nowrap",
        rownames = FALSE
      )
    })

    # Dynamic UI outputs for empresa and variavel selectors
    output$empresa_selector <- renderUI({
      df <- dados_final()
      req(df)

      if ("empresa" %in% names(df)) {
        empresas <- sort(unique(df$empresa))
        empresas <- empresas[!is.na(empresas)]

        tagList(
          # Popup menu container
          div(
            class = "empresa-filter-container",
            # Main button
            tags$button(
              id = ns("empresa_filter_btn"),
              class = "empresa-filter-button",
              type = "button",
              onclick = sprintf("toggleEmpresaDropdown('%s')", ns("empresa_dropdown")),
              span(id = ns("empresa_display"), "Todas as empresas selecionadas"),
              tags$i(class = "fa fa-chevron-down", style = "margin-left: auto;")
            ),
            # Dropdown menu
            div(
              id = ns("empresa_dropdown"),
              class = "empresa-filter-dropdown",
              # Header with Select All/Deselect All
              div(
                class = "empresa-filter-header",
                tags$button(
                  "Todas",
                  class = "btn btn-xs btn-outline-primary",
                  onclick = sprintf("selectAllEmpresas('%s')", ns("")),
                  type = "button"
                ),
                tags$button(
                  "Nenhuma",
                  class = "btn btn-xs btn-outline-secondary",
                  onclick = sprintf("deselectAllEmpresas('%s')", ns("")),
                  type = "button"
                )
              ),
              # Company checkboxes
              lapply(empresas, function(empresa) {
                div(
                  class = "empresa-filter-item",
                  onclick = sprintf("toggleEmpresaCheck('%s', '%s')", ns(""), empresa),
                  tags$input(
                    type = "checkbox",
                    id = sprintf("%s_check_%s", ns("empresa"), gsub("[^A-Za-z0-9]", "_", empresa)),
                    checked = "checked",
                    onchange = sprintf("updateEmpresaSelection('%s')", ns(""))
                  ),
                  tags$label(
                    `for` = sprintf("%s_check_%s", ns("empresa"), gsub("[^A-Za-z0-9]", "_", empresa)),
                    empresa,
                    style = "margin: 0; cursor: pointer; user-select: none;"
                  )
                )
              })
            )
          ),
          # Hidden input to store selected values
          tags$input(
            id = ns("empresa"),
            type = "hidden",
            value = paste(empresas, collapse = ",")
          )
        )
      } else {
        div(
          style = "text-align: center; color: #666; font-style: italic;",
          "Nenhuma empresa disponível"
        )
      }
    })

    output$variavel_selector <- renderUI({
      df <- dados_final()
      req(df)

      # Use the choices passed from the parent module instead of hardcoded ones
      available_choices <- choices[choices %in% names(df)]

      default_selection <- if (length(available_choices) > 0) {
        available_choices[1]
      } else {
        NULL
      }

      div(
        class = "variavel-selector-wrapper",
        style = "width: 100%;",
        tags$style(HTML(paste0("
          .custom-dropdown-", ns("variavel"), " {
            position: relative;
            width: 100%;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-display {
            width: 100%;
            height: 38px;
            padding: 6px 40px 6px 12px;
            border: 1px solid #ccc;
            border-radius: 4px;
            background-color: white;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: space-between;
            font-family: inherit;
            font-size: 14px;
            line-height: 1.42857143;
            color: #555;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-display:hover {
            border-color: #66afe9;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-arrow {
            width: 0;
            height: 0;
            border-left: 4px solid transparent;
            border-right: 4px solid transparent;
            border-top: 4px solid #555;
            margin-left: 8px;
            flex-shrink: 0;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-menu {
            position: absolute;
            top: 100%;
            left: 0;
            right: 0;
            background: white;
            border: 1px solid #ccc;
            border-top: none;
            max-height: 200px;
            overflow-y: auto;
            z-index: 1050;
            display: none;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-option {
            padding: 8px 12px;
            cursor: pointer;
            border-bottom: 1px solid #f5f5f5;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-option:hover {
            background-color: #f5f5f5;
          }
          .custom-dropdown-", ns("variavel"), " .dropdown-option.selected {
            background-color: #337ab7;
            color: white;
          }
        "))),

        div(
          class = paste0("custom-dropdown-", ns("variavel")),
          div(
            class = "dropdown-display",
            id = ns("variavel_display"),
            onclick = paste0("toggleVariavelDropdown('", ns("variavel_menu"), "')"),
            span(id = ns("variavel_text"), if(length(available_choices) > 0) names(available_choices)[1] else "Selecione..."),
            div(class = "dropdown-arrow")
          ),
          div(
            class = "dropdown-menu",
            id = ns("variavel_menu"),
            lapply(seq_along(available_choices), function(i) {
              choice_value <- available_choices[i]
              choice_label <- names(available_choices)[i]
              if (is.null(choice_label) || choice_label == "") {
                choice_label <- choice_value
              }

              div(
                class = if(i == 1) "dropdown-option selected" else "dropdown-option",
                `data-value` = choice_value,
                onclick = paste0("selectVariavelOption('", ns("variavel"), "', '", choice_value, "', '", choice_label, "', '", ns("variavel_text"), "', '", ns("variavel_menu"), "')"),
                choice_label
              )
            })
          )
        ),

        # Hidden input to store the selected value
        tags$input(
          id = ns("variavel"),
          type = "hidden",
          value = if(length(available_choices) > 0) available_choices[1] else ""
        ),

        tags$script(HTML("
          function toggleVariavelDropdown(menuId) {
            var menu = document.getElementById(menuId);
            if (menu.style.display === 'block') {
              menu.style.display = 'none';
            } else {
              menu.style.display = 'block';
            }
          }

          function selectVariavelOption(inputId, value, label, textId, menuId) {
            // Update the hidden input
            document.getElementById(inputId).value = value;

            // Update the display text
            document.getElementById(textId).textContent = label;

            // Update selected state
            var menu = document.getElementById(menuId);
            var options = menu.querySelectorAll('.dropdown-option');
            options.forEach(function(option) {
              option.classList.remove('selected');
            });
            event.target.classList.add('selected');

            // Close the menu
            menu.style.display = 'none';

            // Trigger Shiny input change
            $(document).ready(function() {
              $('#' + inputId).trigger('change');
            });
          }

          // Close dropdown when clicking outside
          document.addEventListener('click', function(event) {
            var dropdowns = document.querySelectorAll('[id$=\"variavel_menu\"]');
            dropdowns.forEach(function(dropdown) {
              if (!dropdown.contains(event.target) && !dropdown.previousElementSibling.contains(event.target)) {
                dropdown.style.display = 'none';
              }
            });
          });

          // Handle custom message for updating dropdown
          Shiny.addCustomMessageHandler('updateCustomDropdown', function(message) {
            var hiddenInput = document.getElementById(message.elementId);
            var displayText = document.getElementById(message.textId);
            var menu = document.getElementById(message.menuId);

            if (hiddenInput && displayText && menu) {
              // Update hidden input value
              hiddenInput.value = message.selected || '';

              // Update display text
              var selectedLabel = '';
              if (message.choices && message.selected) {
                var choiceNames = Object.keys(message.choices);
                var choiceIndex = Object.values(message.choices).indexOf(message.selected);
                selectedLabel = choiceIndex >= 0 ? choiceNames[choiceIndex] : message.selected;
              }
              displayText.textContent = selectedLabel || 'Selecione...';

              // Rebuild menu options
              menu.innerHTML = '';
              if (message.choices) {
                Object.keys(message.choices).forEach(function(label, index) {
                  var value = message.choices[label];
                  var option = document.createElement('div');
                  option.className = value === message.selected ? 'dropdown-option selected' : 'dropdown-option';
                  option.setAttribute('data-value', value);
                  option.textContent = label;
                  option.onclick = function() {
                    selectVariavelOption(message.elementId, value, label, message.textId, message.menuId);
                  };
                  menu.appendChild(option);
                });
              }

              // Trigger change event
              $(hiddenInput).trigger('change');
            }
          });
        "))
      )
    })

    # Observer to handle empresa changes and update variavel selection
    observeEvent(input$empresa,
      {
        df <- dados_final()
        req(df, input$empresa)

        # Use the choices passed from the parent module instead of hardcoded ones
        available_choices <- choices[choices %in% names(df)]

        # For multi-select, we don't need to exclude empresa from choices
        default_selection <- if (length(available_choices) > 0) {
          available_choices[1]
        } else {
          NULL
        }

        # Update custom dropdown instead of selectInput
        session$sendCustomMessage(
          type = "updateCustomDropdown",
          message = list(
            elementId = ns("variavel"),
            textId = ns("variavel_text"),
            menuId = ns("variavel_menu"),
            choices = available_choices,
            selected = default_selection
          )
        )
      },
      ignoreInit = TRUE
    )
  })
}

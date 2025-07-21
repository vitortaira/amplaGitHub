# -*- coding: utf-8 -*-
# =============================================================================
# SUBMÓDULO: graficosBarrasEmpilhadas
# Gráficos de barras empilhadas por mês para análise de despesas e receitas
# Componente reutilizável para diferentes módulos de análise financeira
# =============================================================================

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import lubridate
#' @importFrom DT datatable
#' @importFrom RColorBrewer brewer.pal

#' Interface do usuário do submódulo de gráficos de barras empilhadas
#'
#' @param id Identificador único do módulo
#' @param choices Lista de opções para agrupamento dos dados
#' @param total Nome da coluna com valores totais
#' @param data Nome da coluna com datas
#' @param comecoTitulo Início do título do gráfico
#' @return Lista de elementos da interface
#' @export
sm_grafico_barras_empilhadas_ui <- function(
    id,
    choices,
    total = "total.pago",
    data = "data.doc.pagto",
    comecoTitulo = "Análise") {
  ns <- NS(id)
  tagList(
    # Design CSS profissional e limpo
    tags$style(HTML("
        /* Design profissional e limpo sem sombras e bordas amadoras */
        .main-content {
          background-color: #fafafa;
          min-height: 100vh;
        }

        /* Abas fixas no topo */
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

        /* Seção de parâmetros fixa */
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

        /* Título fixo com padding adequado e legibilidade */
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
        }

        /* Adicionar margem superior ao conteúdo principal */
        .main-content-wrapper {
          margin-top: calc(48px + var(--params-height, 140px) + 58px);
          background-color: #fafafa;
        }

        /* Containers de gráfico sem bordas feias */
        .chart-container {
          background-color: white;
          margin: 0;
          padding: 20px;
        }

        /* Controles organizados horizontalmente */
        .controls-row {
          display: flex;
          flex-wrap: wrap;
          gap: 20px;
          align-items: flex-end;
          margin-bottom: 15px;
        }

        .control-group {
          flex: 1;
          min-width: 150px;
        }

        /* Customização dos inputs */
        .shiny-input-container {
          margin-bottom: 10px;
        }

        /* Radio buttons em linha */
        .shiny-input-radiogroup {
          display: flex;
          flex-wrap: wrap;
          gap: 15px;
        }

        .radio {
          margin: 0;
        }

        /* Responsividade */
        @media (max-width: 768px) {
          .controls-row {
            flex-direction: column;
            gap: 10px;
          }
          .control-group {
            min-width: 100%;
          }
          .nav-tabs {
            padding: 0 10px !important;
          }
          .parameters-section {
            padding: 15px;
          }
          .chart-title {
            padding: 10px 15px;
            font-size: 16px;
          }
        }
      ")),

    # Seção de parâmetros fixa no topo
    div(
      class = "parameters-section",
      id = ns("parameters"),
      div(
        class = "controls-row",
        div(
          class = "control-group",
          selectInput(
            ns("agrupamento"),
            "Agrupar por:",
            choices = choices,
            selected = names(choices)[1]
          )
        ),
        div(
          class = "control-group",
          sm_filtro_periodo_ui(ns("filtro_periodo"))
        )
      )
    ),

    # Título dinâmico
    div(
      class = "chart-title",
      textOutput(ns("titulo_dinamico"))
    ),

    # Conteúdo principal com margem para elementos fixos
    div(
      class = "main-content-wrapper",

      # Abas para diferentes visualizações
      tabsetPanel(
        id = ns("abas"),
        type = "tabs",

        # Aba do gráfico principal
        tabPanel(
          "Gráfico por Mês",
          value = "grafico_mes",
          div(
            class = "chart-container",
            plotlyOutput(ns("grafico_barras"), height = "600px")
          )
        ),

        # Aba de resumo por período
        tabPanel(
          "Resumo por Período",
          value = "resumo_periodo",
          div(
            class = "chart-container",
            plotlyOutput(ns("grafico_resumo"), height = "500px"),
            br(),
            DT::dataTableOutput(ns("tabela_resumo"))
          )
        ),

        # Aba de dados detalhados
        tabPanel(
          "Dados Detalhados",
          value = "dados_detalhados",
          div(
            class = "chart-container",
            DT::dataTableOutput(ns("tabela_detalhada"))
          )
        )
      )
    )
  )
}

#' Servidor do submódulo de gráficos de barras empilhadas
#'
#' @param id Identificador único do módulo
#' @param dados Dados para análise
#' @param choices Lista de opções para agrupamento
#' @param total Nome da coluna com valores totais
#' @param data Nome da coluna com datas
#' @param comecoTitulo Início do título do gráfico
#' @return Função do servidor do módulo
#' @export
sm_grafico_barras_empilhadas_server <- function(
    id,
    dados,
    choices,
    total = "total.pago",
    data = "data.doc.pagto",
    comecoTitulo = "Análise") {
  moduleServer(id, function(input, output, session) {
    # Inicializar submódulo de filtro de período
    filtro_valores <- sm_filtro_periodo_server("filtro_periodo")

    # Dados filtrados por período
    dados_filtrados <- reactive({
      req(dados)

      df <- dados

      # Aplicar filtro de período
      filtro_tipo <- filtro_valores$filtro_periodo()

      if (filtro_tipo == "ano_corrente") {
        ano_atual <- year(Sys.Date())
        df <- df %>%
          filter(year(!!sym(data)) == ano_atual)
      } else if (filtro_tipo == "ultimos_12") {
        data_limite <- Sys.Date() - months(12)
        df <- df %>%
          filter(!!sym(data) >= data_limite)
      } else if (filtro_tipo == "personalizado") {
        data_inicial <- filtro_valores$data_inicial()
        data_final <- filtro_valores$data_final()
        df <- df %>%
          filter(
            !!sym(data) >= data_inicial,
            !!sym(data) <= data_final
          )
      }
      # "desde_inicio" não precisa de filtro

      return(df)
    })

    # Título dinâmico baseado na seleção
    output$titulo_dinamico <- renderText({
      agrupamento_selecionado <- input$agrupamento
      nome_agrupamento <- names(choices)[choices == agrupamento_selecionado]
      paste(comecoTitulo, nome_agrupamento)
    })

    # Dados agrupados para o gráfico principal
    dados_agrupados <- reactive({
      req(dados_filtrados(), input$agrupamento)

      df <- dados_filtrados()

      # Criar coluna de ano-mês para agrupamento temporal
      df <- df %>%
        mutate(
          ano_mes = format(!!sym(data), "%Y-%m"),
          mes_nome = format(!!sym(data), "%b %Y")
        )

      # Agrupar dados por mês e categoria selecionada
      resultado <- df %>%
        group_by(ano_mes, mes_nome, !!sym(input$agrupamento)) %>%
        summarise(
          valor = sum(!!sym(total), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(ano_mes)

      return(resultado)
    })

    # Gráfico de barras empilhadas por mês
    output$grafico_barras <- renderPlotly({
      req(dados_agrupados())

      df <- dados_agrupados()

      if (nrow(df) == 0) {
        return(
          plot_ly() %>%
            add_text(
              x = 0.5, y = 0.5,
              text = "Nenhum dado disponível para o período selecionado",
              textfont = list(size = 16),
              showlegend = FALSE
            ) %>%
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      # Criar gráfico plotly
      p <- df %>%
        plot_ly(
          x = ~mes_nome,
          y = ~valor,
          color = ~ get(input$agrupamento),
          type = "bar",
          hovertemplate = paste(
            "<b>%{fullData.color}</b><br>",
            "Mês: %{x}<br>",
            "Valor: R$ %{y:,.2f}<br>",
            "<extra></extra>"
          )
        ) %>%
        layout(
          title = list(
            text = "",
            font = list(size = 18, family = "Arial, sans-serif")
          ),
          xaxis = list(
            title = "Mês",
            tickangle = -45,
            font = list(family = "Arial, sans-serif")
          ),
          yaxis = list(
            title = "Valor (R$)",
            tickformat = ",.0f",
            font = list(family = "Arial, sans-serif")
          ),
          barmode = "stack",
          hovermode = "closest",
          font = list(family = "Arial, sans-serif"),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "pan2d", "select2d", "lasso2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines"
          )
        )

      return(p)
    })

    # Dados para resumo por período
    dados_resumo <- reactive({
      req(dados_filtrados(), input$agrupamento)

      df <- dados_filtrados()

      resultado <- df %>%
        group_by(!!sym(input$agrupamento)) %>%
        summarise(
          valor_total = sum(!!sym(total), na.rm = TRUE),
          quantidade = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(valor_total))

      return(resultado)
    })

    # Gráfico de resumo (pizza ou barras horizontais)
    output$grafico_resumo <- renderPlotly({
      req(dados_resumo())

      df <- dados_resumo()

      if (nrow(df) == 0) {
        return(
          plot_ly() %>%
            add_text(
              x = 0.5, y = 0.5,
              text = "Nenhum dado disponível",
              showlegend = FALSE
            )
        )
      }

      # Gráfico de barras horizontais para melhor legibilidade
      p <- df %>%
        plot_ly(
          y = ~ reorder(get(input$agrupamento), valor_total),
          x = ~valor_total,
          type = "bar",
          orientation = "h",
          hovertemplate = paste(
            "<b>%{y}</b><br>",
            "Valor Total: R$ %{x:,.2f}<br>",
            "Quantidade: %{customdata}<br>",
            "<extra></extra>"
          ),
          customdata = ~quantidade
        ) %>%
        layout(
          title = list(
            text = paste("Total por", names(choices)[choices == input$agrupamento]),
            font = list(size = 16)
          ),
          xaxis = list(
            title = "Valor Total (R$)",
            tickformat = ",.0f"
          ),
          yaxis = list(
            title = names(choices)[choices == input$agrupamento]
          ),
          font = list(family = "Arial, sans-serif"),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE
        )

      return(p)
    })

    # Tabela de resumo
    output$tabela_resumo <- DT::renderDataTable({
      req(dados_resumo())

      df <- dados_resumo()

      # Formatação da tabela
      df_formatado <- df %>%
        mutate(
          valor_total = scales::dollar(valor_total, prefix = "R$ ", big.mark = ".", decimal.mark = ",")
        )

      DT::datatable(
        df_formatado,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "frtip",
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
          )
        ),
        rownames = FALSE,
        colnames = c(
          names(choices)[choices == input$agrupamento],
          "Valor Total",
          "Quantidade"
        )
      )
    })

    # Tabela de dados detalhados
    output$tabela_detalhada <- DT::renderDataTable({
      req(dados_filtrados())

      df <- dados_filtrados()

      # Selecionar colunas relevantes e formatar
      colunas_relevantes <- c(data, input$agrupamento, total)
      df_exibicao <- df %>%
        select(all_of(colunas_relevantes)) %>%
        arrange(desc(!!sym(data)))

      # Formatação de valores monetários
      if (total %in% names(df_exibicao)) {
        df_exibicao <- df_exibicao %>%
          mutate(!!sym(total) := scales::dollar(!!sym(total), prefix = "R$ ", big.mark = ".", decimal.mark = ","))
      }

      DT::datatable(
        df_exibicao,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "frtip",
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
          )
        ),
        rownames = FALSE,
        filter = "top"
      )
    })
  })
}

# =============================================================================
# MÓDULO: despesas
# Módulo principal para análise de despesas da empresa
# Utiliza submódulos para filtros e gráficos
# =============================================================================

#' @import shiny
#' @import dplyr
#' @import fs
#' @import here

#' Interface do usuário do módulo de despesas
#'
#' @param id Identificador único do módulo
#' @return Lista de elementos da interface
#' @export
m_despesas_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    title = "Análise de Despesas - Ampla",
    # Adicionar favicon
    tags$head(
      tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
      tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
    ),

    # Conteúdo principal
    div(
      style = "padding: 20px;",
      # Filtro de período
      wellPanel(
        h4("Filtros de Período"),
        sm_filtro_periodo_ui(ns("filtro_periodo"))
      ),
      # Gráficos
      sm_grafico_barras_empilhadas_ui(
        ns("grafico_despesas"),
        choices = list(
          "Empresa" = "empresa",
          "Centro" = "centro.negocio",
          "Credor" = "credor",
          "Agente Financeiro" = "agente.financeiro"
        ),
        total = "total.pago",
        data = "data.doc.pagto",
        comecoTitulo = "Despesas por"
      )
    ),

    # CSS mínimo para incorporação limpa
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #ffffff;
      }
      .container-fluid {
        padding: 0;
        max-width: 100%;
      }
    "))
  )
}

#' Servidor do módulo de despesas
#'
#' @param id Identificador único do módulo
#' @param dados_despesas Dados de despesas para análise
#' @return Função do servidor do módulo
#' @export
m_despesas_server <- function(id, dados_despesas) {
  moduleServer(id, function(input, output, session) {
    # Validação de dados
    dados_validados <- reactive({
      req(dados_despesas)

      # Verificar se as colunas necessárias existem
      colunas_necessarias <- c(
        "empresa", "centro.negocio", "credor",
        "agente.financeiro", "total.pago", "data.doc.pagto"
      )

      colunas_faltantes <- setdiff(colunas_necessarias, names(dados_despesas))

      if (length(colunas_faltantes) > 0) {
        showNotification(
          paste("Colunas faltantes nos dados:", paste(colunas_faltantes, collapse = ", ")),
          type = "warning"
        )
        return(NULL)
      }

      # Garantir que data.doc.pagto é do tipo Date
      dados_limpos <- dados_despesas %>%
        mutate(
          data.doc.pagto = as.Date(data.doc.pagto),
          total.pago = as.numeric(total.pago)
        ) %>%
        filter(
          !is.na(data.doc.pagto),
          !is.na(total.pago),
          total.pago > 0
        )

      return(dados_limpos)
    })

    # Inicializar submódulo de filtro de período
    periodo_filtro <- sm_filtro_periodo_server("filtro_periodo")

    # Inicializar submódulo de gráficos
    sm_grafico_barras_empilhadas_server(
      "grafico_despesas",
      dados = dados_validados,
      filtro_periodo = periodo_filtro$filtro_periodo,
      data_inicial = periodo_filtro$data_inicial,
      data_final = periodo_filtro$data_final,
      choices = list(
        "Empresa" = "empresa",
        "Centro" = "centro.negocio",
        "Credor" = "credor",
        "Agente Financeiro" = "agente.financeiro"
      ),
      total = "total.pago",
      data = "data.doc.pagto",
      comecoTitulo = "Despesas por"
    )
  })
}

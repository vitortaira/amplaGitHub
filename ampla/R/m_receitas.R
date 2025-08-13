# =============================================================================
# MÓDULO: receitas
# Módulo principal para análise de receitas da empresa
# Utiliza submódulos para filtros e gráficos
# =============================================================================

#' @import shiny
#' @import dplyr
#' @import fs
#' @import here

#' Interface do usuário do módulo de receitas
#'
#' @param id Identificador único do módulo
#' @return Lista de elementos da interface
#' @export
m_receitas_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    title = "Análise de Receitas - Ampla",
    # Adicionar favicon
    tags$head(
      tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
      tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
    ),

    # Conteúdo principal
    div(
      style = "padding: 20px;",
      # Gráficos
      sm_grafico_barras_empilhadas_ui(
        ns("grafico_receitas"),
        choices = list(
          "Empresa" = "empresa",
          "Empreendimento" = "empreendimento",
          "Agente" = "agente",
          "Elemento" = "elemento"
        ),
        total = "total",
        data = "data.pagamento",
        comecoTitulo = "Receitas por"
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

#' Servidor do módulo de receitas
#'
#' @param id Identificador único do módulo
#' @param dados_receitas Dados de receitas para análise
#' @return Função do servidor do módulo
#' @export
m_receitas_server <- function(id, dados_receitas) {
  moduleServer(id, function(input, output, session) {
    # Validação de dados
    dados_validados <- reactive({
      req(dados_receitas)

      # Verificar se as colunas necessárias existem
      colunas_necessarias <- c(
        "empresa", "empreendimento", "agente",
        "elemento", "total", "data.pagamento"
      )

      colunas_faltantes <- setdiff(colunas_necessarias, names(dados_receitas))

      if (length(colunas_faltantes) > 0) {
        showNotification(
          paste("Colunas faltantes nos dados:", paste(colunas_faltantes, collapse = ", ")),
          type = "warning"
        )
        return(NULL)
      }

      # Garantir que data.pagamento é do tipo Date
      dados_limpos <- dados_receitas %>%
        mutate(
          data.pagamento = as.Date(data.pagamento),
          total = as.numeric(total)
        ) %>%
        filter(
          !is.na(data.pagamento),
          !is.na(total),
          total > 0
        )

      return(dados_limpos)
    })

    # Inicializar submódulo de gráficos
    sm_grafico_barras_empilhadas_server(
      "grafico_receitas",
      dados = dados_validados,
      choices = list(
        "Empresa" = "empresa",
        "Empreendimento" = "empreendimento",
        "Agente" = "agente",
        "Elemento" = "elemento"
      ),
      total = "total",
      data = "data.pagamento",
      comecoTitulo = "Receitas por"
    )
  })
}

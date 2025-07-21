# =============================================================================
# MÓDULO: inicio
# Módulo principal da aplicação - página inicial com navegação
# Interface tipo índice de livro para acesso aos diferentes módulos
# =============================================================================

#' @import shiny

#' Interface do usuário do módulo de início
#'
#' @param id Identificador único do módulo
#' @return Lista de elementos da interface
#' @export
m_inicio_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    title = "Ampla Dashboard",
    # Adicionar favicon
    tags$head(
      tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
      tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
    ),

    # Container principal com estilo de livro
    div(
      style = "max-width: 800px; margin: 60px auto; padding: 40px; background-color: #ffffff; min-height: 80vh;",

      # Cabeçalho com marca da empresa
      div(
        style = "text-align: center; margin-bottom: 60px; padding-bottom: 30px; border-bottom: 1px solid #e9ecef;",
        img(src = "ampla_header.jpg", alt = "Ampla Incorporadora", style = "max-width: 60%; height: auto; margin-bottom: 20px;"),
        h1("Dashboard", style = "color: #2c3e50; font-weight: 300; font-size: 36px; margin: 20px 0 10px 0; letter-spacing: 1px;")
      ),

      # Índice/Sumário
      div(
        style = "margin: 40px 0;",

        # Seções principais
        div(
          style = "margin-left: 20px;",

          # Seção Setores
          div(
            style = "margin-bottom: 35px;",
            h3("Setores", style = "color: #2c3e50; font-weight: 500; font-size: 20px; margin-bottom: 20px;"),
            div(
              style = "margin-left: 30px;",
              # Subseção Financeiro
              div(
                style = "margin-bottom: 20px;",
                h4("Financeiro", style = "color: #34495e; font-weight: 400; font-size: 18px; margin-bottom: 15px;"),
                div(
                  style = "margin-left: 25px;",
                  div(
                    style = "margin-bottom: 8px;",
                    tags$a(
                      "Despesas",
                      href = "?page=despesas",
                      style = "color: #3498db; text-decoration: none; font-size: 16px; line-height: 1.6; display: block; padding: 8px 0; border-left: 3px solid transparent; padding-left: 15px; transition: all 0.3s ease;"
                    )
                  ),
                  div(
                    style = "margin-bottom: 8px;",
                    tags$a(
                      "Receitas",
                      href = "?page=receitas",
                      style = "color: #27ae60; text-decoration: none; font-size: 16px; line-height: 1.6; display: block; padding: 8px 0; border-left: 3px solid transparent; padding-left: 15px; transition: all 0.3s ease;"
                    )
                  )
                )
              )
            )
          ),

          # Seções futuras (placeholder)
          div(
            style = "margin-bottom: 25px; opacity: 0.5;",
            h3("Base de dados", style = "color: #2c3e50; font-weight: 500; font-size: 20px; margin-bottom: 20px;"),
            div(
              style = "margin-left: 30px;",
              div(
                style = "margin-bottom: 8px;",
                span("Buscar", style = "color: #95a5a6; font-size: 16px; line-height: 1.6; display: block; padding: 8px 0; padding-left: 15px; font-style: italic;"),
                span(" (em desenvolvimento)", style = "font-size: 12px; color: #bdc3c7;")
              ),
              div(
                style = "margin-bottom: 8px;",
                span("Cobertura temporal dos arquivos", style = "color: #95a5a6; font-size: 16px; line-height: 1.6; display: block; padding: 8px 0; padding-left: 15px; font-style: italic;"),
                span(" (em desenvolvimento)", style = "font-size: 12px; color: #bdc3c7;")
              ),
              div(
                style = "margin-bottom: 8px;",
                span("Mapa dos dados", style = "color: #95a5a6; font-size: 16px; line-height: 1.6; display: block; padding: 8px 0; padding-left: 15px; font-style: italic;"),
                span(" (em desenvolvimento)", style = "font-size: 12px; color: #bdc3c7;")
              )
            )
          )
        )
      )
    ),

    # CSS aprimorado para aparência de livro
    tags$style(HTML("
      body {
        margin: 0;
        font-family: 'Georgia', 'Times New Roman', serif;
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        min-height: 100vh;
      }
      .container-fluid { padding: 0; max-width: 100%; }

      /* Efeitos de hover para links */
      a[href*='page=']:hover {
        border-left: 3px solid #3498db !important;
        background-color: #f8f9fa !important;
        color: #2c3e50 !important;
        transform: translateX(5px);
      }

      a[href*='receitas']:hover {
        border-left: 3px solid #27ae60 !important;
      }

      /* Transições suaves */
      * {
        transition: all 0.3s ease;
      }

      /* Melhorias tipográficas */
      h1, h2, h3 {
        font-family: 'Georgia', 'Times New Roman', serif;
      }

      /* Estilo do código */
      code {
        font-family: 'Consolas', 'Monaco', 'Courier New', monospace !important;
        word-break: break-all;
        display: inline-block;
        max-width: 100%;
      }

      /* Design responsivo */
      @media (max-width: 768px) {
        .container-fluid > div {
          margin: 20px auto !important;
          padding: 20px !important;
        }

        h1 {
          font-size: 24px !important;
        }

        h2 {
          font-size: 20px !important;
        }

        h3 {
          font-size: 16px !important;
        }
      }
    "))
  )
}

#' Servidor do módulo de início
#'
#' @param id Identificador único do módulo
#' @return Função do servidor do módulo
#' @export
m_inicio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Este módulo é principalmente estático
    # Funcionalidades futuras podem ser adicionadas aqui
    # Como estatísticas gerais, notificações, etc.
  })
}

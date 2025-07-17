#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Get the page parameter from URL query string
  # This works with URLs like: /app?page=despesas or /app?page=receitas
  page <- parseQueryString(request$QUERY_STRING)$page

  # If no page specified, default to home
  if (is.null(page) || page == "") {
    page <- "home"
  }

  # Determine which page to show based on page parameter
  if (page == "despesas") {
    # Clean despesas page - no header, tabs on top
    fluidPage(
      title = "Despesas - Ampla",
      # Add favicon
      tags$head(
        tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
        tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
      ),
      div(
        style = "padding: 20px;",
        g_barras.empilhadas.mes_ui(
          "despesas_chart",
          choices = list("Empresa" = "empresa", "Centro" = "centro.negocio", "Credor" = "credor", "Agente Financeiro" = "agente.financeiro"),
          total = "total.pago",
          data = "data.doc.pagto",
          comeco.titulo = "Despesas por"
        )
      ),
      # Minimal CSS for clean embedding
      tags$style(HTML("
        body { margin: 0; padding: 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #ffffff; }
        .container-fluid { padding: 0; max-width: 100%; }
      "))
    )
  } else if (page == "receitas") {
    # Clean receitas page - no header, tabs on top
    fluidPage(
      title = "Receitas - Ampla",
      # Add favicon
      tags$head(
        tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
        tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
      ),
      div(
        style = "padding: 20px;",
        g_barras.empilhadas.mes_ui(
          "receitas_chart",
          choices = list("Empresa" = "empresa", "Empreendimento" = "empreendimento", "Agente" = "agente", "Elemento" = "elemento"),
          total = "total",
          data = "data.pagamento",
          comeco.titulo = "Receitas por"
        )
      ),
      # Minimal CSS for clean embedding
      tags$style(HTML("
        body { margin: 0; padding: 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #ffffff; }
        .container-fluid { padding: 0; max-width: 100%; }
      "))
    )
  } else {
    # Home page - minimalistic book index style
    fluidPage(
      title = "Ampla Dashboard",
      # Add favicon
      tags$head(
        tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
        tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
      ),

      # Main container with book-like styling
      div(
        style = "max-width: 800px; margin: 60px auto; padding: 40px; background-color: #ffffff; min-height: 80vh;",

        # Header with company branding
        div(
          style = "text-align: center; margin-bottom: 60px; padding-bottom: 30px; border-bottom: 1px solid #e9ecef;",
          img(src = "ampla_header.jpg", alt = "Ampla Incorporadora", style = "max-width: 60%; height: auto; margin-bottom: 20px;"),
          h1("Dashboard", style = "color: #2c3e50; font-weight: 300; font-size: 36px; margin: 20px 0 10px 0; letter-spacing: 1px;")
        ), # Index/Table of Contents
        div(
          style = "margin: 40px 0;",

          # Main sections
          div(
            style = "margin-left: 20px;",

            # Setores section
            div(
              style = "margin-bottom: 35px;",
              h3("Setores", style = "color: #2c3e50; font-weight: 500; font-size: 20px; margin-bottom: 20px;"),
              div(
                style = "margin-left: 30px;",
                # Financeiro subsection
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

            # Future sections (placeholder)
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

      # Enhanced CSS styling for book-like appearance
      tags$style(HTML("
        body {
          margin: 0;
          font-family: 'Georgia', 'Times New Roman', serif;
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          min-height: 100vh;
        }
        .container-fluid { padding: 0; max-width: 100%; }

        /* Hover effects for links */
        a[href*='page=']:hover {
          border-left: 3px solid #3498db !important;
          background-color: #f8f9fa !important;
          color: #2c3e50 !important;
          transform: translateX(5px);
        }

        a[href*='receitas']:hover {
          border-left: 3px solid #27ae60 !important;
        }

        /* Smooth transitions */
        * {
          transition: all 0.3s ease;
        }

        /* Typography enhancements */
        h1, h2, h3 {
          font-family: 'Georgia', 'Times New Roman', serif;
        }

        /* Code styling */
        code {
          font-family: 'Consolas', 'Monaco', 'Courier New', monospace !important;
          word-break: break-all;
          display: inline-block;
          max-width: 100%;
        }

        /* Responsive design */
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
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  tags$head(
    tags$title("Ampla - Financial Dashboard"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
    tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
  )
}

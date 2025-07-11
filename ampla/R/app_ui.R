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
    # Clean despesas page - no navigation, no header, just the chart
    fluidPage(
      title = "Despesas - Ampla",
      div(
        style = "padding: 20px;",
        g_barras.empilhadas.mes_ui(
          "despesas_chart",
          choices = list("Empresa" = "empresa", "Centro" = "centro.negocio", "Categoria" = "categoria"),
          total = "total.pago",
          data = "data.doc.pagto",
          comeco.titulo = "Despesas por"
        )
      ),
      # Minimal CSS for clean embedding
      tags$style(HTML("
        body { margin: 0; padding: 0; font-family: Arial, sans-serif; background-color: #ffffff; }
        .container-fluid { padding: 0; max-width: 100%; }
      "))
    )
  } else if (page == "receitas") {
    # Clean receitas page - no navigation, no header, just the chart
    fluidPage(
      title = "Receitas - Ampla",
      div(
        style = "padding: 20px;",
        g_barras.empilhadas.mes_ui(
          "receitas_chart",
          choices = list("Empresa" = "empresa", "Centro" = "centro.negocio", "Categoria" = "categoria"),
          total = "total.recebido",
          data = "data.recebimento",
          comeco.titulo = "Receitas por"
        )
      ),
      # Minimal CSS for clean embedding
      tags$style(HTML("
        body { margin: 0; padding: 0; font-family: Arial, sans-serif; background-color: #ffffff; }
        .container-fluid { padding: 0; max-width: 100%; }
      "))
    )
  } else {
    # Home page with navigation (default for root path)
    fluidPage(
      title = "Ampla Dashboard",

      # Header
      div(
        style = "padding: 15px; background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; margin-bottom: 20px;",
        div(
          style = "max-width: 1200px; margin: 0 auto;",
          h2("Ampla Dashboard", style = "color: #495057; margin: 0;")
        )
      ),

      # Main content area
      div(
        style = "max-width: 1200px; margin: 0 auto; padding: 0 15px;",
        div(
          style = "text-align: center; padding: 50px;",
          h1("Ampla Dashboard", style = "color: #495057;"),
          h3("Selecione uma seção:", style = "color: #6c757d; margin-bottom: 30px;"),
          div(
            style = "display: inline-block; margin: 20px;",
            tags$a("📊 Análise de Despesas",
              href = "?page=despesas",
              class = "btn btn-primary btn-lg",
              style = "margin: 10px; padding: 15px 30px; text-decoration: none;"
            )
          ),
          div(
            style = "display: inline-block; margin: 20px;",
            tags$a("💰 Análise de Receitas",
              href = "?page=receitas",
              class = "btn btn-success btn-lg",
              style = "margin: 10px; padding: 15px 30px; text-decoration: none;"
            )
          ),
          hr(style = "margin: 40px 0;"),
          div(
            style = "color: #6c757d; font-size: 14px;",
            p("Esta aplicação fornece análises interativas para embedding no Notion."),
            p("URLs diretas para embedding:"),
            div(
              style = "margin: 10px 0;",
              tags$code("https://vitortaira.shinyapps.io/ampla-dashboard/?page=despesas",
                style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0;"
              )
            ),
            div(
              style = "margin: 10px 0;",
              tags$code("https://vitortaira.shinyapps.io/ampla-dashboard/?page=receitas",
                style = "background-color: #f8f9fa; padding: 5px; border-radius: 3px; display: block; margin: 5px 0;"
              )
            )
          )
        )
      ),

      # CSS styling
      tags$style(HTML("
        .container-fluid { padding: 0; max-width: 100%; }
        body { margin: 0; font-family: Arial, sans-serif; background-color: #ffffff; }
        .btn { border-radius: 20px; padding: 8px 16px; font-weight: 500; text-decoration: none; }
        .btn-primary:hover { background-color: #0056b3; }
        .btn-success:hover { background-color: #1e7e34; }
        a.btn:hover { text-decoration: none; }
        code { background-color: #f8f9fa; padding: 2px 4px; border-radius: 3px; font-size: 12px; }
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
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "amplaApp"
    )
  )
}

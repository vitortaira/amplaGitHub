#' Interface do usuário da aplicação
#'
#' @param request Parâmetro interno para `{shiny}`.
#'     NÃO REMOVA.
#' @import shiny
#' @export
app_ui <- function(request) {
  # Adicionar recursos externos
  recursos_externos <- adicionarRecursosExternos()

  # Obter o parâmetro de página da string de consulta da URL
  # Funciona com URLs como: /app?page=despesas ou /app?page=receitas
  pagina <- parseQueryString(request$QUERY_STRING)$page

  # Se nenhuma página especificada, padrão para home
  if (is.null(pagina) || pagina == "") {
    pagina <- "home"
  }

  # Determinar qual página mostrar baseado no parâmetro de página
  conteudo_pagina <- if (pagina == "despesas") {
    # Módulo de despesas
    m_despesas_ui("modulo_despesas")
  } else if (pagina == "receitas") {
    # Módulo de receitas
    m_receitas_ui("modulo_receitas")
  } else {
    # Página inicial
    m_inicio_ui("modulo_inicio")
  }

  # Retornar o UI completo
  tagList(
    recursos_externos,
    conteudo_pagina
  )
}

#' Adicionar recursos externos à aplicação
#'
#' Esta função é utilizada internamente para adicionar recursos
#' externos dentro da aplicação Shiny.
#'
#' @import shiny
#' @noRd
adicionarRecursosExternos <- function() {
  tags$head(
    tags$title("Ampla - Financial Dashboard"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
    tags$link(rel = "shortcut icon", type = "image/jpeg", href = "ampla_icon.jpeg")
  )
}

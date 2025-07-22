# Exemplo: Usando o Notion MCP (API Oficial) no R
#
# Este script demonstra como usar a API oficial do Notion (MCP) no R.
# NÃO usa notionR. Usa httr e seu NOTION_SECRET do .Renviron.

#' Conectar com Notion API
#'
#' Esta função conecta com a API do Notion usando o token do ambiente
#' @param database_id ID do banco de dados Notion (opcional)
#' @param page_id ID da página Notion (opcional)
#' @return Lista com dados do Notion ou NULL se não configurado
#' @export
conectar_notion <- function(database_id = NULL, page_id = NULL) {
  # Verificar se o token está configurado
  tokenNotion <- Sys.getenv("NOTION_SECRET")
  if (tokenNotion == "") {
    warning("Variável de ambiente NOTION_SECRET não encontrada. Funcionalidade Notion não disponível.")
    return(NULL)
  }

  # Só carrega httr se necessário
  if (!requireNamespace("httr", quietly = TRUE)) {
    warning("Pacote httr não disponível. Instale com install.packages('httr')")
    return(NULL)
  }

  # Exemplo de consulta a banco de dados
  if (!is.null(database_id)) {
    urlConsulta <- paste0("https://api.notion.com/v1/databases/", database_id, "/query")

    resposta <- httr::POST(
      url = urlConsulta,
      httr::add_headers(
        Authorization = paste("Bearer", tokenNotion),
        "Notion-Version" = "2022-06-28",
        "Content-Type" = "application/json"
      ),
      body = "{}"
    )

    if (httr::http_error(resposta)) {
      warning("Falha na requisição da API: ", httr::content(resposta, "text"))
      return(NULL)
    }

    return(httr::content(resposta))
  }

  # Exemplo de criação de bloco
  if (!is.null(page_id)) {
    urlCriarBloco <- paste0("https://api.notion.com/v1/blocks/", page_id, "/children")

    corpoBloco <- list(
      children = list(
        list(
          object = "block",
          type = "paragraph",
          paragraph = list(
            rich_text = list(
              list(
                type = "text",
                text = list(content = "Olá do R usando Notion MCP!")
              )
            )
          )
        )
      )
    )

    respostaBloco <- httr::PATCH(
      url = urlCriarBloco,
      httr::add_headers(
        Authorization = paste("Bearer", tokenNotion),
        "Notion-Version" = "2022-06-28",
        "Content-Type" = "application/json"
      ),
      body = corpoBloco,
      encode = "json"
    )

    if (httr::http_error(respostaBloco)) {
      warning("Falha na criação do bloco: ", httr::content(respostaBloco, "text"))
      return(NULL)
    }

    return(httr::content(respostaBloco))
  }

  return(list(status = "Notion API disponível", token_presente = TRUE))
}

# Nota: Você deve reiniciar o R após editar .Renviron para que as mudanças tenham efeito.

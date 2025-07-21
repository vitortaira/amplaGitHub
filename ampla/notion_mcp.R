# Exemplo: Usando o Notion MCP (API Oficial) no R
#
# Este script demonstra como usar a API oficial do Notion (MCP) no R.
# NÃO usa notionR. Usa httr e seu NOTION_SECRET do .Renviron.
#
# 1. Obter o token do ambiente
tokenNotion <- Sys.getenv("NOTION_SECRET")
if (tokenNotion == "") stop("Variável de ambiente NOTION_SECRET não encontrada. Verifique seu arquivo .Renviron.")

library(httr)

# 2. Consultar um banco de dados Notion (substitua {database_id} pelo seu ID real)
# idBancoDados <- "seu_id_banco_dados_aqui"
# urlConsulta <- paste0("https://api.notion.com/v1/databases/", idBancoDados, "/query")
#
# resposta <- POST(
#   url = urlConsulta,
#   add_headers(
#     Authorization = paste("Bearer", tokenNotion),
#     "Notion-Version" = "2022-06-28",
#     "Content-Type" = "application/json"
#   ),
#   body = "{}"
# )
#
# if (http_error(resposta)) stop("Falha na requisição da API: ", content(resposta, "text"))
# cat("Resultado da consulta ao banco de dados:\n")
# print(content(resposta))

# 3. Criar um novo bloco (ex: adicionar um parágrafo a uma página)
# Substitua {page_id} pelo seu ID de página real
idPagina <- "231e63ea-90bb-81fc-b6c3-fda5b788488e"
urlCriarBloco <- paste0("https://api.notion.com/v1/blocks/", idPagina, "/children")

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
respostaBloco <- PATCH(
  url = urlCriarBloco,
  add_headers(
    Authorization = paste("Bearer", tokenNotion),
    "Notion-Version" = "2022-06-28",
    "Content-Type" = "application/json"
  ),
  body = corpoBloco,
  encode = "json"
)

if (http_error(respostaBloco)) stop("Falha na criação do bloco: ", content(respostaBloco, "text"))
cat("Resultado da criação do bloco:\n")
print(content(respostaBloco))

# Nota: Você deve reiniciar o R após editar .Renviron para que as mudanças tenham efeito.

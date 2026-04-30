#' @title Obter Caminho do Projeto por Alias
#'
#' @description
#' Retorna o caminho completo para uma pasta com base no alias fornecido.
#'
#' @param alias Um dos aliases predefinidos: "github", "rpackage", "ciweb",
#'   "cobranca", "extratos", "informakon".
#'
#' @return
#' Uma string com o caminho completo correspondente ao alias fornecido.
#'
#' @examples
#' \dontrun{
#' caminhos_pastas("github")
#' caminhos_pastas("ciweb")
#' }
#'
#' @importFrom stringi stri_c
#' @importFrom fs path
#' @importFrom here here
#'
#' @export
caminhos_pastas <- function(alias) {
  # Define a base path to reduce repetition and improve readability
  # For paths outside the project, consider using environment variables for portability
  caminho.base_c <- fs::path("C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA")

  caminhos_c <- list(
    # Paths inside the project, using here() for portability
    github = here::here(),
    informakon = here::here("dados", "Informakon"),
    rpackage = here::here("amplaRPackage"),
    shiny = here::here("amplaShiny"),
    shinydata = here::here("amplaShiny", "inst", "dados"),
    templates = here::here("templates"),
    testthat = here::here("amplaRPackage", "tests", "testthat"),

    # Paths outside the project. Consider using environment variables for better portability.
    ciweb = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Relatorios - CIWEB"
    ),
    cobertura = fs::path(
      caminho.base_c, "Controladoria - Documentos",
      "Extratos Originais - Grupo Ampla", "Consolidados", "Cobertura"
    ),
    cobranca = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Relatorios - Cobrança"
    ),
    dados = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Vitor", "Bases de dados"
    ),
    extratos = fs::path(
      caminho.base_c, "Controladoria - Documentos",
      "Extratos Originais - Grupo Ampla"
    ),
    financeiro = fs::path(
      caminho.base_c, "Financeiro - Documentos"
    ),
    fechamento = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Vitor", "Relatórios"
    ),
    fechamento_in = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Vitor",
      "Relatórios", "Inputs"
    ),
    fechamento_out = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Vitor",
      "Relatórios", "Outputs"
    ),
    temp = fs::path(
      "C:", "Users", "Ampla", "OneDrive - AMPLA INCORPORADORA LTDA",
      "Documentos", "temp"
    )
  )

  if (!alias %in% names(caminhos_c)) {
    stop("Alias inválido. Escolha um dos seguintes: ", paste(names(caminhos_c), collapse = ", "))
  }

  # Force UTF-8 encoding for the returned value
  return(enc2utf8(caminhos_c[[alias]]))
}

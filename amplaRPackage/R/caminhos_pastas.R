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
#'
#' @export
caminhos_pastas <- function(alias) {
  # Define a base path to reduce repetition and improve readability
  caminho.base_c <- fs::path("C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA")

  caminhos_c <- list(
    ciweb = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Relatorios - CIWEB"
    ),
    cobranca = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Relatorios - Cobrança"
    ),
    dados = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Dados", "Originais"
    ),
    extratos = fs::path(
      caminho.base_c, "Relatórios - Documentos", "Relatorios - Extratos"
    ),
    financeiro = fs::path(
      caminho.base_c, "Financeiro - Documentos"
    ),
    github = fs::path(
      caminho.base_c, "Controladoria - Documentos", "amplaGitHub"
    ),
    informakon = fs::path(
      caminho.base_c, "Financeiro - Documentos", "Informakon"
    ),
    rpackage = fs::path(
      caminho.base_c,
      "Controladoria - Documentos", "amplaGitHub", "amplaRPackage"
    ),
    shiny = fs::path(
      caminho.base_c, "Controladoria - Documentos", "amplaGitHub", "amplaShiny"
    ),
    shinydata = fs::path(
      caminho.base_c,
      "Controladoria - Documentos", "amplaGitHub", "amplaShiny", "dados"
    ),
    temp = fs::path(
      "C:", "Users", "Ampla", "OneDrive - AMPLA INCORPORADORA LTDA",
      "Documentos", "temp"
    ),
    templates = fs::path(
      caminho.base_c, "Controladoria - Documentos", "amplaGitHub", "templates"
    ),
    testthat = fs::path(
      caminho.base_c,
      "Controladoria - Documentos", "amplaGitHub", "amplaRPackage", "tests",
      "testthat"
    )
  )

  if (!alias %in% names(caminhos_c)) {
    stop("Alias inválido. Escolha um dos seguintes: ", paste(names(caminhos_c), collapse = ", "))
  }

  # Force UTF-8 encoding for the returned value
  return(enc2utf8(caminhos_c[[alias]]))
}

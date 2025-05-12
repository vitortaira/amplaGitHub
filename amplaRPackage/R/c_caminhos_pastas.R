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
#' c_caminhos_pastas("github")
#' c_caminhos_pastas("ciweb")
#' }
#'
#' @importFrom stringi stri_c
#'
#' @export
c_caminhos_pastas <- function(alias) {
  caminhos_c <- list(
    ciweb = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - CIWEB",
    cobranca = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - Cobrança",
    dados = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Dados/Originais",
    extratos = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - Extratos",
    github = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub",
    informakon = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Financeiro - Documentos/Informakon",
    rpackage = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaRPackage",
    shiny = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaShiny",
    shinydata = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaShiny/dados",
    temp = "C:/Users/Ampla/OneDrive - AMPLA INCORPORADORA LTDA/Documentos/temp",
    templates = "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/templates"
  )

  if (!alias %in% names(caminhos_c)) {
    stop("Alias inválido. Escolha um dos seguintes: ", paste(names(caminhos_c), collapse = ", "))
  }

  # Force UTF-8 encoding for the returned value
  return(enc2utf8(caminhos_c[[alias]]))
}

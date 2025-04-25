#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_dados.r
#' @title Consolidação de Dados: CEF e Informakon
#'
#' @description
#' A função **e_dados()** consolida dados de diferentes fontes (CEF, Informakon)
#' em um único objeto, facilitando o acesso e manipulação.
#'
#' @details
#' Internamente chama \code{\link{e_cef}} e \code{\link{e_ik}} para obter
#' informações relacionadas a relatórios, extratos e dados específicos.
#'
#' @return
#' Retorna uma lista contendo duas entradas:
#' - \code{cef}: Resultado da função \code{e_cef()}
#' - \code{ik}: Resultado da função \code{e_ik()}
#'
#' @examples
#' \dontrun{
#' # Exemplo simples de chamada
#' dados <- e_dados()
#' str(dados)
#' s
#' }
#'
#' @seealso
#' \code{\link{e_cef}}, \code{\link{e_ik}}
#'
#' @export
e_dados <- function() {
  dados_l <- list(
    "cef" = e_cef(),
    "ik"  = e_ik()
  )
  return(dados_l)
}

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
#' @importFrom stringi stri_trans_nfc
#'
#' @export
e_dados <- function() {
  normalize_names <- function(obj) {
    # If the object has names, normalize them
    if (!is.null(names(obj))) {
      names(obj) <- stri_trans_nfc(enc2utf8(names(obj)))
    }
    # If it's a list (but not a data.frame), process each element recursively.
    if (is.list(obj) && !is.data.frame(obj)) {
      obj <- lapply(obj, normalize_names)
    }
    # For data.frames, only fix the names; leave content unchanged.
    obj
  }
  dados_l <- list(
    "cef" = e_cef(),
    "ik"  = e_ik()
  )
  normalize_names(dados_l)
}

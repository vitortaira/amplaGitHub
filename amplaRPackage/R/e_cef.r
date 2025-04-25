#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_cef.r
#' @title Consolidação de Dados dos Relatórios CEF
#'
#' @description
#' A função **e_cef()** consolida e retorna dados de diversos relatórios da CEF,
#' como CMF_CN, ECN, EPR e Extratos, num só objeto (lista).
#'
#' @details
#' Internamente chama as funções \code{e_cef_cmfcns()}, \code{e_cef_ecns()},
#' \code{e_cef_eprs()} e \code{e_cef_extratos()}, reunindo tudo num único objeto.
#'
#' @return
#' Retorna uma lista contendo todos os dados extraídos das várias fontes:
#' CMF_CN, ECN, EPR e Extratos.
#'
#' @examples
#' \dontrun{
#' # Utilizando as configurações padrão
#' lista_cef <- e_cef()
#' str(lista_cef)
#' }
#'
#' @importFrom magrittr %>%
#' @export
e_cef <- function() {
  dados.cef_l <- list(
    cmfcn  = e_cef_cmfcns(),
    ecnE   = e_cef_ecns()$Empreendimento,
    ecnPJ  = e_cef_ecns()$Emprestimo,
    ecnU   = e_cef_ecns()$Unidades,
    ecnC   = e_cef_ecns()$Consolidado,
    epr    = e_cef_eprs(),
    extCEF = e_cef_extratos()
  )
  return(dados.cef_l)
}

#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_cef.R
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
  plan(multisession)
  dcd_f <- future(
    {
      e_cef_dcds_resumo()
    },
    seed = TRUE
  )
  ecn.e_f <- future(
    {
      e_cef_ecns()$Empreendimento
    },
    seed = TRUE
  )
  ecn.pj_f <- future(
    {
      e_cef_ecns()$Emprestimo
    },
    seed = TRUE
  )
  ecn.u_f <- future(
    {
      e_cef_ecns()$Unidades
    },
    seed = TRUE
  )
  ecn.c_f <- future(
    {
      e_cef_ecns()$Consolidado
    },
    seed = TRUE
  )
  extcef_f <- future(
    {
      e_cef_extratos()
    },
    seed = TRUE
  )
  dados.cef_l <- list(
    cmfcn = e_cef_cmfcns(),
    dcd = value(dcd_f),
    ecn_e = value(ecn.e_f),
    ecn_pj = value(ecn.pj_f),
    ecn_u = value(ecn.u_f),
    ecn_c = value(ecn.c_f),
    epr = e_cef_eprs(),
    extcef = value(extcef_f)
  )
  return(dados.cef_l)
}

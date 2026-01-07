# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik.R

#' @title Consolidação de Dados dos Relatórios Informakon
#'
#' @description
#' A função **e_ik()** consolida e retorna dados de diversos relatórios
#' (Despesas e Contas a Receber) da Informakon em um só objeto (lista).
#'
#' @details
#' Internamente chama as funções \code{e_ik_desp()} e \code{e_ik_cr()} e
#' reúne tudo em um único objeto.
#'
#' @return
#' Retorna uma lista contendo todos os dados extraídos das várias fontes:
#' Despesas e Contas a Receber.
#'
#' @examples
#' \dontrun{
#' # Utilizando as configurações padrão
#' lista_ik <- e_ik()
#' str(lista_ik)
#' }
#'
#' @importFrom magrittr %>%
#' @export
e_ik <- function() {
  dados.ik_l <- list(
    contr = e_ik_contrs(),
    cr = e_ik_cr(),
    desp = e_ik_desp(),
    inad = e_ik_inads()
  )
  return(dados.ik_l)
}

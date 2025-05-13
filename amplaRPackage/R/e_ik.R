# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik.R

#' @title Consolidação de Dados dos Relatórios Informakon
#'
#' @description
#' A função **e_ik()** consolida e retorna dados de diversos relatórios
#' (Despesas e Receitas) da Informakon em um só objeto (lista).
#'
#' @details
#' Internamente chama as funções \code{e_ik_desp()} e \code{e_ik_rec()} e
#' reúne tudo em um único objeto.
#'
#' @return
#' Retorna uma lista contendo todos os dados extraídos das várias fontes:
#' Despesas e Receitas.
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
    desp = e_ik_desp(),
    inad = e_ik_inads(),
    rec = e_ik_rec()
  )
  return(dados.ik_l)
}

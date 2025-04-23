#' @title Consolidação dos dados dos relatórios CMF_CN
#'
#' @description
#' A função **e_cef_cmfcns** consolida os dados dos relatórios CMF_CN
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos PDF que
#' contenham "MOV_FINANC_CN" no nome. Para cada arquivo encontrado,
#' chama a função \code{e_cef_cmfcn} para realizar a extração dos dados e,
#' posteriormente, consolida os resultados em um tibble.
#'
#' @return
#' Retorna um tibble consolidado com os dados extraídos dos arquivos CMF_CN.
#'
#' @examples
#' \dontrun{
#' f_caminho.pasta.ciweb_c <- "caminho/para/a/pasta/Relatorios - CIWEB"
#' resultado <- e_cef_cmfcns(f_caminho.pasta.ciweb_c)
#' print(resultado)
#' }
#'
#' @importFrom purrr keep
#' @importFrom dplyr mutate rename
#' @importFrom stringr str_detect str_sub
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#'
#' @export

e_cef_cmfcns <- function(f_caminho.pasta.ciweb_c) {
  # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
  caminhos.cmfcns_c <-
    list.files(
      f_caminho.pasta.ciweb_c,
      full.names = TRUE, recursive = T
    ) %>%
    keep(~ str_detect(.x, "MOV_FINANC_CN.pdf"))
  cmfcns_l <- list()
  cmfcns_t <- data.frame()
  for (
    i_caminho.cmfcn_c in caminhos.cmfcns_c
  ) {
    cmfcns_l[[i_caminho.cmfcn_c]] <-
      e_cef_cmfcn(i_caminho.cmfcn_c)
    cmfcns_t <-
      bind_rows(cmfcns_t, cmfcns_l[[i_caminho.cmfcn_c]])
  }
  cmfcns_t %<>%
    mutate(Contrato_6 = CONTRATO %>% str_sub(-6, -1)) %>%
    rename(
      `Data de movimento` = "DT. REMES.",
      Valor = VALOR
    ) %>%
    as_tibble()
  return(cmfcns_t)
}

e_cef_cmfcns <- function(f_caminho.pasta.ciweb_c) {
  # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
  caminhos.cmfcns_c <-
    list.files(
      f_caminho.pasta.ciweb_c,
      full.names = TRUE, recursive = T
    ) %>%
    keep(~ str_detect(.x, "MOV_FINANC_CN.pdf"))
  cmfcns_l <- list()
  cmfcns_t <- data.frame()
  for (
    i_caminho.cmfcn_c in caminhos.cmfcns_c
  ) {
    cmfcns_l[[i_caminho.cmfcn_c]] <-
      e_cef_cmfcn(i_caminho.cmfcn_c)
    cmfcns_t <-
      bind_rows(cmfcns_t, cmfcns_l[[i_caminho.cmfcn_c]])
  }
  cmfcns_t %<>%
    mutate(Contrato_6 = CONTRATO %>% str_sub(-6, -1)) %>%
    rename(
      `Data de movimento` = "DT. REMES.",
      Valor = VALOR
    ) %>%
    as_tibble()
  return(cmfcns_t)
}

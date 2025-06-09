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

e_cef_cmfcns <- function(f_caminho.pasta.ciweb_c = c_caminhos_pastas("ciweb")) {
  # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
  caminhos.cmfcn_c <-
    dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE, type = "file") %>%
    keep(~ str_detect(.x, "MOV_FINANC_CN.pdf"))
  # Identifica o arquivo mais recente de cada empreendimento
  contratos.empreendimentos.12.primeiros_c <-
    caminhos.cmfcn_c %>%
    str_extract("\\d{12}") %>%
    unique()
  caminhos.cmfcn.recentes_c <- map(
    contratos.empreendimentos.12.primeiros_c,
    ~ {
      i <- caminhos.cmfcn_c %>%
        str_subset(.x) %>%
        path_file() %>%
        str_extract("^\\d{8}") %>%
        ymd() %>%
        which.max()
      caminhos.cmfcn_c[i]
    }
  ) %>%
    flatten_chr() %>%
    unname()
  #
  cmfcns_l <- list()
  cmfcns_t <- data.frame()
  for (i_caminho.cmfcn_c in caminhos.cmfcn.recentes_c) {
    cmfcns_l[[i_caminho.cmfcn_c]] <-
      e_cef_cmfcn(i_caminho.cmfcn_c)
    cmfcns_t <-
      bind_rows(cmfcns_t, cmfcns_l[[i_caminho.cmfcn_c]])
  }
  cmfcns_t %<>%
    mutate(
      contrato.6 = contrato %>% str_sub(-6, -1),
      arquivo.tabela.tipo = "cmfcn",
      arquivo.tipo = "cmfcn",
      arquivo.fonte = "cef"
    ) %>%
    rename(
      data.movimento = data.remessa,
    ) %>%
    as_tibble() %>%
    select(
      contrato, data.lancamento, data.movimento, lancamentos, np,
      `conta.sidec/nsgd`, valor, situacao, mot, contrato.6, arquivo,
      arquivo.tipo, arquivo.tabela.tipo, arquivo.fonte
    )
  return(cmfcns_t)
}

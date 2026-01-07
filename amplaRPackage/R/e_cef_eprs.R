#' @title Extração e Consolidação de Relatórios EPR da CEF
#'
#' @description
#' A função **e_cef_eprs()** extrai e consolida os dados dos relatórios EPR da CEF
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @details
#' O processo busca todos os arquivos EPR com extensão \code{.pdf}, realiza a extração
#' dos dados e consolida em um único \code{tibble}.
#'
#' @return Retorna um \code{tibble} contendo as informações extraídas dos relatórios EPR.
#'
#' @examples
#' \dontrun{
#' e_cef_eprs()
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect str_ends
#' @importFrom purrr keep
#' @importFrom dplyr bind_rows distinct
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @export
e_cef_eprs <- function() {
  # Consolida os dados dos relatórios EPR da CEF na pasta "Relatorios - CIWEB"
  caminhos.epr_c <-
    dir_ls(caminhos_pastas("ciweb"), recurse = TRUE, type = "file") %>%
    keep(~ str_ends(.x, "(?i)contratos_empreen(d)?\\.pdf"))
  # Identifica o arquivo mais recente de cada empreendimento
  caminhos.epr.recentes_c <- tibble::tibble(caminho = caminhos.epr_c) %>%
    mutate(
      contrato = stringr::str_extract(caminho, "\\d{12}"),
      data_arquivo = lubridate::ymd(stringr::str_extract(fs::path_file(caminho), "^\\d{8}"))
    ) %>%
    # Remove arquivos onde o contrato ou a data não puderam ser extraídos
    dplyr::filter(!is.na(contrato) & !is.na(data_arquivo)) %>%
    # Para cada contrato, encontra o arquivo mais recente
    dplyr::group_by(contrato) %>%
    dplyr::slice_max(order_by = data_arquivo, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::pull(caminho)
  eprs_l <- list()
  eprs_t <- data.frame()
  for (
    i_caminho.cef.epr_c in caminhos.epr.recentes_c
  ) {
    eprs_l[[i_caminho.cef.epr_c]] <-
      e_cef_epr(i_caminho.cef.epr_c)
    eprs_t <-
      bind_rows(eprs_t, eprs_l[[i_caminho.cef.epr_c]])
  }
  eprs_t %<>% distinct(across(-arquivo), .keep_all = TRUE) %>%
    as_tibble() %>%
    mutate(
      arquivo.tabela.tipo = "epr",
      arquivo.tipo = "epr",
      arquivo.fonte = "cef"
    )
  return(eprs_t)
}

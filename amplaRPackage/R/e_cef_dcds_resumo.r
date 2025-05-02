#' @title Extração e Consolidação de Relatórios DCDS da CEF
#'
#' @description
#' A função **e_cef_dcds_resumo()** consolida os dados de relatórios DCDS na pasta
#' "Relatorios - CIWEB", retornando em um tibble as informações extraídas.
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'   O valor padrão é \code{c_caminhos_pastas("ciweb")}.
#'
#' @details
#' Identifica todos os arquivos DCDS com final "demonst_cronograma.pdf" e
#' aplica \code{\link{e_cef_dcd_resumo}} em cada um deles para extrair os dados.
#'
#' @return Um tibble com os dados compilados dos relatórios DCDS da CEF.
#'
#' @examples
#' \dontrun{
#' dcds_resumo <- e_cef_dcds_resumo()
#' head(dcds_resumo)
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_ends
#' @importFrom purrr keep map_dfr
#' @export
e_cef_dcds_resumo <- function(
    f_caminho.pasta.ciweb_c = c_caminhos_pastas("ciweb")) {
  # Consolida os dados dos relatórios DCDS da CEF na pasta "Relatorios - CIWEB"
  caminhos.dcds_c <-
    dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE, type = "file") %>%
    keep(~ str_ends(.x, "(?i)demonst_cronograma.pdf"))

  dcds.resumo_t <-
    caminhos.dcds_c %>%
    map_dfr(~ e_cef_dcd_resumo(.x)) %>%
    mutate(
      Arquivo_tipo = "dcd",
      Arquivo_tipo_tabela = "dcd"
    ) %>%
    return(dcds.resumo_t)
}

#' @title Classificação do subtipo dos extratos bancários do Itaú
#'
#' @description
#' A função **c_ita_xita** classifica o subtipo dos arquivos de extratos bancários
#' do Itaú com base na extensão do arquivo. A classificação é simples:
#' arquivos Excel (.xlsx) são classificados como "xita2" e arquivos PDF (.pdf)
#' são classificados como "xita1".
#'
#' @param f_caminho.arquivo_c Caminho(s) do(s) arquivo(s) a ser(em) classificado(s).
#'   Pode ser um único caminho (string) ou um vetor de caminhos.
#'
#' @details
#' A função determina o subtipo baseado na extensão do arquivo:
#' \itemize{
#'   \item **xita1**: Arquivos com extensão .pdf (PDF)
#'   \item **xita2**: Arquivos com extensão .xlsx (Excel)
#'   \item **NA**: Arquivos com outras extensões ou arquivos inexistentes
#' }
#'
#' A função é vetorizada, ou seja, pode processar múltiplos arquivos
#' simultaneamente retornando um vetor de classificações.
#'
#' @return
#' Retorna um vetor de caracteres com as classificações dos subtipos:
#' \itemize{
#'   \item "xita1" para arquivos PDF (.pdf)
#'   \item "xita2" para arquivos Excel (.xlsx)
#'   \item NA para outras extensões ou arquivos inexistentes
#' }
#'
#' @examples
#' \dontrun{
#' # Classificar um único arquivo
#' c_ita_xita("extrato_janeiro.xlsx") # Retorna "xita2"
#' c_ita_xita("extrato_fevereiro.pdf") # Retorna "xita1"
#'
#' # Classificar múltiplos arquivos
#' arquivos <- c("extrato1.xlsx", "extrato2.pdf", "extrato3.txt")
#' c_ita_xita(arquivos) # Retorna c("xita1", "xita2", NA)
#' }
#'
#' @seealso
#' \code{\link{c_cef_xcef}} para classificação de extratos da CEF,
#' \code{\link{e_ita_xita}} para extração de dados de extratos do Itaú.
#'
#' @importFrom fs path_ext
#' @importFrom purrr map_chr
#' @importFrom dplyr case_when
#'
#' @export

c_ita_xita <- function(f_caminho.arquivo_c) {
  # Vetorizar a função para tratar múltiplos caminhos de arquivo
  if (length(f_caminho.arquivo_c) > 1) {
    return(purrr::map_chr(f_caminho.arquivo_c, c_ita_xita))
  }

  # Extrair a extensão do arquivo
  extensao <- fs::path_ext(f_caminho.arquivo_c)

  # Classificar baseado na extensão
  resultado <- dplyr::case_when(
    tolower(extensao) == "xlsx" ~ "xita2",
    tolower(extensao) == "pdf" ~ "xita1",
    TRUE ~ NA_character_
  )

  return(resultado)
}

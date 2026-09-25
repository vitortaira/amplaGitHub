#' Localiza o arquivo Informakon mais recente por prefixo
#'
#' Helper interno (não exportado) usado por \code{e_ik_fcs} e \code{e_ik_cmfs}
#' para encontrar, na pasta de inputs, o arquivo mais recente cujo nome começa
#' com \code{<prefixo>-}. A data no nome pode ser \code{AAAA_MM} (mensal) ou um
#' intervalo \code{AAAA_MM_DD-AAAA_MM_DD}; usa-se a última data encontrada no
#' nome para determinar o mais recente.
#'
#' @param caminho.pasta.inputs_c String do caminho da pasta de inputs.
#' @param prefixo_c String com o prefixo do arquivo (ex.: \code{"fc"},
#'   \code{"cmf"}).
#'
#' @return String com o caminho do arquivo mais recente.
#'
#' @keywords internal
#' @noRd
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect str_extract_all str_count
obterArquivoIkMaisRecente <- function(caminho.pasta.inputs_c, prefixo_c) {
  if (!dir.exists(caminho.pasta.inputs_c)) {
    stop("A pasta de inputs não foi encontrada: ", caminho.pasta.inputs_c)
  }

  caminhos_c <- fs::dir_ls(
    caminho.pasta.inputs_c,
    recurse = TRUE,
    type = "file",
    regexp = paste0("(^|/)", prefixo_c, "-.*\\.xlsx$")
  )

  if (length(caminhos_c) == 0) {
    stop(
      "Nenhum arquivo '", prefixo_c, "-*.xlsx' encontrado na pasta de inputs."
    )
  }

  # Extrai a última data (AAAA_MM ou AAAA_MM_DD) do nome de cada arquivo
  datasFinais_d <- vapply(
    caminhos_c,
    function(caminho_c) {
      tokens_c <- stringr::str_extract_all(
        basename(caminho_c), "\\d{4}_\\d{2}(?:_\\d{2})?"
      )[[1]]
      if (length(tokens_c) == 0) {
        return(as.Date(NA))
      }
      ultimo_c <- tokens_c[length(tokens_c)]
      if (stringr::str_count(ultimo_c, "_") == 2) {
        as.Date(ultimo_c, format = "%Y_%m_%d")
      } else {
        as.Date(paste0(ultimo_c, "_01"), format = "%Y_%m_%d")
      }
    },
    FUN.VALUE = as.Date(NA)
  )

  caminhos_c[which.max(datasFinais_d)]
}

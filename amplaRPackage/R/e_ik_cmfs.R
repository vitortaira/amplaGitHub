#' @title Extração do relatório CMF (Conta Movimento Financeiro) mais recente
#'
#' @description
#' A função e_ik_cmfs() localiza o arquivo de Conta Movimento Financeiro (CMF)
#' mais recente na pasta de inputs do fechamento e o extrai chamando
#' \code{\link{e_ik_cmf}}.
#'
#' @details
#' Os arquivos seguem o padrão \code{cmf-<data>.xlsx}, onde \code{<data>} pode
#' ser \code{AAAA_MM} (mensal) ou um intervalo \code{AAAA_MM_DD-AAAA_MM_DD}.
#' O arquivo mais recente é determinado pela última data presente no nome.
#'
#' @param caminho.pasta.inputs_c String do caminho da pasta de inputs.
#'   Valor padrão: \code{caminhos_pastas("fechamento_in")}.
#'
#' @return \code{tibble} conforme retornado por \code{\link{e_ik_cmf}}.
#'
#' @examples
#' \dontrun{
#' cmf_t <- e_ik_cmfs()
#' }
#'
#' @seealso \code{\link{e_ik_cmf}}
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_extract_all str_count
#' @export
e_ik_cmfs <- function(
    caminho.pasta.inputs_c = caminhos_pastas("fechamento_in")) {
  caminho.arquivo.cmf_c <- obterArquivoIkMaisRecente(
    caminho.pasta.inputs_c = caminho.pasta.inputs_c,
    prefixo_c = "cmf"
  )

  message("Extraindo arquivo: ", basename(caminho.arquivo.cmf_c))

  e_ik_cmf(caminho.arquivo.cmf_c)
}

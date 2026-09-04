#' @title Extração do relatório FC (Fechamento de Caixa) mais recente
#'
#' @description
#' A função e_ik_fcs() localiza o arquivo de Fechamento de Caixa (FC) mais
#' recente na pasta de inputs do fechamento e o extrai chamando
#' \code{\link{e_ik_fc}}.
#'
#' @details
#' Os arquivos seguem o padrão \code{fc-<data>.xlsx}, onde \code{<data>} pode
#' ser \code{AAAA_MM} (mensal) ou um intervalo \code{AAAA_MM_DD-AAAA_MM_DD}.
#' O arquivo mais recente é determinado pela última data presente no nome.
#'
#' @param caminho.pasta.inputs_c String do caminho da pasta de inputs.
#'   Valor padrão: \code{caminhos_pastas("fechamento_in")}.
#'
#' @return \code{tibble} conforme retornado por \code{\link{e_ik_fc}}.
#'
#' @examples
#' \dontrun{
#' fc_t <- e_ik_fcs()
#' }
#'
#' @seealso \code{\link{e_ik_fc}}
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_extract_all str_count
#' @export
e_ik_fcs <- function(
    caminho.pasta.inputs_c = caminhos_pastas("fechamento_in")) {
  caminho.arquivo.fc_c <- obterArquivoIkMaisRecente(
    caminho.pasta.inputs_c = caminho.pasta.inputs_c,
    prefixo_c = "fc"
  )

  message("Extraindo arquivo: ", basename(caminho.arquivo.fc_c))

  e_ik_fc(caminho.arquivo.fc_c)
}

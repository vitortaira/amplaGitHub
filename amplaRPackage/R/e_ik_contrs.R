#' @title Extração do Contrato Informakon Mais Recente
#'
#' @description
#' A função e_ik_contrs() identifica e extrai apenas o arquivo de contrato
#' (contr) mais recente na pasta especificada.
#'
#' @param caminho.pasta.inputs_c String do caminho da pasta "Inputs".
#'   Valor padrão: \code{"C:\\Users\\Ampla\\AMPLA INCORPORADORA LTDA\\Relatórios - Documentos\\Dados\\Para o Soares\\Inputs"}.
#'
#' @return Data frame com dados dos contratos do arquivo mais recente.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' contratos_df <- e_ik_contrs()
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect str_extract
#' @importFrom dplyr arrange desc
#' @export
e_ik_contrs <- function(
    caminho.pasta.inputs_c = "C:\\Users\\Ampla\\AMPLA INCORPORADORA LTDA\\Relatórios - Documentos\\Dados\\Para o Soares\\Inputs") {
  # Função interna para buscar o arquivo de contratos mais recente
  obterCaminhoContrMaisRecente <- function() {
    if (!dir.exists(caminho.pasta.inputs_c)) {
      stop("A pasta 'Inputs' não foi encontrada: ", caminho.pasta.inputs_c)
    }

    # Busca todos os arquivos contr recursivamente
    caminhos.contr_c <- fs::dir_ls(
      caminho.pasta.inputs_c,
      recurse = TRUE,
      type = "file",
      regexp = "contr-.*\\.xlsx$"
    )

    if (length(caminhos.contr_c) == 0) {
      stop("Nenhum arquivo contr encontrado na pasta Inputs.")
    }

    # Extrai as datas dos nomes dos arquivos (formato: contr-YYYY_MM_DD.xlsx)
    datas.por.arquivo_d <- sapply(caminhos.contr_c, function(caminho_c) {
      basename(caminho_c) %>%
        stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") %>%
        as.Date(format = "%Y_%m_%d")
    })

    # Encontra o índice do arquivo mais recente
    indice.recente_i <- which.max(datas.por.arquivo_d)

    # Retorna o caminho do arquivo mais recente
    caminhos.contr_c[indice.recente_i]
  }

  # Obtém o caminho do arquivo mais recente
  caminho.arquivo.contr_c <- obterCaminhoContrMaisRecente()

  # Mensagem informativa
  message("Extraindo arquivo: ", basename(caminho.arquivo.contr_c))

  # Extrai os dados usando a função e_ik_contr
  contr_t <- e_ik_contr(caminho.arquivo.contr_c)

  return(contr_t)
}

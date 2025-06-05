# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik_desp.R
#' @title Extração de Despesas Informakon
#'
#' @description
#' A função e_ik_desp() extrai os dados de despesas dos arquivos na pasta
#' "informakon", preenche-os em uma planilha xlsx (opcional) e os retorna em
#' um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{c_caminhos_pastas("informakon")}.
#' @param xlsx Logical. Se \code{TRUE}, cria um arquivo xlsx com os dados extraídos.
#'   Valor padrão: \code{FALSE}.
#'
#' @return Data frame com dados das despesas consolidadas.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' despesas_df <- e_ik_desp()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub str_detect
#' @importFrom lubridate floor_date
#' @export
e_ik_desp <- function(
    f_caminho.pasta.ik_c = c_caminhos_pastas("informakon"),
    xlsx = FALSE) {
  # Função interna para buscar o arquivo de despesas mais recente
  obter_caminho_despesas <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }
    # Busca arquivos que começam com "despesas_" e ignora os "Informakon"
    caminhos_desp <- dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_desp <- caminhos_desp[
      basename(caminhos_desp) %>% str_detect("^despesas") &
        !basename(caminhos_desp) %>% str_detect("^Informakon")
    ]
    if (length(caminhos_desp) == 0) {
      stop("Nenhum arquivo de despesas encontrado na pasta informakon.")
    }
    # Determina a data final (YYYYMMDD) mais recente
    data_final_por_arquivo <- sapply(caminhos_desp, function(path) {
      basename(path) %>%
        str_extract("[^_]+$") %>%
        str_remove("\\.xlsx$") %>%
        as.Date(format = "%Y%m%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_desp[indice_recente]
  }

  # Carrega o arquivo de despesas mais recente
  caminho_arquivo_despesas <- obter_caminho_despesas()
  despesas_df <- read_excel(caminho_arquivo_despesas) %>%
    mutate(
      `a/c` = as.character(`a/c`),
      `Acréscimos` = as.numeric(`Acréscimos`),
      `Agente Financeiro` = as.character(`Agente Financeiro`),
      `Centro de Negócio` = as.character(`Centro de Negócio`),
      `Cod. Centro` = as.character(`Cod. Centro`),
      Credor = as.character(Credor),
      `Data Doc Pagto` = as.Date(`Data Doc Pagto`),
      `Data Liberação` = as.Date(`Data Liberação`),
      `Data Vencimento` = as.Date(`Data Vencimento`),
      `Data Vencimento Origem` = as.Date(`Data Vencimento Origem`),
      Descontos = as.numeric(Descontos),
      `Descontos Adiant.` = as.numeric(`Descontos Adiant.`),
      Documento = as.character(Documento),
      Empresa = `Cod. Centro` %>% str_sub(1, 3),
      Encargos = as.numeric(Encargos),
      `Mês` = floor_date(`Data Doc Pagto`, "month"),
      Multa = as.numeric(Multa),
      `N° Conta` = as.character(`N° Conta`),
      `Nº Entrada` = as.integer(`Nº Entrada`),
      `Observação` = as.character(`Observação`),
      Parcela = as.character(Parcela),
      `Total Pago` = as.numeric(`Total Pago`),
      `Valor Titulo` = as.numeric(`Valor Titulo`),
      Arquivo = caminho_arquivo_despesas,
      arquivo.tabela.tipo = "desp",
      Arquivo_tipo = "desp",
      Arquivo_fonte = "ik"
    ) %>%
    select(
      `Data Doc Pagto`, `Mês`, `Data Liberação`, Credor, Empresa,
      `Cod. Centro`, `Centro de Negócio`, `Agente Financeiro`, `N° Conta`,
      `Nº Entrada`, Documento, Parcela, `Data Vencimento`,
      `Data Vencimento Origem`, `Valor Titulo`, `Acréscimos`, Descontos,
      Encargos, `Descontos Adiant.`, Multa, `Total Pago`, `a/c`,
      `Observação`, Arquivo, arquivo.tabela.tipo, Arquivo_tipo,
      Arquivo_fonte
    )

  # Se solicitado, salva em xlsx
  if (xlsx) {
    # Ajuste este caminho e nome de arquivo conforme necessário
    writexl::write_xlsx(despesas_df, paste0(f_caminho.pasta.ik_c, "/despesas_consolidadas.xlsx"))
  }

  return(despesas_df)
}

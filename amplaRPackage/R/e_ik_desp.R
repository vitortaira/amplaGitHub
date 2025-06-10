# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik_desp.R
#' @title Extração de Despesas Informakon
#'
#' @description
#' A função e_ik_desp() extrai os dados de despesas dos arquivos na pasta
#' "informakon", preenche-os em uma planilha xlsx (opcional) e os retorna em
#' um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
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
    f_caminho.pasta.ik_c = caminhos_pastas("informakon"),
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
    # Padroniza nomes
    rename(
      data.doc.pagto = `Data Doc Pagto`,
      data.liberacao = `Data Liberação`,
      credor = `Credor`,
      cod.centro = `Cod. Centro`,
      centro.negocio = `Centro de Negócio`,
      agente.financeiro = `Agente Financeiro`,
      numero.conta = `N° Conta`,
      numero.entrada = `Nº Entrada`,
      documento = `Documento`,
      parcela = `Parcela`,
      data.vencimento = `Data Vencimento`,
      data.vencimento.origem = `Data Vencimento Origem`,
      valor.titulo = `Valor Titulo`,
      acrescimos = `Acréscimos`,
      descontos = `Descontos`,
      encargos = `Encargos`,
      descontos.adiant = `Descontos Adiant.`,
      multa = `Multa`,
      total.pago = `Total Pago`,
      `a/c` = `a/c`,
      observacao = `Observação`
    ) %>%
    mutate(
      `a/c` = as.character(`a/c`),
      acrescimos = as.numeric(acrescimos),
      agente.financeiro = as.character(agente.financeiro),
      centro.negocio = as.character(centro.negocio),
      cod.centro = as.character(cod.centro),
      credor = as.character(credor),
      data.doc.pagto = as.Date(data.doc.pagto),
      data.liberacao = as.Date(data.liberacao),
      data.vencimento = as.Date(data.vencimento),
      data.vencimento.origem = as.Date(data.vencimento.origem),
      descontos = as.numeric(descontos),
      descontos.adiant = as.numeric(descontos.adiant),
      documento = as.character(documento),
      empresa = cod.centro %>% stringr::str_sub(1, 3),
      encargos = as.numeric(encargos),
      mes = lubridate::floor_date(data.doc.pagto, "month"),
      multa = as.numeric(multa),
      n.conta = as.character(numero.conta),
      n.entrada = as.integer(numero.entrada),
      observacao = as.character(observacao),
      parcela = as.character(parcela),
      total.pago = as.numeric(total.pago),
      valor.titulo = as.numeric(valor.titulo),
      arquivo = caminho_arquivo_despesas,
      arquivo.tabela.tipo = "desp",
      arquivo.tipo = "desp",
      arquivo.fonte = "ik"
    ) %>%
    select(
      data.doc.pagto, mes, data.liberacao, credor, empresa,
      cod.centro, centro.negocio, agente.financeiro, numero.conta,
      numero.entrada, documento, parcela, data.vencimento,
      data.vencimento.origem, valor.titulo, acrescimos, descontos,
      encargos, descontos.adiant, multa, total.pago, `a/c`,
      observacao, arquivo, arquivo.tabela.tipo, arquivo.tipo,
      arquivo.fonte
    )

  # Se solicitado, salva em xlsx
  if (xlsx) {
    # Ajuste este caminho e nome de arquivo conforme necessário
    writexl::write_xlsx(despesas_df, paste0(f_caminho.pasta.ik_c, "/despesas_consolidadas.xlsx"))
  }

  return(despesas_df)
}

# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik_desp.R
#' @title Extração de Despesas Informakon
#'
#' @description
#' A função e_ik_desp() extrai os dados de despesas dos arquivos na pasta
#' "fechamento_in" e os retorna em um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "fechamento_in".
#'   Valor padrão: \code{caminhos_pastas("fechamento_in")}.
#'
#' @return Data frame com dados das despesas consolidadas.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' desp <- e_ik_desp()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub str_detect
#' @importFrom lubridate floor_date
#' @export
e_ik_desp <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("fechamento_in")) {
  # Função interna para buscar o arquivo de despesas mais recente
  obter_caminho_despesas <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'fechamento_in' não foi encontrada.")
    }
    # Busca arquivos que começam com "desp-"
    caminhos_desp <- dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_desp <- caminhos_desp[
      basename(caminhos_desp) %>% str_detect("^desp-")
    ]
    if (length(caminhos_desp) == 0) {
      stop("Nenhum arquivo de despesas encontrado na pasta fechamento_in.")
    }
    # Determina a data final (YYYY_MM_DD) mais recente
    # Padrão: desp-YYYY_MM_DD-YYYY_MM_DD.xlsx
    data_final_por_arquivo <- sapply(caminhos_desp, function(path) {
      basename(path) %>%
        str_extract("\\d{4}_\\d{2}_\\d{2}(?=\\.xlsx$)") %>%
        as.Date(format = "%Y_%m_%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_desp[indice_recente]
  }

  # Carrega o arquivo de despesas mais recente
  caminho_arquivo_despesas <- obter_caminho_despesas()
  nomes_colunas <- c(
    "n.siban", "nucleo.num", "empresa", "centro.negocio", "origem",
    "tipo.entrada", "documento", "parcela", "data.vencimento",
    "data.pagamento", "valor", "a/c", "documento.pagto", "credor",
    "classe", "assunto.titulo", "grupo.titulo", "subgrupo.titulo",
    "classificacao", "d/c", "nucleo", "cod.grupo.nuc", "grupo.nucleo",
    "cod.grupo.cen", "grupo.centro", "cod.classe.cen", "classe.centro"
  )
  desp <- read_excel(
    caminho_arquivo_despesas,
    skip = 1,
    col_names = nomes_colunas,
    col_types = "text"
  ) %>%
    mutate(
      data.pagamento = as.Date(as.numeric(data.pagamento), origin = "1899-12-30"),
      data.vencimento = as.Date(as.numeric(data.vencimento), origin = "1899-12-30"),
      nucleo.num = as.integer(nucleo.num),
      valor = as.numeric(valor),
      arquivo = caminho_arquivo_despesas,
      arquivo.tabela.tipo = "desp",
      arquivo.tipo = "desp",
      arquivo.fonte = "ik"
    ) %>%
    select(
      n.siban, nucleo.num, empresa, centro.negocio, origem, tipo.entrada,
      documento, parcela, data.vencimento, data.pagamento, valor,
      `a/c`, documento.pagto, credor, classe, assunto.titulo,
      grupo.titulo, subgrupo.titulo, classificacao, `d/c`, nucleo,
      cod.grupo.nuc, grupo.nucleo, cod.grupo.cen, grupo.centro,
      cod.classe.cen, classe.centro, arquivo, arquivo.tabela.tipo,
      arquivo.tipo, arquivo.fonte
    ) %>%
    # Remove linha totalizadora (soma no final da tabela do arquivo bruto)
    filter(!(!is.na(valor) & if_all(
      c(n.siban, centro.negocio, nucleo, classe),
      is.na
    )))

  return(desp)
}

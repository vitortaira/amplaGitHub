#' @title Extração de dados CMF Informakon
#'
#' @description
#' A função e_ik_cmf() extrai os dados de CMF (Conta Movimento Financeiro)
#' dos arquivos Excel na pasta "informakon" e os retorna em um data frame
#' padronizado. A função automaticamente localiza o arquivo CMF mais recente,
#' mapeia as colunas usando expressões regulares flexíveis, e converte os
#' tipos de dados adequadamente (datas Excel, valores numéricos, etc.).
#'
#' @details
#' A função realiza as seguintes operações:
#' \itemize{
#'   \item Busca arquivos com padrão "cmf_ik_*.xlsx" na pasta informakon
#'   \item Seleciona automaticamente o arquivo mais recente baseado na data no nome
#'   \item Mapeia colunas de forma flexível usando regex case-insensitive
#'   \item Converte datas do formato serial Excel para Date
#'   \item Converte valores numéricos adequadamente
#'   \item Adiciona metadados de rastreamento (arquivo, tipo, fonte)
#' }
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#'
#' @return Data frame com dados do CMF consolidados contendo as seguintes colunas padronizadas:
#' \itemize{
#'   \item \code{n.mov}: Número do movimento (integer)
#'   \item \code{data}: Data do movimento (Date)
#'   \item \code{c}: Código C (character)
#'   \item \code{origem}: Origem da transação (character)
#'   \item \code{historico}: Histórico da transação (character)
#'   \item \code{agente.financeiro}: Agente financeiro (character)
#'   \item \code{n.conta}: Número da conta (character)
#'   \item \code{valor}: Valor da transação (numeric)
#'   \item \code{d.c}: Débito/Crédito (character)
#'   \item \code{cancelado}: Status de cancelamento (character)
#'   \item \code{nat}: Código de natureza (character)
#'   \item \code{natureza.mov}: Natureza do movimento (character)
#'   \item \code{emp.filial}: Empresa/Filial (character)
#'   \item \code{nucleo}: Núcleo (character)
#'   \item \code{conciliacao}: Data de conciliação (Date)
#'   \item \code{cliente}: Cliente (character)
#'   \item \code{link.natureza}: Link de natureza (character)
#'   \item \code{saldo.caucao.cliente}: Saldo de caução do cliente (numeric)
#'   \item \code{arquivo}: Caminho do arquivo fonte (character)
#'   \item \code{arquivo.tipo}: Tipo do arquivo ("cmf") (character)
#'   \item \code{arquivo.fonte}: Fonte dos dados ("ik") (character)
#' }
#'
#' @examples
#' \dontrun{
#' # Chamando a função básica
#' cmf_df <- e_ik_cmf()
#'
#' # Especificando caminho customizado
#' cmf_df <- e_ik_cmf(f_caminho.pasta.ik_c = "caminho/para/informakon")
#'
#' # Verificando as colunas retornadas
#' names(cmf_df)
#'
#' # Verificando os tipos de dados
#' str(cmf_df)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename
#' @importFrom stringr str_detect str_extract str_remove str_which
#' @importFrom fs dir_ls
#' @export
e_ik_cmf <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("informakon")) {
  # Função interna para buscar o arquivo CMF mais recente
  obter_caminho_cmf <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }

    # Busca arquivos que começam com "cmf_ik_"
    caminhos_cmf <- fs::dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_cmf <- caminhos_cmf[
      basename(caminhos_cmf) %>% stringr::str_detect("^cmf_ik_") &
        basename(caminhos_cmf) %>% stringr::str_detect("\\.xlsx$")
    ]

    if (length(caminhos_cmf) == 0) {
      stop("Nenhum arquivo CMF encontrado na pasta informakon.")
    }

    # Se houver múltiplos arquivos, pega o mais recente baseado na data no nome
    if (length(caminhos_cmf) > 1) {
      data_final_por_arquivo <- sapply(caminhos_cmf, function(path) {
        # Extrai a data final do padrão cmf_ik_YYYYMMDD_YYYYMMDD.xlsx
        basename(path) %>%
          stringr::str_extract("_\\d{8}\\.xlsx$") %>%
          stringr::str_remove("\\.xlsx$") %>%
          stringr::str_remove("^_") %>%
          as.Date(format = "%Y%m%d")
      })
      indice_recente <- which.max(data_final_por_arquivo)
      caminhos_cmf[indice_recente]
    } else {
      caminhos_cmf[1]
    }
  }

  # Carrega o arquivo CMF mais recente
  caminho_arquivo_cmf <- obter_caminho_cmf()

  # Lê o arquivo forçando todas as colunas como texto
  cmf_df <- readxl::read_excel(caminho_arquivo_cmf, col_types = "text") %>%
    rename(
      n.mov = names(.)[str_which(names(.), "(?i)mov")[1]],
      data = names(.)[str_which(names(.), "(?i)data")[1]],
      c = names(.)[str_which(names(.), "^(?i)c$")[1]],
      origem = names(.)[str_which(names(.), "(?i)origem")[1]],
      historico = names(.)[str_which(names(.), "(?i)hist[oó]rico")[1]],
      agente.financeiro = names(.)[str_which(names(.), "(?i)agente\\s?financeiro")[1]],
      n.conta = names(.)[str_which(names(.), "(?i)conta\\s?nº?")[1]],
      valor = names(.)[str_which(names(.), "(?i)valor")[1]],
      d.c = names(.)[str_which(names(.), "(?i)d/c")[1]],
      cancelado = names(.)[str_which(names(.), "(?i)cancelado")[1]],
      nat = names(.)[str_which(names(.), "^(?i)nat$")[1]],
      natureza.mov = names(.)[str_which(names(.), "(?i)natureza.*mov")[1]],
      emp.filial = names(.)[str_which(names(.), "(?i)emp-filial")[1]],
      nucleo = names(.)[str_which(names(.), "(?i)n[uú]cleo")[1]],
      conciliacao = names(.)[str_which(names(.), "(?i)concilia[cç][aã]o")[1]],
      cliente = names(.)[str_which(names(.), "(?i)cliente")[1]],
      link.natureza = names(.)[str_which(names(.), "(?i)link\\s?natureza")[1]],
      saldo.caucao.cliente = names(.)[str_which(names(.), "(?i)saldo\\s?cau[cç][aã]o\\s?cliente")[1]]
    ) %>%
    dplyr::mutate(
      n.mov = as.integer(n.mov),
      data = as.Date(as.integer(data), origin = "1899-12-30"),
      valor = valor %>% as.numeric(),
      conciliacao = as.Date(as.integer(conciliacao), origin = "1899-12-30"),
      saldo.caucao.cliente = saldo.caucao.cliente %>% as.numeric(),
      arquivo = caminho_arquivo_cmf,
      arquivo.tipo = "cmf",
      arquivo.fonte = "ik"
    )
  return(cmf_df)
}

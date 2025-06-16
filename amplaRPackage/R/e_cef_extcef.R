# Descrição ---------------------------------------------------------------

#' @title Extracao dos dados do PDF de um extrato da CEF
#'
#' @description
#' Extrai e organiza dados de um extrato bancario da CEF em PDF.
#'
#' @param f_caminho.arquivo_c Caminho completo para o arquivo PDF contendo o
#' extrato da CEF.
#'
#' @details
#' Utiliza o pacote pdftools para ler o arquivo e manipular o texto,
#' identificando padroes que auxiliam na extracao das informacoes.
#'
#' @return
#' Retorna uma tibble com as seguintes colunas:
#'   - Data de lancamento  : Date
#'   - Data de movimento   : Date
#'   - documento           : Character
#'   - Historico           : Character
#'   - valor               : Numeric
#'   - Saldo               : Numeric
#'   - conta.interno       : Character
#'   - conta               : Character
#'   - Agencia             : Character
#'   - produto             : Character
#'   - cnpj                : Character
#'   - cliente             : Character
#'   - Periodo_inicio      : Date
#'   - Periodo_fim         : Date
#'   - data.consulta       : POSIXct
#'
#' @examples
#' \dontrun{
#' extrato <- e_cef_extcef(
#'   f_caminho.arquivo_c = "caminho/para/o/extrato.pdf"
#' )
#' print(extrato)
#'
#' library(dplyr)
#' extrato_filtrado <- e_cef_extcef("caminho/para/o/extrato.pdf") %>%
#'   filter(valor > 0)
#' summary(extrato_filtrado)
#' }
#'
#' @seealso
#' Consulte \code{\link{e_cef_extcefs}}.
#'
#' @references
#' Consulte \code{\link{pdf_text}} para extracao de texto de arquivos PDF.
#'
#' @export

caminhos.teste_c <- c(
  str_c(caminhos_pastas("testthat"), "/data/extcef1.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/extcef2.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/extcef3.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/extcef4.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/extcef5.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/extcef6.pdf")
)

e_cef_extcef <- function(f_caminho.arquivo_c) {
  # Read and clean lines
  paginas_l <- pdftools::pdf_text(f_caminho.arquivo_c) %>%
    purrr::map(function(page) {
      lines <- stringr::str_split(page, "\n")[[1]]
      lines <- stringr::str_squish(lines)
      purrr::discard(lines, function(line) line == "")
    })
  linhas_c <- unlist(paginas_l, use.names = FALSE)
  tipo_c <- c_cef_extcef(
    f_caminho.arquivo_c, linhas_c
  )

  # TODO: Add your extraction logic here to build the tibble
  # For now, return an empty tibble with the correct columns:
  tibble::tibble(
    data.lancamento = as.Date(character()),
    data.movimentacao = as.Date(character()),
    documento = character(),
    descricao = character(),
    valor = numeric(),
    saldo = numeric(),
    conta.interno = character(),
    conta = character(),
    agencia = character(),
    produto = character(),
    cnpj = character(),
    empresa = character(),
    periodo.inicio = as.Date(character()),
    periodo.fim = as.Date(character()),
    data.consulta = as.POSIXct(character()),
    contrato.6 = character(),
    repasse = logical(),
    pj = logical(),
    arquivo = character(),
    arquivo.tabela.tipo = character(),
    arquivo.tipo = character(),
    arquivo.fonte = character()
  )
}

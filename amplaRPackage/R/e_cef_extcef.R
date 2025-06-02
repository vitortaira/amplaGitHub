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
#'   - Valor               : Numeric
#'   - Saldo               : Numeric
#'   - Conta_interno       : Character
#'   - Conta               : Character
#'   - Agencia             : Character
#'   - Produto             : Character
#'   - cnpj                : Character
#'   - cliente             : Character
#'   - Periodo_inicio      : Date
#'   - Periodo_fim         : Date
#'   - Data_consulta       : POSIXct
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
#'   filter(Valor > 0)
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
e_cef_extcef <- function(f_caminho.arquivo_c) {
  # Define paginas_l
  paginas_l <- pdf_text(f_caminho.arquivo_c) %>%
    map(function(page) {
      lines <- str_split(page, "\n")[[1]]
      lines <- str_squish(lines)
      discard(lines, function(line) {
        line == ""
      })
    })

  # Se o extrato da CEF for do tipo com o título "Extrato por período"
  if (sum(str_detect(paginas_l[[1]], "Extra([A-Za-z]{2})? por per")) > 0) {
    linhas_c <- unlist(paginas_l, use.names = FALSE)

    # Metadados
    cliente_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "cliente:")
      }) %>%
      str_remove("^cliente: ") %>%
      str_trim()

    conta_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Conta[A-Za-z]?:")
      }) %>%
      str_remove("^Conta[A-Za-z]?:\\s?") %>%
      str_trim()

    data.consulta_h <-
      case_when(
        str_starts(nth(linhas_c, 1), "\\d{2}/\\d{2}/\\d{4}") ~
          (nth(linhas_c, 1) %>% str_extract("\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}")
            %>% str_replace("\\,\\s?", "-")
            %>% as.POSIXct(format = "%d/%m/%Y-%H:%M")),
        sum(str_starts(linhas_c, "Data:")) > 0 ~
          (linhas_c %>%
            keep(function(x) {
              str_starts(x, "Data:")
            }) %>%
            str_extract("\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}\\:\\d{2}") %>%
            str_remove_all(" "))[1] %>% as.POSIXct(format = "%d/%m/%Y-%H:%M"),
        TRUE ~ NA
      )

    mes.consultado_d <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Mês:")
      }) %>%
      str_remove("^Mês: ") %>%
      str_trim() %>%
      str_replace_all(
        c(
          "Janeiro" = "01", "Fevereiro" = "02", "Março" = "03",
          "Abril" = "04", "Maio" = "05", "Junho" = "06",
          "Julho" = "07", "Agosto" = "08", "Setembro" = "09",
          "Outubro" = "10", "Novembro" = "11", "Dezembro" = "12"
        )
      )

    periodo.consultado_c <- str_c(
      (linhas_c %>%
        keep(function(x) {
          str_starts(x, "Período:")
        }) %>%
        str_remove("^Período: ") %>% str_replace_all(" ", "") %>% str_trim() %>%
        str_extract(".*(?=-)") %>% if_else(str_length(.) == 1, str_c("0", .), .)),
      "/",
      mes.consultado_d,
      "-",
      (linhas_c %>%
        keep(function(x) {
          str_starts(x, "Período:")
        }) %>%
        str_remove("^Período: ") %>% str_replace_all(" ", "") %>% str_trim() %>%
        str_extract("(?<=-).*") %>% if_else(str_length(.) == 1, str_c("0", .), .)),
      "/",
      mes.consultado_d
    )

    linhas_c <- linhas_c %>%
      keep(function(x) {
        !str_starts(x, "https") &&
          !str_starts(x, "file:") &&
          !str_ends(x, "CaIXA") &&
          !str_starts(x, "\\d{2}/\\d{2}/\\d{4}\\,")
      }) %>%
      str_remove_all("\\°|\\º")

    indice.comeco_i <- linhas_c %>%
      str_which("^\\d{2}") %>%
      nth(1)
    indice.fim_i <- linhas_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      last()

    extrato_t <- linhas_c %>%
      as_tibble_col(column_name = "linhas") %>%
      slice(indice.comeco_i:indice.fim_i) %>%
      mutate(
        `Data Mov.` = if_else(
          word(linhas) == "000000",
          str_extract(periodo.consultado_c, ".*(?=-)") %>%
            as.Date(format = "%d/%m/%Y") %>% rep(length(linhas)),
          word(linhas) %>% as.Date(format = "%d/%m/%Y")
        ),
        linhas = str_remove(linhas, "^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        `Nr. Doc` = word(linhas),
        linhas = str_remove(linhas, str_c("^", word(linhas))) %>% str_trim(),
        Saldo = str_extract(linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$") %>%
          str_remove("\\s?C") %>% str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        linhas = str_remove(linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$"),
        Valor = str_extract(linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?") %>%
          str_remove("\\s?C") %>% str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        descricao = str_remove(
          linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?"
        ) %>% str_trim(),
        `Data Lanc.` = NA,
        Conta = word(conta_c, -1) %>% str_trim(),
        agencia = str_sub(conta_c, 1, 4),
        Produto = str_sub(conta_c, 6, -1) %>%
          str_extract("\\s\\d{4}\\s") %>%
          str_trim(),
        cnpj = NA,
        cliente = cliente_c,
        `periodo.inicio` = str_remove(periodo.consultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        `periodo.fim` = str_remove(periodo.consultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        Data_consulta = data.consulta_h,
        Conta_interno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        Arquivo = f_caminho.arquivo_c
      ) %>%
      select(
        `Data Lanc.`, `Data Mov.`, `Nr. Doc`, descricao, Valor, Saldo,
        Conta_interno, Conta, agencia, Produto, cnpj, cliente,
        `periodo.inicio`, `periodo.fim`, Data_consulta, Arquivo
      ) %>%
      rename(
        data.lancamento = `Data Lanc.`,
        data.movimento = `Data Mov.`,
        documento = `Nr. Doc`
      )

    return(extrato_t)
  }
  if (
    any(str_detect(paginas_l[[1]], "(?i)ag[eê]ncia")) &
      !any(str_detect(paginas_l[[1]], "(?i)extrato\\s?fundo\\s?de\\s?investimento"))
  ) {
    # Se o extrato da CEF for do tipo sem o título "Extrato por período"
    linhas_c <- unlist(paginas_l, use.names = FALSE) %>%
      keep(function(x) {
        !str_starts(x, "Data de lançamento")
      })

    agencia_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*Produto:.*") %>%
      str_sub(-4, -1)

    cliente_c <- linhas_c %>%
      nth(1) %>%
      str_trim()

    cnpj_c <- linhas_c %>%
      nth(2) %>%
      str_remove("^cnpj:\\s*") %>%
      str_remove_all("[A-Za-z]") %>%
      str_trim()

    conta_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*\\d{2}/\\d{2}/\\d{4}.*") %>%
      str_remove(".*Conta:\\s*") %>%
      str_trim()

    data.consulta_h <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}") %>%
      str_trim() %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M")

    periodo.consultado_c <- linhas_c %>%
      keep(function(x) {
        str_detect(x, "Lançamentos de")
      }) %>%
      str_remove(".*amentos de\\s?") %>%
      str_remove_all(" ") %>%
      str_replace("à", "-") %>%
      str_trim()

    produto_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*Conta:.*") %>%
      str_remove(".*Produto:\\s*") %>%
      str_trim()

    indice.comeco_i <- linhas_c %>% str_which("^Extrato") + 1
    indice.fim_i <- linhas_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      last()

    extrato_t <- linhas_c %>%
      as_tibble_col(column_name = "linhas") %>%
      slice(indice.comeco_i:indice.fim_i) %>%
      mutate(
        data.lancamento = str_extract(linhas, "\\d{2}/\\d{2}/\\d{4}") %>%
          as.Date(format = "%d/%m/%Y"),
        linhas = str_remove(linhas, "\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        data.movimento = str_extract(linhas, "\\d{2}/\\d{2}/\\d{4}") %>%
          as.Date(format = "%d/%m/%Y"),
        linhas = str_remove(linhas, "\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        documento = str_remove(linhas, "[A-Za-z].*") %>% str_trim(),
        linhas = str_extract(linhas, "(?i)[A-Za-z].*") %>% str_trim(),
        descricao = str_remove(linhas, "R\\$.*") %>% str_trim(),
        linhas = str_extract(linhas, "(?<=R\\$).*") %>% str_trim(),
        valor = str_remove(linhas, "R\\$.*") %>%
          str_remove_all("\\.") %>% str_replace("\\,", "\\.") %>% as.numeric(),
        saldo = str_extract(linhas, "(?<=R\\$).*") %>%
          str_trim() %>% str_remove_all("\\.") %>% str_replace("\\,", "\\.") %>%
          as.numeric(),
        agencia = agencia_c,
        cliente = cliente_c,
        cnpj = cnpj_c,
        Conta = conta_c,
        Data_consulta = data.consulta_h,
        `periodo.inicio` = str_remove(periodo.consultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        `periodo.fim` = str_remove(periodo.consultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        Produto = produto_c,
        Conta_interno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        Arquivo = f_caminho.arquivo_c
      ) %>%
      select(
        data.lancamento, data.movimento, documento, descricao,
        valor, saldo,
        Conta_interno, Conta, agencia, Produto, cnpj, cliente,
        `periodo.inicio`, `periodo.fim`, Data_consulta, Arquivo
      ) %>%
      rename(Saldo = saldo, Valor = valor)

    return(extrato_t)
  } else {
    message(str_c("O arquivo ", f_caminho.arquivo_c, " não foi extraído."))
  }
}
# Teste -------------------------------------------------------------------

# f_caminho.arquivo_c <- caminhos.extratos.cef_c[4]
# e_cef_extcef(f_caminho.arquivo_c)
# shell.exec(f_caminho.arquivo_c)
# sapply(caminhos.extratos.cef_c, shell.exec)

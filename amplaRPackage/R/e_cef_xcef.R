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
#' extrato <- e_cef_xcef(
#'   f_caminho.arquivo_c = "caminho/para/o/extrato.pdf"
#' )
#' print(extrato)
#'
#' library(dplyr)
#' extrato_filtrado <- e_cef_xcef("caminho/para/o/extrato.pdf") %>%
#'   filter(valor > 0)
#' summary(extrato_filtrado)
#' }
#'
#' @seealso
#' Consulte \code{\link{e_cef_xcefs}}.
#'
#' @references
#' Consulte \code{\link{pdf_text}} para extracao de texto de arquivos PDF.
#'
#' @export

caminhos.teste_c <- c(
  str_c(caminhos_pastas("testthat"), "/data/xcef1.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef2.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef3.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef4.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef5.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef6.pdf")
)

e_cef_xcef <- function(f_caminho.arquivo_c) {
  # Validação de entrada
  if (is.null(f_caminho.arquivo_c) || !file.exists(f_caminho.arquivo_c)) {
    # Retorna tibble vazia com estrutura esperada
    return(tibble::tibble(
      data = as.Date(character()),
      agencia = character(),
      conta = character(),
      documento = character(),
      descricao = character(),
      valor = numeric(),
      tipo_lancamento = character()
    ))
  }

  # Tentar ler PDF com tratamento de erro
  resultadoLeitura <- tryCatch(
    {
      list(
        paginasLista = ler_pdf(f_caminho.arquivo_c)$paginas,
        linhasTexto = ler_pdf(f_caminho.arquivo_c)$linhas
      )
    },
    error = function(e) {
      # Retorna tibble vazia se não conseguir ler o PDF
      return(tibble::tibble(
        data = as.Date(character()),
        agencia = character(),
        conta = character(),
        documento = character(),
        descricao = character(),
        valor = numeric(),
        tipo_lancamento = character()
      ))
    }
  )

  # Se retornou erro na leitura, retornar o tibble vazio
  if (inherits(resultadoLeitura, "tbl_df")) {
    return(resultadoLeitura)
  }

  paginasLista_l <- resultadoLeitura$paginasLista
  linhasTexto_c <- resultadoLeitura$linhasTexto

  # Identificar o tipo do xcef
  tipoXcef_c <- c_cef_xcef(f_caminho.arquivo_c, linhasTexto_c)
  if (tipoXcef_c == "xcef1") {
    message(sprintf("Formato pendente para o arquivo: %s", f_caminho.arquivo_c))
    # Obter dados estruturados do PDF
    dadosPdf_l <- pdf_data(f_caminho.arquivo_c)
    palavrasTabela_t <-
      dadosPdf_l %>%
      seq_along() %>%
      map_dfr(~ dadosPdf_l[[.x]] %>% mutate(pagina = .x))
    linhasTexto_c <-
      palavrasTabela_t %>%
      arrange(pagina, y, x) %>%
      group_by(pagina) %>%
      mutate(
        diferencaY = abs(y - dplyr::lag(y, default = first(y))),
        numeroLinha = cumsum(row_number() == 1 | diferencaY > 10)
      ) %>%
      ungroup() %>%
      group_by(pagina, numeroLinha) %>%
      summarise(
        textoCompleto =
          str_c(text, collapse = " ") %>%
            str_replace_all("\\s+", " ") %>%
            str_trim(),
        posicaoY = mean(y),
        .groups = "drop"
      ) %>%
      dplyr::select(textoCompleto, everything()) %>%
      # Concatenar linhas que começam com hh:mm:ss à linha anterior
      {
        indicesHorario_i <- stringr::str_which(.$textoCompleto, "^\\d{2}:\\d{2}:\\d{2}")
        if (length(indicesHorario_i) > 0) {
          for (indice in rev(indicesHorario_i)) {
            if (indice > 1) {
              .$textoCompleto[indice - 1] <- paste(.$textoCompleto[indice - 1], .$textoCompleto[indice], sep = " ")
            }
          }
          . <- .[-indicesHorario_i, ]
        }
        .
      } %>%
      pull(textoCompleto) %>%
      discard(~ str_starts(
        .x,
        "about\\:|\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}|\\d{2}/\\d{2}/\\d{4}$"
      ))

    # Retornar tibble vazia para formato xcef1 (pendente de implementação)
    return(tibble::tibble(
      data = as.Date(character()),
      agencia = character(),
      conta = character(),
      documento = character(),
      descricao = character(),
      valor = numeric(),
      tipo_lancamento = character()
    ))
  }
  if (tipoXcef_c == "xcef2") {
    linhasTexto_c %<>% keep(function(x) {
      !str_starts(x, "Data de lançamento")
    })
    numeroAgencia_c <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*produto:.*") %>%
      str_sub(-4, -1)
    nomeCliente_c <- linhasTexto_c %>%
      nth(1) %>%
      str_trim()
    numeroCnpj_c <- linhasTexto_c %>%
      nth(2) %>%
      str_remove("^cnpj:\\s*") %>%
      str_remove_all("[A-Za-z]") %>%
      str_trim()
    numeroConta_c <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*\\d{2}/\\d{2}/\\d{4}.*") %>%
      str_remove(".*Conta:\\s*") %>%
      str_trim()
    dataConsulta_h <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}") %>%
      str_trim() %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M")
    periodoConsultado_c <- linhasTexto_c %>%
      keep(function(x) {
        str_detect(x, "Lançamentos de")
      }) %>%
      str_remove(".*amentos de\\s?") %>%
      str_remove_all(" ") %>%
      str_replace("à", "-") %>%
      str_trim()
    tipoProduto_c <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*Conta:.*") %>%
      str_remove(".*produto:\\s*") %>%
      str_trim()
    indiceComeco_i <- linhasTexto_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      first()
    indiceFim_i <- linhasTexto_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      last()
    extratoTabela_t <- linhasTexto_c %>%
      as_tibble_col(column_name = "linhasTexto") %>%
      slice(indiceComeco_i:indiceFim_i) %>%
      mutate(
        dataLancamento = str_extract(linhasTexto, "\\d{2}/\\d{2}/\\d{4}") %>%
          as.Date(format = "%d/%m/%Y"),
        linhasTexto = str_remove(linhasTexto, "\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        dataMovimentacao = str_extract(linhasTexto, "\\d{2}/\\d{2}/\\d{4}") %>%
          as.Date(format = "%d/%m/%Y"),
        linhasTexto = str_remove(linhasTexto, "\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        numeroDocumento = str_remove(linhasTexto, "[A-Za-z].*") %>% str_trim(),
        linhasTexto = str_extract(linhasTexto, "(?i)[A-Za-z].*") %>% str_trim(),
        valorTransacao = str_extract(
          linhasTexto,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_remove_all("\\s") %>%
          readr::parse_number(
            locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
          ),
        linhasTexto = str_remove(
          linhasTexto,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_trim(),
        saldoConta = str_extract(
          linhasTexto,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_remove_all("\\s") %>%
          readr::parse_number(
            locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
          ),
        descricaoTransacao = str_remove(
          linhasTexto,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_trim(),
        numeroAgencia = numeroAgencia_c,
        nomeEmpresa = nomeCliente_c,
        cnpjEmpresa = numeroCnpj_c,
        numeroConta = numeroConta_c,
        dataConsulta = dataConsulta_h,
        periodoInicio = str_remove(periodoConsultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        periodoFim = str_remove(periodoConsultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        tipoProduto = tipoProduto_c,
        contaInterno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        caminhoArquivo = f_caminho.arquivo_c,
        subtipoArquivo = tipoXcef_c
      ) %>%
      dplyr::select(
        data = dataLancamento,
        agencia = numeroAgencia,
        conta = numeroConta,
        documento = numeroDocumento,
        descricao = descricaoTransacao,
        valor = valorTransacao,
        tipo_lancamento = subtipoArquivo
      )
    return(extratoTabela_t)
  }
  if (tipoXcef_c %in% c("xcef3", "xcef4", "xcef5", "xcef6")) {
    # Metadados
    nomeCliente_c <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "(?i)cliente:")
      }) %>%
      str_remove("^(?i)cliente: ") %>%
      str_trim()

    numeroConta_c <- linhasTexto_c %>%
      keep(function(x) {
        str_starts(x, "Conta[A-Za-z]?:")
      }) %>%
      str_remove("^Conta[A-Za-z]?:\\s?") %>%
      str_trim()

    dataConsulta_h <-
      case_when(
        str_starts(nth(linhasTexto_c, 1), "\\d{2}/\\d{2}/\\d{4}") ~
          (nth(linhasTexto_c, 1) %>% str_extract("\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}")
            %>% str_replace("\\,\\s?", "-")
            %>% as.POSIXct(format = "%d/%m/%Y-%H:%M")),
        sum(str_starts(linhasTexto_c, "Data:")) > 0 ~
          (linhasTexto_c %>%
            keep(function(x) {
              str_starts(x, "Data:")
            }) %>%
            str_extract("\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}\\:\\d{2}") %>%
            str_remove_all(" "))[1] %>% as.POSIXct(format = "%d/%m/%Y-%H:%M"),
        TRUE ~ NA
      )

    mesConsultado_d <- linhasTexto_c %>%
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

    periodoConsultado_c <- str_c(
      (linhasTexto_c %>%
        keep(function(x) {
          str_starts(x, "Período:")
        }) %>%
        str_remove("^Período: ") %>% str_replace_all(" ", "") %>% str_trim() %>%
        str_extract(".*(?=-)") %>% if_else(str_length(.) == 1, str_c("0", .), .)),
      "/",
      mesConsultado_d,
      "-",
      (linhasTexto_c %>%
        keep(function(x) {
          str_starts(x, "Período:")
        }) %>%
        str_remove("^Período: ") %>% str_replace_all(" ", "") %>% str_trim() %>%
        str_extract("(?<=-).*") %>% if_else(str_length(.) == 1, str_c("0", .), .)),
      "/",
      mesConsultado_d
    )

    linhasTexto_c <- linhasTexto_c %>%
      keep(function(x) {
        !str_starts(x, "https") &&
          !str_starts(x, "file:") &&
          !str_ends(x, "CaIXA") &&
          !str_starts(x, "\\d{2}/\\d{2}/\\d{4}\\,")
      }) %>%
      str_remove_all("\\°|\\º")

    indiceComeco_i <- linhasTexto_c %>%
      str_which("^\\d{2}") %>%
      nth(1)
    indiceFim_i <- linhasTexto_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      last()

    extratoTabela_t <- linhasTexto_c %>%
      as_tibble_col(column_name = "linhasTexto") %>%
      slice(indiceComeco_i:indiceFim_i) %>%
      mutate(
        dataMovimentacao = if_else(
          word(linhasTexto) == "000000",
          str_extract(periodoConsultado_c, ".*(?=-)") %>%
            as.Date(format = "%d/%m/%Y") %>% rep(length(linhasTexto)),
          word(linhasTexto) %>% as.Date(format = "%d/%m/%Y")
        ),
        linhasTexto = str_remove(linhasTexto, "^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        numeroDocumento = word(linhasTexto),
        linhasTexto = str_remove(linhasTexto, str_c("^", word(linhasTexto))) %>% str_trim(),
        saldoFinal = str_extract(linhasTexto, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$") %>%
          str_remove("\\s?C") %>% str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        linhasTexto = str_remove(linhasTexto, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$"),
        valorTransacao = stringr::str_extract(linhasTexto, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
        saldoConta = stringr::str_extract(linhasTexto, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
        descricaoTransacao = str_remove(
          linhasTexto, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?"
        ) %>% str_trim(),
        dataLancamento = NA,
        numeroConta = word(numeroConta_c, -1) %>% str_trim(),
        numeroAgencia = str_sub(numeroConta_c, 1, 4),
        tipoProduto = str_sub(numeroConta_c, 6, -1) %>%
          str_extract("\\s\\d{4}\\s") %>%
          str_trim(),
        cnpjEmpresa = NA,
        nomeEmpresa = nomeCliente_c,
        periodoInicio = str_remove(periodoConsultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        periodoFim = str_remove(periodoConsultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        dataConsulta = dataConsulta_h,
        contaInterno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        caminhoArquivo = f_caminho.arquivo_c,
        subtipoArquivo = tipoXcef_c
      ) %>%
      select(
        data = dataMovimentacao,
        agencia = numeroAgencia,
        conta = numeroConta,
        documento = numeroDocumento,
        descricao = descricaoTransacao,
        valor = valorTransacao,
        tipo_lancamento = subtipoArquivo
      )
    return(extratoTabela_t)
  } else {
    message(sprintf("Tipo desconhecido para o arquivo: %s", f_caminho.arquivo_c))
    return(tibble())
  }
}

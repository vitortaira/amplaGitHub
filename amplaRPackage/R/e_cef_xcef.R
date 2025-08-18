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
  str_c(caminhos_pastas("testthat"), "/data/xcef3.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef4.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef5.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef6.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef7.pdf"),
  str_c(caminhos_pastas("testthat"), "/data/xcef8.xlsx")
)

e_cef_xcef <- function(f_caminho.arquivo_c) {
  if (fs::path_ext(f_caminho.arquivo_c) == "pdf") {
    # Ler PDF
    paginas_l <- ler_pdf(f_caminho.arquivo_c)$paginas
    linhas_c <- ler_pdf(f_caminho.arquivo_c)$linhas
  }
  if (fs::path_ext(f_caminho.arquivo_c) == "xlsx") {
    # Ler XLSX
    tabela_t <- suppressMessages(
      readxl::read_excel(f_caminho.arquivo_c, col_names = FALSE)
    )
  }
  # Identificar o tipo do xcef
  tipo_c <- c_cef_xcef(f_caminho.arquivo_c)
  if (tipo_c == "xcef1") {
    message(sprintf("Formato pendente para o arquivo: %s", f_caminho.arquivo_c))
    palavras_t <-
      pdf_data(f_caminho.arquivo_c) %>%
      seq_along() %>%
      map_dfr(~ paginas_l[[.x]] %>% mutate(pagina = .x))
    linhas_c <-
      palavras_t %>%
      arrange(pagina, y, x) %>%
      group_by(pagina) %>%
      mutate(
        dif.y = abs(y - dplyr::lag(y, default = first(y))),
        linha = cumsum(row_number() == 1 | dif.y > 10)
      ) %>%
      ungroup() %>%
      group_by(pagina, linha) %>%
      summarise(
        texto =
          str_c(text, collapse = " ") %>%
            str_replace_all("\\s+", " ") %>%
            str_trim(),
        y = mean(y),
        .groups = "drop"
      ) %>%
      dplyr::select(texto, everything()) %>%
      # Concatenate lines starting with hh:mm:ss to the previous line
      {
        time_line_idx <- stringr::str_which(.$texto, "^\\d{2}:\\d{2}:\\d{2}")
        if (length(time_line_idx) > 0) {
          for (idx in rev(time_line_idx)) {
            if (idx > 1) {
              .$texto[idx - 1] <- paste(.$texto[idx - 1], .$texto[idx], sep = " ")
            }
          }
          . <- .[-time_line_idx, ]
        }
        .
      } %>%
      pull(texto) %>%
      discard(~ str_starts(
        .x,
        "about\\:|\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}|\\d{2}/\\d{2}/\\d{4}$"
      ))
  }
  if (tipo_c == "xcef3") {
    linhas_c %<>% keep(function(x) {
      !str_starts(x, "Data de lançamento")
    })
    agencia_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "Agência:")
      }) %>%
      str_remove("\\s*produto:.*") %>%
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
      str_remove(".*produto:\\s*") %>%
      str_trim()
    indice.comeco_i <- linhas_c %>%
      str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
      first()
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
        data.movimentacao = str_extract(linhas, "\\d{2}/\\d{2}/\\d{4}") %>%
          as.Date(format = "%d/%m/%Y"),
        linhas = str_remove(linhas, "\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        documento = str_remove(linhas, "[A-Za-z].*") %>% str_trim(),
        linhas = str_extract(linhas, "(?i)[A-Za-z].*") %>% str_trim(),
        valor = str_extract(
          linhas,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_remove_all("\\s") %>%
          readr::parse_number(
            locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
          ),
        linhas = str_remove(
          linhas,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_trim(),
        saldo = str_extract(
          linhas,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_remove_all("\\s") %>%
          readr::parse_number(
            locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
          ),
        descricao = str_remove(
          linhas,
          "(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})"
        ) %>%
          str_trim(),
        agencia = agencia_c,
        empresa = cliente_c,
        cnpj = cnpj_c,
        conta = conta_c,
        data.consulta = data.consulta_h,
        periodo.inicio = str_remove(periodo.consultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        periodo.fim = str_remove(periodo.consultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        produto = produto_c,
        conta.interno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        arquivo = f_caminho.arquivo_c,
        arquivo.subtipo = tipo_c
      ) %>%
      dplyr::select(
        data.lancamento, data.movimentacao, documento, descricao,
        valor, saldo,
        conta.interno, conta, agencia, produto, cnpj, empresa,
        periodo.inicio, periodo.fim, data.consulta, arquivo, arquivo.subtipo
      ) %>%
      dplyr::filter(
        !str_starts(descricao, "(?i)saldo\\s?anterior|(?i)saldo\\s?dia")
      )
    return(extrato_t)
  }
  if (tipo_c %in% c("xcef4", "xcef5", "xcef6", "xcef7")) {
    # Metadados
    cliente_c <- linhas_c %>%
      keep(function(x) {
        str_starts(x, "(?i)cliente:")
      }) %>%
      str_remove("^(?i)cliente: ") %>%
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
          (nth(linhas_c, 1) %>%
            str_extract("\\d{2}/\\d{2}/\\d{4}\\,?\\s?\\d{2}\\:\\d{2}") %>%
            str_replace("\\,\\s?", "-") %>%
            str_replace("\\s", "-") %>%
            as.POSIXct(format = "%d/%m/%Y-%H:%M")),
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
        !str_starts(x, "https") &
          !str_starts(x, "file:") &
          !str_ends(x, "CaIXA") &
          !str_starts(x, "\\d{2}/\\d{2}/\\d{4}\\,") &
          !str_starts(x, "\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}")
      }) %>%
      str_remove_all("\\°|\\º")

    indice.comeco_i <- linhas_c %>%
      str_which(
        "^(?i)data\\s?mov\\.\\s?nr\\.\\s?doc\\.\\s?hist[oó]rico\\s?valor\\s?saldo"
      ) %>%
      first() + 1
    indice.fim_i <- if_else(
      any(str_detect(linhas_c, "^(?i)lan[cç]amentos\\s?do\\s?dia")),
      str_which(linhas_c, "^(?i)lan[cç]amentos\\s?do\\s?dia")[1] - 1,
      linhas_c %>%
        str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
        last()
    )

    extrato_t <- linhas_c %>%
      as_tibble_col(column_name = "linhas") %>%
      slice(indice.comeco_i:indice.fim_i) %>%
      mutate(
        data.movimentacao = if_else(
          word(linhas) == "000000",
          str_extract(periodo.consultado_c, ".*(?=-)") %>%
            as.Date(format = "%d/%m/%Y") %>% rep(length(linhas)),
          word(linhas) %>% as.Date(format = "%d/%m/%Y")
        ),
        linhas = str_remove(linhas, "^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        documento = word(linhas),
        linhas = str_remove(linhas, str_c("^", word(linhas))) %>% str_trim(),
        Saldo = str_extract(linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$") %>%
          str_remove("\\s?C") %>% str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        linhas = str_remove(linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$"),
        valor = stringr::str_extract(linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
        saldo = stringr::str_extract(linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
        descricao = str_remove(
          linhas, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?"
        ) %>% str_trim(),
        data.lancamento = NA,
        conta = word(conta_c, -1) %>% str_trim(),
        agencia = str_sub(conta_c, 1, 4),
        produto = str_sub(conta_c, 6, -1) %>%
          str_extract("\\s\\d{4}\\s") %>%
          str_trim(),
        cnpj = NA,
        empresa = cliente_c,
        periodo.inicio = str_remove(periodo.consultado_c, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        periodo.fim = str_remove(periodo.consultado_c, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        data.consulta = data.consulta_h,
        conta.interno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        arquivo = f_caminho.arquivo_c,
        arquivo.subtipo = tipo_c
      ) %>%
      dplyr::select(
        data.lancamento, data.movimentacao, documento, descricao, valor, saldo,
        conta.interno, conta, agencia, produto, cnpj, empresa,
        periodo.inicio, periodo.fim, data.consulta, arquivo, arquivo.subtipo
      ) %>%
      dplyr::filter(
        !str_starts(descricao, "(?i)saldo\\s?anterior|(?i)saldo\\s?dia")
      )
    return(extrato_t)
  } else if (tipo_c == "xcef8") {
    # Fora da tabela
    empresa <- tabela_t[2, 2] %>% as.character()
    conta <- tabela_t[3, 2] %>%
      as.character() %>%
      str_extract("[^|]+$") %>%
      str_trim()
    agencia <- tabela_t[3, 2] %>%
      as.character() %>%
      str_extract("^[^|]+") %>%
      str_trim()
    produto <- tabela_t[3, 2] %>%
      as.character() %>%
      str_extract("\\|.*\\|") %>%
      str_remove_all("\\|") %>%
      str_trim()
    data.consulta <- tabela_t[4, 2] %>%
      as.character() %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}:\\d{2}") %>%
      str_remove_all("\\s") %>%
      as.POSIXct(format = "%d/%m/%Y-%H:%M")
    mes.referencia <- tabela_t[5, 2] %>%
      as.character() %>%
      str_replace_all(
        c(
          "Janeiro" = "01", "Fevereiro" = "02", "Março" = "03",
          "Abril" = "04", "Maio" = "05", "Junho" = "06",
          "Julho" = "07", "Agosto" = "08", "Setembro" = "09",
          "Outubro" = "10", "Novembro" = "11", "Dezembro" = "12"
        )
      )
    periodo.referencia <- str_c(
      # Início
      str_c(
        tabela_t[6, 2] %>%
          as.character() %>%
          str_extract("^[^-]") %>%
          str_trim(),
        "/",
        mes.referencia,
        "-"
      ),
      # Fim
      str_c(
        tabela_t[6, 2] %>%
          as.character() %>%
          str_extract("[^-]+$") %>%
          str_trim(),
        "/",
        mes.referencia
      )
    )
    # Tabela
    extrato_t <- tabela_t %>%
      slice(-c(1:7)) %>%
      {
        # Encontrar linha SAC CAIXA nos dados filtrados
        linha_sac <- str_which(pull(., 1), "(?i)sac\\s?caixa")
        if (length(linha_sac) > 0) {
          slice(., -c(linha_sac:nrow(.)))
        } else {
          .
        }
      } %>%
      dplyr::select(-2) %>%
      magrittr::set_names(c(
        "data.movimentacao", "documento", "descricao", "valor", "saldo"
      )) %>%
      mutate(
        data.movimentacao = as.Date(
          as.integer(data.movimentacao),
          origin = "1899-12-30"
        ),
        documento = str_pad(documento, 6, "0", side = "left"),
        valor = valor %>%
          str_remove("\\s?C") %>%
          str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        saldo = saldo %>%
          str_remove("\\s?C") %>%
          str_remove_all("\\.") %>%
          str_replace("\\,", "\\.") %>%
          if_else(str_detect(., "D$"),
            str_c("-", .) %>% str_remove("\\s?D$"),
            .
          ) %>% as.numeric(),
        # Adicionar metadados que estavam faltando
        data.lancamento = NA_Date_,
        conta = word(conta, -1) %>% str_trim(),
        agencia = agencia,
        produto = produto,
        cnpj = NA_character_,
        empresa = empresa,
        periodo.inicio = str_remove(periodo.referencia, "-.*") %>%
          as.Date(format = "%d/%m/%Y"),
        periodo.fim = str_remove(periodo.referencia, ".*-") %>%
          as.Date(format = "%d/%m/%Y"),
        data.consulta = data.consulta,
        conta.interno = basename(f_caminho.arquivo_c) %>%
          str_extract("\\d{4}"),
        arquivo = f_caminho.arquivo_c,
        arquivo.subtipo = tipo_c
      ) %>%
      dplyr::select(
        data.lancamento, data.movimentacao, documento, descricao, valor, saldo,
        conta.interno, conta, agencia, produto, cnpj, empresa,
        periodo.inicio, periodo.fim, data.consulta, arquivo, arquivo.subtipo
      ) %>%
      dplyr::filter(
        !str_starts(descricao, "(?i)saldo\\s?anterior|(?i)saldo\\s?dia")
      )
    return(extrato_t)
  } else {
    message(sprintf("Tipo desconhecido para o arquivo: %s", f_caminho.arquivo_c))
    return(tibble())
  }
}

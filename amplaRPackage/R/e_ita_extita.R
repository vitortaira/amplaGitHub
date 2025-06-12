#' Extrai dados de extrato ITAÚ (EXTITA) em formato PDF
#'
#' @description
#' Função para extrair e estruturar dados de extrato ITAÚ (EXTITA) a partir de um arquivo PDF.
#' Retorna uma lista com dois data.frames: lançamentos e saldos.
#'
#' @param file_path Caminho para o arquivo PDF do extrato ITAÚ.
#' @return Uma lista com dois data.frames: `extita_l` (lançamentos) e `extita_c` (saldos).
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_split str_squish str_starts str_detect str_remove str_extract str_replace_all str_which word
#' @importFrom purrr map discard keep
#' @importFrom lubridate dmy dmy_hms year
#' @importFrom tibble as_tibble_col tibble
#' @importFrom readr parse_number locale
#' @importFrom dplyr select bind_rows mutate if_else
#' @export
#'
e_ita_extita <- function(file_path) {
  # Validação de entrada
  stopifnot(is.character(file_path), length(file_path) == 1, file.exists(file_path))

  # Extração de texto do PDF
  paginas_l <- pdftools::pdf_text(file_path) %>%
    purrr::map(~ stringr::str_split(.x, "\n")[[1]] %>%
      stringr::str_squish() %>%
      purrr::discard(~ .x == ""))
  linhas_c <- unlist(paginas_l, use.names = FALSE)

  # Handle empty linhas_c
  if (length(linhas_c) == 0) {
    stop("The PDF file does not contain any extractable text.")
  }

  # Funções auxiliares internas
  extrai_data_consulta <- function(linhas) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "(?i)atualizado em")) %>%
      stringr::str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}:\\d{2}") %>%
      lubridate::dmy_hms()
  }
  extrai_periodo <- function(linhas, fim = FALSE) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "(?i)lan[cç]amentos\\s?per[ií]odo:")) %>%
      stringr::str_extract(if (fim) "\\d{2}/\\d{2}/\\d{4}$" else "\\d{2}/\\d{2}/\\d{4}") %>%
      lubridate::dmy()
  }
  extrai_empresa <- function(linhas) {
    linhas %>%
      purrr::keep(~ stringr::str_detect(.x, "(?i)ag[eê]ncia\\s?conta\\s?corrente")) %>%
      stringr::str_remove("(?i)\\s?ag[eê]ncia.*")
  }
  extrai_cnpj <- function(linhas) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      stringr::str_extract("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")
  }
  extrai_agencia <- function(linhas) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      stringr::str_remove("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}\\s?") %>%
      stringr::str_extract("\\d{4}")
  }
  extrai_conta <- function(linhas) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      stringr::word(-1)
  }
  extrai_valores <- function(linhas, pos) {
    linhas %>%
      purrr::keep(~ stringr::str_starts(.x, "(?i)R\\$")) %>%
      stringr::str_remove_all("(?i)R\\s?\\$\\s?") %>%
      stringr::word(pos) %>%
      readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  }

  # Extração dos dados principais
  data.consulta_dhms <- extrai_data_consulta(linhas_c)
  periodo.inicio_d <- extrai_periodo(linhas_c, fim = FALSE)
  periodo.fim_d <- extrai_periodo(linhas_c, fim = TRUE)
  empresa_c <- extrai_empresa(linhas_c)
  cnpj_c <- extrai_cnpj(linhas_c)
  agencia_c <- extrai_agencia(linhas_c)
  conta_c <- extrai_conta(linhas_c)
  saldo.disponivel.conta_n <- extrai_valores(linhas_c, 1)
  limite.conta.contratado_n <- extrai_valores(linhas_c, 2)
  limite.conta.utilizado_n <- extrai_valores(linhas_c, 3)
  limite.conta.disponivel_n <- extrai_valores(linhas_c, 4)

  # Lançamentos
  extita_t <- linhas_c %>%
    purrr::keep(~ stringr::str_starts(.x, "\\d{2}\\s?/\\s?[A-Za-z]{3}")) %>%
    tibble::as_tibble_col(column_name = "Linhas") %>%
    dplyr::mutate(
      data = stringr::str_extract(Linhas, "^\\d{2}\\s?/\\s?[A-Za-z]{3}") %>%
        stringr::str_remove_all("\\s") %>%
        stringr::str_replace_all(datas.b_pt.en),
      ano = dplyr::if_else(
        lubridate::year(periodo.inicio_d) == lubridate::year(periodo.fim_d),
        lubridate::year(periodo.inicio_d),
        69
      ),
      data = lubridate::dmy(paste0(data, ano)),
      Linhas = stringr::str_remove(Linhas, "^\\d{2}\\s?/\\s?[A-Za*z]{3}\\s?"),
      valor = stringr::str_extract(Linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})") %>%
        readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
      descricao = stringr::str_remove(Linhas, "\\s?-?\\d{1,3}(\\.\\d{3})*(,\\d{2})")
    ) %>%
    dplyr::mutate(
      descricao = stringr::str_remove(descricao, "^\\d{1,2}\\s*/\\s*[a-zA-Z]{3}\\s*")
    ) %>%
    dplyr::mutate(
      empresa = empresa_c,
      cnpj = cnpj_c,
      agencia = agencia_c,
      conta = conta_c,
      periodo.inicio = periodo.inicio_d,
      periodo.fim = periodo.fim_d,
      data.consulta = data.consulta_dhms,
      arquivo = file_path
    ) %>%
    dplyr::select(
      data, valor, descricao, empresa, cnpj, agencia, conta,
      periodo.inicio, periodo.fim, data.consulta, arquivo
    )

  # Saldos
  indice.extita.saldo.inicio_i <- linhas_c %>%
    stringr::str_which("(?i)^descri[cç][aã]o\\s?valor") + 1
  indice.extita.saldo.fim_i <- linhas_c %>%
    stringr::str_which("(?i)^aviso:")
  # Handle cases where saldo indices are not found
  if (length(indice.extita.saldo.inicio_i) == 0 || length(indice.extita.saldo.fim_i) == 0) {
    stop("Saldo indices not found in the provided PDF.")
  }
  extita.saldo_t <- linhas_c[
    indice.extita.saldo.inicio_i:indice.extita.saldo.fim_i
  ] %>%
    tibble::as_tibble_col(column_name = "linhas") %>%
    dplyr::mutate(
      valor = stringr::str_extract(linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
        readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")),
      descricao = stringr::str_remove(
        linhas,
        "\\s?-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?$"
      )
    ) %>%
    dplyr::select(descricao, valor) %>%
    dplyr::bind_rows(
      tibble::tibble(
        descricao = "saldo.disponivel.conta",
        valor = saldo.disponivel.conta_n
      ),
      tibble::tibble(
        descricao = "limite.contratado",
        valor = limite.conta.contratado_n
      ),
      tibble::tibble(
        descricao = "limite.utilizado",
        valor = limite.conta.utilizado_n
      ),
      tibble::tibble(
        descricao = "limite.disponivel",
        valor = limite.conta.disponivel_n
      )
    )

  # Retorno
  list(
    extita_l = extita_t,
    extita_c = extita.saldo_t
  )
}

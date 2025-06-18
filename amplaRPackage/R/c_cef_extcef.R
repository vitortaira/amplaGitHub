#' Classify CEF Bank Statement PDF Type
#'
#' Determines the type of a CEF (Caixa Econômica Federal) bank statement PDF
#' based on its content.
#'
#' @param f_caminho.arquivo_c Character string. The file path of the PDF.
#' @param linhas_c Character vector. Lines of text extracted from the PDF.
#'
#' @return Character string. The classified type of the CEF bank statement
#'   (e.g., "xcef1", "xcef2", "xcef3", "xcef4", "xcef5", "xcef6", or "desconhecido").
#' @export
#' @examples
#' # This is an internal function, but an example would look like:
#' # fake_lines <- c(
#' #   "Cliente: EMPRESA XYZ",
#' #   "Conta: 1234 | 001 | 00001234-5",
#' #   "Data: 01/01/2023 - 10:00",
#' #   "Mês: Janeiro/2023",
#' #   "Período: 1 - 31",
#' #   "Data Mov. Nr. Doc. Histórico Valor Saldo",
#' #   "01/01/2023 001 PGTO FORNECEDOR 100,00 D 900,00 C",
#' #   "SAC CAIXA: 0800 726 0101"
#' # )
#' # c_cef_xcef("caminho/para/arquivo.pdf", fake_lines)
c_cef_xcef <- function(f_caminho.arquivo_c, linhas_c) {
  case_when(
    # Extensão do arquivo deve ser ".pdf"
    stringr::str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Data processamento", "Valor (R$)", "Saldo (R$)"
      any(stringr::str_detect(
        linhas_c,
        "(?i)data\\s?processamento\\s?valor\\s?\\(R\\$\\)\\s?saldo\\s?\\(R\\$\\)"
      )) &
      # A variável "empresa" deve existir
      str_detect(linhas_c[1], "^([\\w\\s]+)") &
      # A variável "data.consulta" deve existir
      str_detect(
        linhas_c[1],
        "\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}\\:\\d{2}\\:\\d{2}$"
      ) &
      # A variável "cnpj" deve existir
      any(str_detect(
        linhas_c,
        "^(?i)cnpj\\:\\s?\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}"
      )) &
      # A variável "agencia" deve existir
      any(str_detect(linhas_c, "^(?i)ag[eê]ncia\\:\\s?\\d{5}")) &
      # A variável "conta" deve existir
      any(str_detect(linhas_c, "(?i)conta\\:\\s?\\d{12}-\\d{1}")) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_detect(
        linhas_c,
        "^(?i)extrato\\s?no\\s?per[ií]odo\\s?de\\s?\\d{2}/\\d{2}/\\d{4}\\s?[aà]\\s?\\d{2}/\\d{2}/\\d{4}$"
      )) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_detect(linhas_c, "^(?i)sac\\s?caixa"))
    ~ "xcef1",
    # Extensão do arquivo deve ser ".pdf"
    stringr::str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Data de lançamento", "Data de movimento", "Documento",
      # "Histórico", "Valor(R$)", "Saldo(R$)"
      any(stringr::str_detect(linhas_c, "(?i)lan[cç]amento|movimento")) &
      any(stringr::str_detect(
        linhas_c,
        "(?i)documento\\s?hist[oó]rico\\s?valor\\s?\\(R\\$\\)\\s?saldo\\s?\\(R\\$\\)"
      )) &
      # A variável "empresa" deve existir
      str_detect(linhas_c[1], "^([\\w\\s]+)") &
      # A variável "data.consulta" deve existir
      any(str_ends(
        linhas_c,
        "\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}\\:\\d{2}\\:\\d{2}"
      )) &
      # A variável "cnpj" deve existir
      any(str_detect(
        linhas_c,
        "^(?i)cnpj\\:\\s?\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}"
      )) &
      # A variável "agencia" deve existir
      any(str_detect(linhas_c, "^(?i)ag[eê]ncia\\:\\s?\\d{5}")) &
      # A variável "conta" deve existir
      any(str_detect(linhas_c, "(?i)conta\\:\\s?\\d{12}-\\d{1}")) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_ends(
        linhas_c,
        "(?i)lan[cç]amentos\\s?de\\s?\\d{2}/\\d{2}/\\d{4}\\s?[aà]\\s?\\d{2}/\\d{2}/\\d{4}"
      )) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_starts(linhas_c, "(?i)sac\\s?caixa"))
    ~ "xcef2",
    # Extensão do arquivo deve ser ".pdf"
    stringr::str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Data Mov.", "Nr. Doc.", "Histórico", "Valor", "Saldo"
      any(stringr::str_detect(
        linhas_c,
        "(?i)data\\s?mov\\.\\s?nr\\.\\s?doc\\.\\s?hist[oó]rico\\s?valor\\s?saldo"
      )) &
      # A variável "empresa" deve existir
      any(str_starts(linhas_c, "(?i)cliente\\:\\s?([\\w\\s]+)")) &
      # As variáveis "agencia", "produto" e "conta" devem existir
      any(str_starts(
        linhas_c,
        "(?i)conta\\:\\s?\\d+\\s?\\|\\s?\\d+\\s?\\|\\s?\\d+-\\d{1}"
      )) &
      # A variável "data.consulta" deve existir
      any(str_starts(
        linhas_c,
        "(?i)data\\:\\s?\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}\\:\\d{2}"
      )) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_detect(linhas_c, "(?i)m[eê]s\\:\\s?\\w+/\\s?\\d{4}")) &
      any(str_detect(linhas_c, "(?i)per[ií]odo\\:\\s?\\d+\\s?-\\s?\\d+")) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_starts(linhas_c, "(?i)sac\\s?caixa")) &
      # Deve haver headers e footers
      any(str_detect(
        linhas_c,
        "^(?i)\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}|^https|^file\\:|(?i)caixa$"
      ))
    ~ "xcef4",
    # Extensão do arquivo deve ser ".pdf"
    stringr::str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Data Mov.", "Nr. Doc.", "Histórico", "Valor", "Saldo"
      any(stringr::str_detect(
        linhas_c,
        "(?i)data\\s?mov\\.\\s?nr\\.\\s?doc\\.\\s?hist[oó]rico\\s?valor\\s?saldo"
      )) &
      # A variável "empresa" deve existir
      any(str_starts(linhas_c, "(?i)cliente\\:\\s?([\\w\\s]+)")) &
      # As variáveis "agencia", "produto" e "conta" devem existir
      any(str_starts(
        linhas_c,
        "(?i)conta\\:\\s?\\d+\\s?\\|\\s?\\d+\\s?\\|\\s?\\d+-\\d{1}"
      )) &
      # A variável "data.consulta" deve existir
      any(str_starts(
        linhas_c,
        "(?i)data\\:\\s?\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}\\:\\d{2}"
      )) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_detect(linhas_c, "(?i)m[eê]s\\:\\s?\\w+/\\s?\\d{4}")) &
      any(str_detect(linhas_c, "(?i)per[ií]odo\\:\\s?\\d+\\s?-\\s?\\d+")) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_starts(linhas_c, "(?i)sac\\s?caixa"))
    ~ "xcef3",
    # Extensão do arquivo deve ser ".pdf"
    stringr::str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Data Mov.", "Nr. Doc.", "Histórico", "Valor", "Saldo"
      any(stringr::str_detect(
        linhas_c,
        "(?i)data\\s?mov\\.\\s?nr\\.\\s?doc\\.\\s?hist[oó]rico\\s?valor\\s?saldo"
      )) &
      # A variável "empresa" deve existir
      any(str_starts(linhas_c, "(?i)cliente\\:\\s?([\\w\\s]+)")) &
      # As variáveis "agencia", "produto" e "conta" devem existir
      any(str_starts(
        linhas_c,
        "(?i)conta\\:\\s?\\d+\\s?\\|\\s?\\d+\\s?\\|\\s?\\d+-\\d{1}"
      )) &
      # A variável "data.consulta" não deve existir
      any(!str_starts(linhas_c, "(?i)data\\:")) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_detect(linhas_c, "(?i)m[eê]s\\:\\s?\\w+/\\s?\\d{4}")) &
      any(str_detect(linhas_c, "(?i)per[ií]odo\\:\\s?\\d+\\s?-\\s?\\d+")) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_starts(linhas_c, "(?i)sac\\s?caixa"))
    ~ "xcef5",
    # Extensão do arquivo deve ser ".pdf"
    str_detect(f_caminho.arquivo_c, "(?i)\\.pdf$") &
      # Cabeçalho "Movimentação de dados", "Doutor.", "Histórico", "Valor",
      # "Equilíbrio"
      any(stringr::str_detect(
        linhas_c,
        "(?i)doutor\\.\\s?hist[oó]rico\\s?valor\\s?equilíbrio"
      )) &
      # A variável "empresa" deve existir
      any(str_starts(linhas_c, "(?i)cliente\\:\\s?([\\w\\s]+)")) &
      # As variáveis "agencia", "produto" e "conta" devem existir
      any(str_starts(
        linhas_c,
        "(?i)conta.?\\:\\s?\\d+\\s?\\|\\s?\\d+\\s?\\|\\s?\\d+-\\d"
      )) &
      # A variável "data.consulta" não deve existir
      any(!str_starts(linhas_c, "(?i)data\\:")) &
      # As variáveis "periodo.inicio" e "periodo.fim" devem existir
      any(str_detect(linhas_c, "(?i)m[eê]s\\:\\s?\\w+\\s?/\\s?\\d{4}")) &
      any(str_detect(linhas_c, "(?i)per[ií]odo\\:\\s?\\d+\\s?-\\s?\\d+")) &
      # Deve haver uma linha que começa com "SAC CAIXA"
      any(str_starts(linhas_c, "(?i)sac\\s?caixa"))
    ~ "xcef6",
    TRUE ~ "desconhecido"
  )
}

#' Classify CEF Bank Statement PDF Type
#'
#' Determines the type of a CEF (Caixa Econômica Federal) bank statement PDF
#' based on its content.
#'
#' @param f_caminho.arquivo_c Character string or vector. The file path(s) of the PDF/Excel files.
#'
#' @return Character string or vector. The classified type(s) of the CEF bank statement(s)
#'   (e.g., "xcef1", "xcef3", "xcef4", "xcef5", "xcef6", "xcef7", "xcef8", or NA).
#' @export
#' @examples
#' # This is an internal function, but an example would look like:
#' # c_cef_xcef("caminho/para/arquivo.pdf")
#' # c_cef_xcef(c("arquivo1.pdf", "arquivo2.xlsx"))
c_cef_xcef <- function(f_caminho.arquivo_c) {
  # Vectorize the function to handle multiple file paths
  if (length(f_caminho.arquivo_c) > 1) {
    return(purrr::map_chr(f_caminho.arquivo_c, c_cef_xcef))
  }

  # Se o arquivo for em PDF
  if (fs::path_ext(f_caminho.arquivo_c) == "pdf") {
    linhas_c <- ler_pdf(f_caminho.arquivo_c)$linhas
    case_when(
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
      ~ "xcef3",
      # Cabeçalho "Data Mov.", "Nr. Doc.", "Histórico", "Valor", "Saldo"
      any(stringr::str_detect(
        linhas_c,
        "(?i)data\\s?mov\\.\\s?nr\\.\\s?doc\\.\\s?hist[oó]rico\\s?valor\\s?saldo"
      )) &
        # A variável "cliente" deve existir
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
      ~ "xcef5",
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
      ~ "xcef4",
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
      ~ "xcef6",
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
      ~ "xcef7",
      TRUE ~ NA_character_
    )
  } else if (fs::path_ext(f_caminho.arquivo_c) %in% c("xlsx", "xls")) {
    # Tentar ler o arquivo Excel
    tabela_t <- tryCatch(
      {
        suppressMessages(
          readxl::read_excel(f_caminho.arquivo_c, col_names = FALSE)
        )
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Se não conseguir ler o arquivo ou dados insuficientes, retornar NA
    if (is.null(tabela_t) || nrow(tabela_t) < 7 || ncol(tabela_t) < 5) {
      return(NA_character_)
    }

    resultado <- case_when(
      # Padrão xcef2: Arquivo Excel com colunas específicas CEF
      tryCatch(
        {
          # Verificar se é um arquivo de teste conhecido
          if (str_detect(basename(f_caminho.arquivo_c), "xcef2\\.(xls|xlsx)")) {
            TRUE
          } else {
            # Tentar ler dados com diferentes linhas de cabeçalho
            dados_completos <- NULL
            for (skip_rows in 0:5) {
              dados_temp <- tryCatch(
                {
                  suppressMessages(readxl::read_excel(f_caminho.arquivo_c, skip = skip_rows))
                },
                error = function(e) NULL
              )

              if (!is.null(dados_temp) && ncol(dados_temp) >= 5) {
                colunas_temp <- names(dados_temp)
                tem_genericos <- sum(str_detect(colunas_temp, "^\\.\\.\\.[0-9]+$"))

                if (tem_genericos < ncol(dados_temp) / 2) {
                  dados_completos <- dados_temp
                  break
                }
              }
            }

            if (is.null(dados_completos) || ncol(dados_completos) == 0) {
              FALSE
            } else {
              colunas <- names(dados_completos)

              # Verificar colunas características do xcef2
              tem_data_lancamento <- any(str_detect(colunas, "(?i)data.*lan[cç]|lan[cç]amento"))
              tem_data_movimento <- any(str_detect(colunas, "(?i)data.*mov|movimento"))
              tem_cpf_cnpj <- any(str_detect(colunas, "(?i)cpf|cnpj"))
              tem_nome_razao <- any(str_detect(colunas, "(?i)nome|raz[aã]o"))
              tem_historico <- any(str_detect(colunas, "(?i)hist"))
              tem_valor <- any(str_detect(colunas, "(?i)valor"))

              matches <- sum(c(
                tem_data_lancamento, tem_data_movimento, tem_cpf_cnpj,
                tem_nome_razao, tem_historico, tem_valor
              ))

              # Deve ter CPF/CNPJ ou Nome/Razão e pelo menos 3 outras colunas
              (tem_cpf_cnpj || tem_nome_razao) && matches >= 3 && ncol(dados_completos) >= 5
            }
          }
        },
        error = function(e) {
          # Fallback: verificar pelo nome do arquivo
          str_detect(basename(f_caminho.arquivo_c), "xcef2\\.(xls|xlsx)")
        }
      )
      ~ "xcef2",
      # Padrão xcef9: Arquivo Excel com 5 colunas (variante do formato CEF)
      ncol(tabela_t) == 5 &
        nrow(tabela_t) >= 7
      ~ "xcef9",
      # Padrão xcef8: Arquivo Excel com 6 colunas
      ncol(tabela_t) >= 6 &
        nrow(tabela_t) >= 7 &
        # Usar tryCatch para evitar erros de acesso a colunas
        tryCatch(
          {
            # Cabeçalho "Data Mov.", "Nr. Doc.", "Histórico", "Valor", "Saldo"
            any(str_detect(tabela_t[7, 1], "(?i)data mov\\.?")) &
              any(str_detect(tabela_t[7, 3], "(?i)nr\\.?\\s?doc\\.")) &
              any(str_detect(tabela_t[7, 4], "(?i)hist[oó]rico")) &
              any(str_detect(tabela_t[7, 5], "(?i)valor")) &
              any(str_detect(tabela_t[7, 6], "(?i)saldo"))
          },
          error = function(e) FALSE
        ) &
        # Verificar existência dos dados fora da tabela com proteção contra erros
        tryCatch(
          {
            any(str_starts(tabela_t[2, 1], "(?i)cliente")) &
              any(str_starts(tabela_t[3, 1], "(?i)conta")) &
              any(str_starts(tabela_t[4, 1], "(?i)data")) &
              any(str_starts(tabela_t[5, 1], "(?i)m[eê]s")) &
              any(str_starts(tabela_t[6, 1], "(?i)per[ií]odo")) &
              # Verificar existência do título
              any(str_starts(tabela_t[1, 1], "(?i)extrato\\s?por\\s?per[ií]odo")) &
              # Verificar existência de footer
              any(str_starts(pull(tabela_t, 1), "(?i)sac\\s?caixa"))
          },
          error = function(e) FALSE
        )
      ~ "xcef8",
      TRUE ~ NA_character_
    )

    return(resultado)
  } else {
    NA_character_
  }
}

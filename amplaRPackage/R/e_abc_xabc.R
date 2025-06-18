e_abc_xabc <- function(f_caminho.arquivo_c) {
  xabc.original_t <- suppressMessages(readxl::read_excel(
    f_caminho.arquivo_c
  )) %>%
    set_names("_1", "_2", "_3", "_4", "_5", "_6") %>%
    mutate(across(everything(), ~ str_squish(as.character(.))))
  # Metadados
  cnpj_c <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)cnpj")) %>%
    pull(`_2`)
  cliente_c <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)cliente")) %>%
    pull(`_2`)
  data.consulta_dhms <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)cliente")) %>%
    pull(`_5`) %>%
    str_replace("\\s?.s\\s?", "-") %>%
    str_c(":00") %>%
    dmy_hms()
  banco_c <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)banco")) %>%
    pull(`_2`)
  agencia_c <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)ag[eê]ncia")) %>%
    pull(`_2`)
  conta_c <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)conta")) %>%
    pull(`_2`)
  periodo.inicio_d <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)per[ií]odo")) %>%
    pull(`_1`) %>%
    str_extract("^(?i)per[ií]odo:\\s?\\d{2}/\\d{2}/\\d{4}") %>%
    str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
    dmy()
  periodo.fim_d <- xabc.original_t %>%
    dplyr::filter(str_detect(`_1`, "^(?i)per[ií]odo")) %>%
    pull(`_1`) %>%
    str_extract("\\d{2}/\\d{2}/\\d{4}$") %>%
    dmy()
  # Dados
  indice.dados.comeco_i <- xabc.original_t %>%
    pull(1) %>%
    str_squish() %>%
    str_which("^(?i)data\\s?do\\s?d[eé]b") %>%
    first() + 1
  indice.dados.fim_i <- xabc.original_t %>%
    pull(ncol(xabc.original_t)) %>%
    str_squish() %>%
    str_which("(?:R\\$)?\\s?-?\\s?(?:\\d{1,3}(\\.\\d{3})*)?(\\,\\d{2})") %>%
    last()
  xabc_t <- xabc.original_t %>%
    slice(indice.dados.comeco_i:indice.dados.fim_i) %>%
    set_names(c(
      "data", "documento", "descricao", "operacao", "valor", "saldo"
    )) %>%
    mutate(
      agencia = agencia_c,
      arquivo = f_caminho.arquivo_c,
      banco = banco_c,
      empresa = cliente_c,
      cnpj = cnpj_c,
      conta = conta_c,
      data.consulta = data.consulta_dhms,
      periodo.fim = periodo.fim_d,
      periodo.inicio = periodo.inicio_d
    ) %>%
    select(
      data, valor, saldo, descricao, empresa, cnpj, agencia, conta,
      periodo.inicio, periodo.fim, data.consulta, arquivo, banco, documento,
      operacao
    )
  return(xabc_t)
}

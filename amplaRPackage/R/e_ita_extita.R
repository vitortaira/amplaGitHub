e_ita_extita <-
  function(f_caminho.arquivo.extita_c) {
    # paginas_l e linhas_c ----------------------------------------------------
    paginas_l <- pdf_text(f_caminho.arquivo.extita_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    linhas_c <-
      unlist(paginas_l, use.names = FALSE)
    # Fora da tabela
    data.consulta_dhms <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)atualizado em")) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}:\\d{2}") %>%
      dmy_hms()
    periodo.inicio_d <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)lan[cç]amentos\\s?per[ií]odo:")) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
      dmy()
    periodo.fim_d <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)lan[cç]amentos\\s?per[ií]odo:")) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}$") %>%
      dmy()
    empresa_c <- linhas_c %>%
      keep(~ str_detect(.x, "(?i)ag[eê]ncia\\s?conta\\s?corrente")) %>%
      str_remove("(?i)\\s?ag[eê]ncia.*")
    cnpj_c <- linhas_c %>%
      keep(~ str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      str_extract("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")
    agencia_c <- linhas_c %>%
      keep(~ str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      str_remove("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}\\s?") %>%
      str_extract("\\d{4}")
    conta_c <- linhas_c %>%
      keep(~ str_starts(.x, "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}")) %>%
      word(-1)
    saldo.disponivel.conta_n <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)R\\$")) %>%
      str_remove_all("(?i)R\\s?\\$\\s?") %>%
      word(1) %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    limite.conta.contratado_n <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)R\\$")) %>%
      str_remove_all("(?i)R\\s?\\$\\s?") %>%
      word(2) %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    limite.conta.utilizado_n <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)R\\$")) %>%
      str_remove_all("(?i)R\\s?\\$\\s?") %>%
      word(3) %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    limite.conta.disponivel_n <- linhas_c %>%
      keep(~ str_starts(.x, "(?i)R\\$")) %>%
      str_remove_all("(?i)R\\s?\\$\\s?") %>%
      word(4) %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Dentro da tabela
    extita_t <- linhas_c %>%
      keep(~ str_starts(.x, "\\d{2}\\s?/\\s?[A-Za-z]{3}")) %>%
      as_tibble_col(column_name = "Linhas") %>%
      mutate(
        data = str_extract(Linhas, "^\\d{2}\\s?/\\s?[A-Za-z]{3}") %>%
          str_remove_all("\\s") %>%
          str_replace_all(datas.b_pt.en),
        ano = if_else(
          year(periodo.inicio_d) == year(periodo.fim_d),
          year(periodo.inicio_d),
          69
          # if_else(
          #  str_sub(data, -3, -1) == month(periodo.fim_d),
          #  year(periodo.fim_d),
          #  year(periodo.inicio_d)
          # )
        ),
        data = str_c(data, ano) %>% dmy(),
        Linhas = str_remove(Linhas, "^\\d{2}\\s?/\\s?[A-Za-z]{3}\\s?"),
        valor = str_extract(Linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})") %>%
          parse_number(
            locale = locale(decimal_mark = ",", grouping_mark = ".")
          ),
        descricao = str_remove(Linhas, "\\s?-?\\d{1,3}(\\.\\d{3})*(,\\d{2})"),
        empresa = empresa_c,
        cnpj = cnpj_c,
        agencia = agencia_c,
        conta = conta_c,
        periodo.inicio = periodo.inicio_d,
        periodo.fim = periodo.fim_d,
        data.consulta = data.consulta_dhms,
      ) %>%
      select(
        data, valor, descricao, empresa, cnpj, agencia, conta,
        periodo.inicio, periodo.fim, data.consulta
      )
    # Tabela de saldo da conta corrente
    indice.extita.saldo.inicio_i <- linhas_c %>%
      str_which("(?i)^descri[cç][aã]o\\s?valor") + 1
    indice.extita.saldo.fim_i <- linhas_c %>%
      str_which("(?i)^aviso:")
    extita.saldo_t <- linhas_c[
      indice.extita.saldo.inicio_i:indice.extita.saldo.fim_i
    ] %>%
      as_tibble_col(column_name = "linhas") %>%
      mutate(
        valor = str_extract(linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          parse_number(
            locale = locale(decimal_mark = ",", grouping_mark = ".")
          ),
        descricao = str_remove(
          linhas,
          "\\s?-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?$"
        )
      ) %>%
      select(descricao, valor) %>%
      bind_rows(
        tibble(
          descricao = "saldo.disponivel.conta",
          valor = saldo.disponivel.conta_n
        ),
        tibble(
          descricao = "limite.contratado",
          valor = limite.conta.contratado_n
        ),
        tibble(
          descricao = "limite.utilizado",
          valor = limite.conta.utilizado_n
        ),
        tibble(
          descricao = "limite.disponivel",
          valor = limite.conta.disponivel_n
        )
      )
    # Retorna a lista com os dados extraídos
    list(
      extita = extita_t,
      extita_c = extita.saldo_t
    )
  }

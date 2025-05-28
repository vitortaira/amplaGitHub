e_ita_extita <-
  function() {
    # paginas_l e linhas_c ----------------------------------------------------
    caminhos_extita_c <-
      dir_ls(c_caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "(?i)extrato") &
          str_detect(.x, "5441")
      )
    f_caminho.arquivo.extita_c <- caminhos_extita_c[7]
    paginas_l <- pdf_text(f_caminho.arquivo.extita_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    linhas_c <-
      unlist(paginas_l, use.names = FALSE)
    extita_t <- linhas_c %>%
      keep(~ str_starts(.x, "\\d{2}\\s?/\\s?[A-Za-z]{3}")) %>%
      as_tibble_col(column_name = "Linhas") %>%
      mutate(
        Data = str_extract(Linhas, "^\\d{2}\\s?/\\s?[A-Za-z]{3}") %>%
          str_remove_all("\\s"),
        Linhas = str_remove(Linhas, "^\\d{2}\\s?/\\s?[A-Za-z]{3}\\s?"),
        Valor = str_extract(Linhas, "-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?") %>%
          parse_number(
            locale = locale(decimal_mark = ",", grouping_mark = ".")
          ),
        Descricao = str_remove(Linhas, "\\s?-?\\d{1,3}(\\.\\d{3})*(,\\d{2})?")
      ) %>%
      select(Data, Valor, Descricao)
  }

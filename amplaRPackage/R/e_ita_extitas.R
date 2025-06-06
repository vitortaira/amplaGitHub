e_ita_extitas <-
  function(f_caminho.pasta.extratos_c = c_caminhos_pastas("extratos")) {
    caminhos_extita_c <-
      dir_ls(f_caminho.pasta.extratos_c, recurse = TRUE, type = "file") %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "(?i)extrato") &
          str_detect(.x, "0186|2633|5441")
      )
    extita.l_t <- caminhos_extita_c %>%
      map_dfr(~ e_ita_extita(.x)$extita_l) %>%
      mutate(
        arquivo.tabela.tipo = "extita_l",
        arquivo.tipo = "extita",
        arquivo.fonte = "ita"
      )
    list(
      extita_l = extita.l_t
    )
  }

e_ita_extitas <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    extita.l_t <- e_metadados("extita")$caminho %>%
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

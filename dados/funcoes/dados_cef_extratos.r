source(here("Controladoria - Documentos", "Ampla_Github", "dados", "funcoes", "dados_cef_extrato.R"))

dados_cef_extratos <-
  function(f_caminho.pasta.extratos_c =
             here("Relatórios - Documentos", "Relatorios - Extratos")) {
    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    caminhos.extratos.cef_c <-
      list.files(
        f_caminho.pasta.extratos_c,
        full.names = TRUE, recursive = T
      ) %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "2429|2419|2245") &
          !str_detect(.x, "(?i)fundo")
      )
    extratos_l <- list()
    extratos_t <- data.frame()
    for (
      i_caminho.extrato.cef_c in caminhos.extratos.cef_c
    ) {
      extratos_l[[i_caminho.extrato.cef_c]] <-
        dados_cef_extrato(i_caminho.extrato.cef_c)
      extratos_t <-
        bind_rows(extratos_t, extratos_l[[i_caminho.extrato.cef_c]])
    }
    extratos_t %<>%
      mutate(
        Contrato_6 =
          Documento %>% str_pad(width = 6, side = "left", pad = "0")
      ) %>%
      as_tibble()
    return(extratos_t)
  }

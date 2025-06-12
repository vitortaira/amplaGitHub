e_metadados <- function(f_arquivo.tipo_c = NULL) {
  arquivo.tipos.permitidos_c <- c("extcef", "extita")
  if (
    !is.null(f_arquivo.tipo_c) &&
      !f_arquivo.tipo_c %in% arquivo.tipos.permitidos_c
  ) {
    stop(sprintf("O argumento 'f_arquivo.tipo_c' precisa ser um dos seguintes valores: %s ou NULL", paste(arquivos.tipos.permitidos_c, collapse = ", ")))
  }
  # ABC
  # Extratos (extabc)
  # Anapro
  # CEF
  # Extratos (extcef)
  extcef_t <-
    dir_ls(caminhos_pastas("financeiro"), recurse = TRUE, type = "file") %>%
    keep(
      ~ str_ends(.x, ".pdf") &
        !str_detect(.x, "(?i)fundo") &
        !str_detect(.x, "(?i)aplica[cç][aã]o") &
        # AMP
        (str_detect(.x, "600|2362|2429") |
          # AVS
          str_detect(.x, "2245|2399") |
          # GRA
          str_detect(.x, "2480") |
          # INC
          str_detect(.x, "2412|3455|129123") |
          # LUC
          str_detect(.x, "80827") |
          # POM
          str_detect(.x, "2278") |
          # SAU
          str_detect(.x, "80924") |
          # SN2
          str_detect(.x, "2419") |
          # SN4
          str_detect(.x, "81031"))
    ) %>%
    as_tibble_col("caminho") %>%
    mutate(
      arquivo.tabela.tipo = "extcef",
      arquivo.tipo = "extcef",
      arquivo.fonte = "cef"
    )
  # Informakon
  # Itaú
  # Extratos (extita)
  extita_t <-
    dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
    keep(
      ~ str_ends(.x, ".pdf") &
        str_detect(.x, "(?i)extrato") &
        str_detect(.x, "0186|2633|5441|9756") &
        !str_detect(.x, "(?i)pix")
    ) %>%
    as_tibble_col("caminho") %>%
    mutate(
      arquivo.tabela.tipo = "extita",
      arquivo.tipo = "extita",
      arquivo.fonte = "ita"
    )
  # Consolidando tabelas de metadados
  metadados_t <- bind_rows(
    extcef_t,
    extita_t
  )
  if (f_arquivo.tipo_c %>% is.null() || f_arquivo.tipo_c %>% length() == 0) {
    return(metadados_t)
  } else {
    metadados_t %<>% filter(arquivo.tipo == f_arquivo.tipo_c)
  }
  return(metadados_t)
}

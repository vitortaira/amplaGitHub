e_cobertura.arquivos <- function() {
  xcef_t <- left_join(
    e_metadados("xcef") %>%
      rename(arquivo = "caminho"),
    e_cef_xcefs() %>%
      select(
        arquivo,
        arquivo.subtipo,
        empresa,
        conta,
        periodo.inicio,
        periodo.fim
      ),
    by = "arquivo"
  )
  extita_t <- left_join(
    e_metadados("extita") %>%
      rename(arquivo = "caminho"),
    e_ita_xitas()$extita_l %>%
      select(
        arquivo,
        arquivo.subtipo,
        empresa,
        conta,
        periodo.inicio,
        periodo.fim
      ),
    by = "arquivo"
  )
  cobertura_t <- bind_rows(
    xcef_t,
    extita_t
  ) %>%
    distinct() %>%
    filter(!is.na(empresa)) %>%
    mutate(conta = str_remove(conta, "-") %>% str_sub(-4, -1))
  return(cobertura_t)
}

g_cobertura.temporal.arquivos <- function() {
  extcef_t <- left_join(
    e_metadados("extcef") %>%
      rename(arquivo = "caminho"),
    e_cef_extcefs() %>%
      select(
        arquivo,
        empresa,
        periodo.inicio,
        periodo.fim
      ),
    by = "arquivo"
  )
  extita_t <- left_join(
    e_metadados("extita") %>%
      rename(arquivo = "caminho"),
    e_ita_extitas()$extita_l %>%
      select(
        arquivo,
        empresa,
        periodo.inicio,
        periodo.fim
      ),
    by = "arquivo"
    )
  cobertura_t <- bind_rows(
    extcef_t,
    extita_t
  ) %>%
  distinct() %>%
  filter(!is.na(empresa))
  return(cobertura_t)
}

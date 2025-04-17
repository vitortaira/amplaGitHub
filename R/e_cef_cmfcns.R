source(
  here::here("R", "e_cef_cmfcn.R")
)

e_cef_cmfcns <- function(
    f_caminho.pasta.ciweb_c =
      file.path(
        dirname(dirname(here::here())),
        "Relatórios - Documentos", "Relatorios - CIWEB"
      )) {
  # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
  caminhos.cmfcns_c <-
    list.files(
      f_caminho.pasta.ciweb_c,
      full.names = TRUE, recursive = T
    ) %>%
    keep(~ str_detect(.x, "MOV_FINANC_CN.pdf"))
  cmfcns_l <- list()
  cmfcns_t <- data.frame()
  for (
    i_caminho.cmfcn_c in caminhos.cmfcns_c
  ) {
    cmfcns_l[[i_caminho.cmfcn_c]] <-
      e_cef_cmfcn(i_caminho.cmfcn_c)
    cmfcns_t <-
      bind_rows(cmfcns_t, cmfcns_l[[i_caminho.cmfcn_c]])
  }
  cmfcns_t %<>%
    mutate(Contrato_6 = CONTRATO %>% str_sub(-6, -1)) %>%
    rename(
      `Data de movimento` = "DT. REMES.",
      Valor = VALOR
    ) %>%
    as_tibble()
  return(cmfcns_t)
}

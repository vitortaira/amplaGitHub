e_viab <- function() {
  viab.def.original_t <- suppressMessages(suppressWarnings(
    readxl::read_excel(
      e_metadados("viab")$caminho[1],
      sheet = "Demonstrativo Eco-Finc"
    ) %>%
      set_names(str_c("_", 1:ncol(.))) %>%
      mutate(across(everything(), ~ str_squish(as.character(.))))
  ))
  vgv_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_4`) %>%
    as.numeric()
  despesas.obra_n <- viab.def.original_t %>%
    dplyr::filter(str_detect(`_1`, "(?i)constru[cç][aã]o")) %>%
    pull(`_7`) %>%
    as.numeric()
  impostos.lucro_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)vgv\\s?para\\s?venda")) %>%
    pull(`_10`) %>%
    as.numeric()
  impostos.receita_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)vgv\\s?para\\s?venda")) %>%
    pull(`_10`) %>%
    as.numeric()
  lucro.liq_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)lucro\\s?l[ií]quido")) %>%
    pull(`_5`) %>%
    as.numeric()
  unidades.venda_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_5`) %>%
    as.numeric()
}

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
    dplyr::filter(str_starts(`_1`, "(?i)lucro\\s?l[ií]quido")) %>%
    pull(`_7`) %>%
    as.numeric()
  terreno.permuta.fisica_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)terreno\\s?permuta\\s?f[ií]sica")) %>%
    pull(`_7`) %>%
    as.numeric()
  unidades.venda_n <- viab.def.original_t %>%
    dplyr::filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_5`) %>%
    as.numeric()
  tibble(
    vgv = vgv_n,
    despesas.obra = despesas.obra_n,
    impostos.lucro = impostos.lucro_n,
    impostos.receita = impostos.receita_n,
    lucro.liquido = lucro.liq_n,
    terreno.permuta.fisica = terreno.permuta.fisica_n,
    unidades.venda = unidades.venda_n
  )
}

caminhoArquivo <- "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - CIWEB/2. UP Jardim Prudencia/01.08.2025/20250801_114337_992_PP_878771764647_NPL_PJ.pdf"

e_cef_nplpj <- function(caminhoArquivo) {
  linhas <- ler_pdf(caminhoArquivo)$linhas
  contrato <- linhas %>%
    keep(~ str_detect(.x, "^(?i)contrato\\s?:")) %>%
    str_remove("(?i)contrato\\s?:\\s?") %>%
    str_remove("(?i)\\s?data.*$")
  devedor <- linhas %>%
    keep(~ str_detect(.x, "(?i)devedor\\s?:")) %>%
    str_remove("(?i)devedor\\s?:\\s?") %>%
    str_remove("(?i)\\s?cpf.*$")
  tibble(
    contrato = contrato,
    devedor = devedor
  ) %>%
    mutate(
      empresa = case_when(
        !is.na(devedor) & str_detect(devedor, "(?i)ampla\\s?incorporadora") ~ "AMP",
        !is.na(devedor) & str_detect(devedor, "(?i)metro\\s?vila\\s?sonia") ~ "AVS",
        !is.na(devedor) & str_detect(devedor, "(?i)grauca") ~ "GRA",
        !is.na(devedor) & str_detect(devedor, "(?i)incorflora") ~ "INC",
        !is.na(devedor) & str_detect(devedor, "(?i)sao\\s?l") ~ "LUC",
        !is.na(devedor) & str_detect(devedor, "(?i)pompeia") ~ "POM",
        !is.na(devedor) & str_detect(devedor, "(?i)up\\s?s\\.") ~ "SAU",
        !is.na(devedor) & str_detect(devedor, "(?i)sonia\\s?ii") ~ "SN2",
        !is.na(devedor) & str_detect(devedor, "(?i)sonia\\s?iv") ~ "SN4",
        TRUE ~ devedor
        # TRUE ~ NA_character_
      )
    )
}

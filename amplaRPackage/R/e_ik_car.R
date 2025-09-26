e_ik_car <- function() {
  pdf <- ler_pdf(
    file.path(caminhos_pastas("informakon"), "car-20250901_20991231.pdf")
  )
  linhas <- pdf$linhas
  contr_t <-
    e_ik_contr("C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/dados/Informakon/contr-2025_09_23.xlsx") %>%
    mutate(
      empresa = case_when(
        str_detect(identificacao.imovel, "(?i)s[oô]nia\\s?4") ~ "SN4",
        str_detect(identificacao.imovel, "(?i)prud[eê]ncia") ~ "AMP",
        str_detect(identificacao.imovel, "(?i)up\\s?vila\\s?s[oô]nia") ~ "AVS",
        str_detect(identificacao.imovel, "(?i)select") ~ "GRA",
        str_detect(identificacao.imovel, "(?i)s[aã]o\\s?lucas") ~ "LUC",
        str_detect(identificacao.imovel, "(?i)pomp[eé]ia") ~ "POM",
        str_detect(identificacao.imovel, "(?i)esta[cç][aã]o\\s?vila") ~ "SN2",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(empresa, contrato.ampla, contrato.cef, repassado) %>%
    rename(contrato = contrato.ampla) %>%
    dplyr::filter(!is.na(empresa))
  car_t <- linhas %>%
    keep(~ str_starts(.x, "\\d{2}/\\d{2}/\\d{4}")) %>%
    as_tibble_col("linha") %>%
    mutate(
      data.vencimento = str_extract(linha, "^\\d{2}/\\d{2}/\\d{4}") %>%
        lubridate::dmy(),
      linha = str_remove(linha, "^\\d{2}/\\d{2}/\\d{4}\\s+"),
      empreendimento.codigo =
        str_extract(linha, "[A-Z0-9]{3}\\.\\d{2}\\.\\d{4}"),
      linha = str_remove(linha, "^[A-Z0-9]{3}\\.\\d{2}\\.\\d{4}\\s+"),
      contrato = str_extract(linha, "\\d{4}-\\d"),
      linha = str_remove(linha, "^\\d{4}-\\d\\s+"),
      esp.con = word(linha, 1),
      linha = str_remove(linha, str_c(esp.con, "\\s+")),
      parcela = str_extract(linha, "[^\\p{L}\\s]+"),
      linha = str_remove(linha, str_c(parcela, "\\s+")),
      esp = word(linha, 1),
      linha = str_remove(linha, str_c(esp, "\\s+")),
      ele = word(linha, 1),
      linha = str_remove(linha, str_c(ele, "\\s+")),
      emissao = str_extract(linha, "^\\d{2}/\\d{2}/\\d{4}") %>%
        lubridate::dmy(),
      linha = str_remove(linha, "^\\d{2}/\\d{2}/\\d{4}\\s+"),
      valor.atualizado =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      principal =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      juros.contrato =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      reajuste =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      encargos =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      juros =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      multa =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)"),
      seguros =
        str_extract(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)") %>%
          str_replace_all("\\.", "") %>%
          str_replace(",", ".") %>%
          as.numeric(),
      linha = str_remove(linha, "(?<!\\d)-?\\s*(?:\\d{1,3}(?:\\.\\d{3})+|\\d+)(?:,\\d{2})?(?!\\d)\\s+?"),
      agente = word(linha, 1),
      linha = str_remove(linha, str_c(agente, "\\s+")),
      cart = word(linha, 1),
      linha = str_remove(linha, str_c(cart, "\\s+")),
      disp = word(linha, 1),
      linha = str_remove(linha, str_c(disp, "\\s+"))
    ) %>%
    select(-linha) %>%
    mutate(
      empresa = str_sub(empreendimento.codigo, 1, 3),
    ) %>%
    left_join(contr_t, by = c("empresa", "contrato")) %>%
    select(
      empresa, contrato, contrato.cef, repassado, everything()
    ) %>%
    mutate(
      natureza = case_when(
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Não" ~ "Parcela CEF a repassar",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Sim" ~ "Parcela CEF",
        TRUE ~ "Pro soluto"
      )
    )
  carm_t <- car_t %>%
    group_by(empresa, natureza,
      data.mes = floor_date(data.vencimento, "month")
    ) %>%
    summarise(
      across(valor.atualizado, sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = data.mes,
      values_from = where(is.numeric),
      values_fill = 0
    ) %>%
    arrange(empresa, natureza) %>%
    select(empresa, natureza, everything()) %>%
    select(empresa, natureza, sort(names(select(., -empresa, -natureza))))
  return(list(car = car_t, carm = carm_t, contr = contr_t))
}
# View(car_t)

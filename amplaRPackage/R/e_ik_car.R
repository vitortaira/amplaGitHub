e_ik_car <- function() {
  pdf <- ler_pdf(
    file.path(caminhos_pastas("informakon"), "car-20250901_20991231.pdf")
  )
  linhas <- pdf$linhas
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
      natureza = case_when(
        ele %in% c("FGT", "FIB", "FIN") ~ "Repasse CEF",
        !ele %in% c("FGT", "FIB", "FIN") ~ "Pro soluto",
        TRUE ~ "Outro"
      )
    )
  carm_t <- car_t %>%
    group_by(empresa, natureza,
      ano = year(data.vencimento),
      mes = month(data.vencimento)
    ) %>%
    summarise(
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(empresa, ano, mes, natureza)
  return(list(car = car_t, carm = carm_t))
}
# View(car_t)

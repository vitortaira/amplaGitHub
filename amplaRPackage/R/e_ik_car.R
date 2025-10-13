e_ik_car <- function() {
  caminho.arquivo <- file.path(
    "C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA",
    "Relatórios - Documentos", "Dados", "Para o Soares", "Inputs",
    "2025_09_30", "car-2025_10_01-2099_12_31.pdf"
  )
  pdf <- ler_pdf(caminho.arquivo)
  linhas <- pdf$linhas
  contr_t <-
    e_ik_contrs() %>%
    dplyr::select(empresa, contrato.ampla, contrato.cef, repassado) %>%
    rename(contrato = contrato.ampla) %>%
    dplyr::filter(!is.na(empresa))
  car_t <- linhas %>%
    ###
    keep(~ str_starts(.x, "\\d{2}/\\d{2}/\\d{4}") |
      str_detect(.x, "(?i)im[oó]v\\s?el")) %>%
    as_tibble_col("linha") %>%
    mutate(
      parcela = str_starts(linha, "\\d{2}/\\d{2}/\\d{4}"),
      unidade = if_else(!parcela, linha[row_number()], NA_character_)
    ) %>%
    fill(unidade, .direction = "down") %>%
    dplyr::filter(parcela) %>%
    dplyr::select(-parcela) %>%
    mutate(
      data.vencimento = str_extract(linha, "^\\d{2}/\\d{2}/\\d{4}") %>%
        lubridate::dmy(),
      linha = str_remove(linha, "^\\d{2}/\\d{2}/\\d{4}\\s+"),
      empreendimento =
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
      linha = str_remove(linha, str_c(disp, "\\s+")),
      cliente = str_extract(unidade, "(?i)^.*?(?=\\s*-\\s?im[oó])"),
      unidade = str_remove(unidade, "^[^,]*,\\s?"),
      pavimento = str_extract(unidade, "^[^,]*,") %>% str_remove(","),
      unidade = str_remove(unidade, "^[^,]*,\\s?") %>% str_remove("\\s-\\s.*$"),
      especie = str_remove_all(unidade, "\\d|\\s"),
      unidade = str_remove_all(unidade, "\\D") %>% as.integer()
    ) %>%
    select(-linha) %>%
    mutate(
      empresa = str_sub(empreendimento, 1, 3),
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
    ) %>%
    mutate(
      arquivo =
      )
  carm_t <- car_t %>%
    group_by(empresa, especie, pavimento, unidade,
      data.mes = floor_date(data.vencimento, "month")
    ) %>%
    summarise(
      across(valor.atualizado, sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = data.mes,
      values_from = valor.atualizado,
      values_fill = 0
    )
  # arrange(empresa, natureza) %>%
  # select(empresa, natureza, everything()) %>%
  # select(empresa, natureza, sort(names(select(., -empresa, -natureza))))
  caru_t <- car_t %>%
    group_by(empresa, especie, pavimento, unidade) %>%
    summarise(
      total = sum(valor.atualizado, na.rm = TRUE),
      .groups = "drop"
    )
  return(list(car = car_t, carm = carm_t, contr = contr_t, caru = caru_t))
}
# View(car_t)

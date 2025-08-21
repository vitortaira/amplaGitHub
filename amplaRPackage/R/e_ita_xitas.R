e_ita_xitas <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    extratos_l <- list()
    extratos_t <- data.frame()

    for (i_caminho.extrato.ita_c in e_metadados("xita")$caminho) {
      extrato <- tryCatch(
        {
          e_ita_xita(i_caminho.extrato.ita_c)$xita_l
        },
        error = function(e) {
          message(sprintf("Falha ao extrair: %s - %s", basename(i_caminho.extrato.ita_c), e$message))
          return(NULL)
        }
      )

      if (!is.null(extrato) && nrow(extrato) > 0) {
        message(sprintf("Arquivo extraído com sucesso: %s", basename(i_caminho.extrato.ita_c)))
        extratos_l[[i_caminho.extrato.ita_c]] <- extrato
        extratos_t <- bind_rows(extratos_t, extrato)
      } else {
        message(sprintf("Arquivo vazio ou não extraído: %s", basename(i_caminho.extrato.ita_c)))
      }
    }

    # Verificar se há dados antes de processar
    if (nrow(extratos_t) == 0) {
      message("Nenhum extrato ITAÚ foi extraído com sucesso.")
      return(list(xita_l = tibble()))
    }

    # Verificar se a coluna empresa existe, se não, criar com NA
    if (!"empresa" %in% names(extratos_t)) {
      extratos_t$empresa <- NA_character_
    }

    xita.l_t <- extratos_t %>%
      mutate(
        empresa = case_when(
          !is.na(empresa) & str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
          !is.na(empresa) & str_detect(empresa, "(?i)metro\\s?v\\s?s\\s?e\\s?i") ~ "AVS",
          !is.na(empresa) & str_detect(empresa, "(?i)campo\\s?belo") ~ "CBL",
          !is.na(empresa) & str_detect(empresa, "(?i)nova\\s?civil") ~ "ENC",
          !is.na(empresa) & str_detect(empresa, "(?i)grauca") ~ "GRA",
          !is.na(empresa) & str_detect(empresa, "(?i)incorflora") ~ "INC",
          !is.na(empresa) & str_detect(empresa, "(?i)jd\\s?sao\\s?paulo") ~ "JSP",
          !is.na(empresa) & str_detect(empresa, "(?i)pompeia") ~ "POM",
          !is.na(empresa) & str_detect(empresa, "(?i)saude") ~ "SAU",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
          !is.na(empresa) & str_detect(empresa, "(?i)sale") ~ "USL",
          !is.na(empresa) & str_detect(empresa, "(?i)sao\\s?lucas") ~ "LUC",
          !is.na(empresa) & str_detect(empresa, "(?i)socorro") ~ "SOC",
          TRUE ~ empresa
        ),
        arquivo.tabela.tipo = "xita_l",
        arquivo.tipo = "xita",
        arquivo.fonte = "ita"
      ) %>%
      as_tibble() %>%
      dplyr::filter(
        !str_starts(descricao, "(?i)saldo") &
          !str_detect(descricao, "(?i)saldo\\s?a\\s?liberar")
      )
    list(
      xita_l = xita.l_t
    )
  }

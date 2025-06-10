e_ita_extitas <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    extita.l_t <- e_metadados("extita")$caminho %>%
      map_dfr(~ e_ita_extita(.x)$extita_l) %>%
      mutate(
        empresa = case_when(
          str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
          str_detect(empresa, "(?i)metro\\s?v\\s?s\\s?e\\s?i") ~ "AVS",
          str_detect(empresa, "(?i)campo\\s?belo") ~ "CBL",
          str_detect(empresa, "(?i)nova\\s?civil") ~ "ENC",
          str_detect(empresa, "(?i)grauca") ~ "GRA",
          str_detect(empresa, "(?i)incorflora") ~ "INC",
          str_detect(empresa, "(?i)jd\\s?sao\\s?paulo") ~ "JSP",
          str_detect(empresa, "(?i)pompeia") ~ "POM",
          str_detect(empresa, "(?i)saude") ~ "SAU",
          str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
          str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
          str_detect(empresa, "(?i)sale") ~ "USL",
          str_detect(empresa, "(?i)sao\\s?lucas") ~ "LUC",
          str_detect(empresa, "(?i)socorro") ~ "SOC",
          TRUE ~ NA_character_
        ),
        arquivo.tabela.tipo = "extita_l",
        arquivo.tipo = "extita",
        arquivo.fonte = "ita"
      )
    list(
      extita_l = extita.l_t
    )
  }

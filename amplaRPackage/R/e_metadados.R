e_metadados <- function(f_arquivo.tipo_c = NULL) {
  arquivo.tipos.permitidos_c <- c("viab", "xcef", "xita")
  if (
    !is.null(f_arquivo.tipo_c) &&
      !f_arquivo.tipo_c %in% arquivo.tipos.permitidos_c
  ) {
    stop(
      str_c(
        "O argumento 'f_arquivo.tipo_c' precisa ser um dos seguintes valores: ",
        str_c(arquivo.tipos.permitidos_c, collapse = ", "),
        " ou NULL"
      )
    )
  }
  # ABC ------------------------------------------------------------------------
  # extabc
  # Anapro ---------------------------------------------------------------------
  # CEF ------------------------------------------------------------------------
  # ecn
  if (
    !is.null(f_arquivo.tipo_c) &&
      f_arquivo.tipo_c == "ecn"
  ) {
    ecn_t <- dir_ls(caminhos_pastas("ciweb"), recurse = TRUE) %>%
      keep(~ str_ends(.x, "(?i)empreendimento_construcao.pdf"))
  }
  # xcef
  if (
    !is.null(f_arquivo.tipo_c) &&
      f_arquivo.tipo_c == "xcef"
  ) {
    xcef_t <-
      dir_ls(caminhos_pastas("financeiro"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "(?i)cont[aá]bil") &
          !str_detect(.x, "(?i)aplica[cç][aã]o|cdb|fundo|parcela|pix|simples") &
          !str_detect(.x, "(?i)6\\s?meses|comprovante|inativas|nota|nf\\s") &
          !str_detect(.x, "(?i)facil|investimento|recebiveis|sihex|topazio") &
          # AMP
          (str_detect(.x, "600|2362|2429") |
            # AVS
            str_detect(.x, "2245|2399") |
            # GRA
            str_detect(.x, "2480") |
            # INC
            str_detect(.x, "2412|3455|129123") |
            # LUC
            str_detect(.x, "80827") |
            # POM
            str_detect(.x, "2278") |
            # SAU
            str_detect(.x, "80924") |
            # SN2
            str_detect(.x, "2419") |
            # SN4
            str_detect(.x, "81031"))
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "xcef",
        arquivo.tipo = "xcef",
        arquivo.fonte = "cef",
        empresa = case_when(
          str_detect(caminho, "600|2362|2429") ~ "AMP",
          str_detect(caminho, "2245|2399") ~ "AVS",
          str_detect(caminho, "2480") ~ "GRA",
          str_detect(caminho, "2412|3455|129123") ~ "INC",
          str_detect(caminho, "80827") ~ "LUC",
          str_detect(caminho, "2278") ~ "POM",
          str_detect(caminho, "80924") ~ "SAU",
          str_detect(caminho, "2419") ~ "SN2",
          str_detect(caminho, "81031") ~ "SN4",
          TRUE ~ NA
        ),
        data = NA
      )
    return(xcef_t)
  }
  # Informakon -----------------------------------------------------------------
  # inad
  if (
    !is.null(f_arquivo.tipo_c) &&
      f_arquivo.tipo_c == "inad"
  ) {
    # Tabela com todos os caminhos dos arquivos do tipo inad
    inad_t <-
      dir_ls(caminhos_pastas("cobranca"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)/inadimpl.ncia\\s?-.*\\.xlsx") &
          !str_detect(.x, "(?i)consolidado")
      ) %>%
      map_dfr(~ {
        # Extrai a data do nome do arquivo no formato %Y_%m (ex: 2025_04)
        data_str <- str_extract(.x, "-\\s?\\d{4}_\\d{2}") %>%
          str_extract("\\d{4}_\\d{2}")
        data <- suppressWarnings(
          as.Date(paste0(data_str, "_01"), format = "%Y_%m_%d")
        )
        # Extrai o empreendimento do nome do arquivo (ex: AVS)
        empreendimentos_c <- str_extract(.x, "-\\s?\\w{3}\\s?-") %>%
          str_extract("\\w{3}")
        tibble(
          caminho = .x,
          arquivo.tabela.tipo = "inad",
          arquivo.tipo = "inad",
          arquivo.fonte = "ik",
          empresa = empreendimentos_c,
          data = data
        )
      })
    return(inad_t)
  }
  # Itaú -----------------------------------------------------------------------
  # xita
  if (
    !is.null(f_arquivo.tipo_c) &&
      f_arquivo.tipo_c == "xita"
  ) {
    xita_t <-
      dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "(?i)extrato") &
          str_detect(.x, "0186|2633|5441|9756") &
          !str_detect(.x, "(?i)pix")
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "xita",
        arquivo.tipo = "xita",
        arquivo.fonte = "ita"
      )
    return(xita_t)
  }
  # Viabilidade ----------------------------------------------------------------
  # viab
  if (
    !is.null(f_arquivo.tipo_c) &&
      f_arquivo.tipo_c == "viab"
  ) {
    viab_t <- c(
      # AMP (Jardim Prudência)
      fs::path(
        "C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA",
        "Controladoria - Documentos", "Financeiro & Controladoria",
        "Empreendimentos", "2]  UP Jardim Prudência",
        "Viabilidade UP Jardim Prudência.xlsx"
      ),
      # AVS
      fs::path(
        "C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA",
        "Controladoria - Documentos", "Financeiro & Controladoria",
        "Empreendimentos", "1]  UP Vila Sonia",
        "Viabilidade UP Vila Sonia.xlsx"
      ),
      # GRA
      fs::path(
        "C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA",
        "Controladoria - Documentos", "Financeiro & Controladoria",
        "Empreendimentos", "8]  UP Select",
        "Viabilidade UP Select.xlsx"
      ),
      # SN2
      fs::path(
        "C:", "Users", "Ampla", "AMPLA INCORPORADORA LTDA",
        "Controladoria - Documentos", "Financeiro & Controladoria",
        "Empreendimentos", "4]  UP Estação Vila Sonia",
        "Viabilidade UP Estação Vila Sonia.xlsx"
      )
    ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "viab",
        arquivo.tipo = "viab",
        arquivo.fonte = "viab",
        empresa = case_when(
          str_detect(caminho, "(?i)jardim\\s?prud[eê]ncia") ~ "AMP",
          str_detect(caminho, "(?i)vila\\s?s[oô]nia") ~ "AVS",
          str_detect(caminho, "(?i)select") ~ "GRA",
          str_detect(caminho, "(?i)esta[cç][aã]o\\s?vila\\s?s[oô]nia") ~ "SN2",
          TRUE ~ NA
        ),
        data = NA
      )
    return(viab_t)
  }
  # Consolidado ----------------------------------------------------------------
  else {
    metadados_t <- bind_rows(
      viab_t,
      xcef_t,
      xita_t
    )
    return(metadados_t)
  }
}

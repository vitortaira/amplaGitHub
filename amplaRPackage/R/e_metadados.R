#' @title Coleta de Metadados de Diversos Tipos de Arquivos
#' @description Função principal que agrega metadados de diferentes fontes.
#'   Pode retornar metadados para um tipo de arquivo específico ou para todos os tipos.
#' @param f_arquivo.tipo_c O tipo de arquivo para o qual os metadados devem ser coletados.
#'   Valores permitidos: "inad", "viab", "xcef", "xita". Se NULL (padrão),
#'   retorna metadados para todos os tipos.
#' @return Um tibble consolidado com os metadados dos arquivos solicitados.
#' @export
e_metadados <- function(f_arquivo.tipo_c = NULL) {
  c_contr <- function() {
    dir_ls(caminhos_pastas("cobranca"), recurse = TRUE, type = "file") %>%
      keep(~ str_detect(.x, "(?i)contratos-.*\\.xlsx")) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "contr",
        arquivo.tipo = "contr",
        arquivo.fonte = "ik",
        empresa = str_extract(.data$caminho, "-\\s?\\w{3}\\s?-") %>%
          str_extract("\\w{3}"),
        data = as.Date(
          paste0(
            str_extract(.data$caminho, "-\\s?\\d{4}_\\d{2}") %>%
              str_extract("\\d{4}_\\d{2}"),
            "_01"
          ),
          format = "%Y_%m_%d"
        )
      )
  }

  c_inad <- function() {
    dir_ls(caminhos_pastas("cobranca"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)/inadimpl.ncia\\s?-.*\\.xlsx") &
          !str_detect(.x, "(?i)consolidado")
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "inad",
        arquivo.tipo = "inad",
        arquivo.fonte = "ik",
        empresa = str_extract(.data$caminho, "-\\s?\\w{3}\\s?-") %>%
          str_extract("\\w{3}"),
        data = as.Date(
          paste0(
            str_extract(.data$caminho, "-\\s?\\d{4}_\\d{2}") %>%
              str_extract("\\d{4}_\\d{2}"),
            "_01"
          ),
          format = "%Y_%m_%d"
        )
      )
  }

  c_xabc <- function() {
    dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)abc") & str_ends(.x, "\\.xlsx")
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "xabc",
        arquivo.tipo = "xabc",
        arquivo.fonte = "abc",
        empresa = str_extract(.data$caminho, "(?<=/)[A-Z0-9]{3}(?=/)") %>%
          str_extract("[A-Z]{3}"),
        data = .data$caminho %>%
          str_extract("\\d{4}_\\d{2}") %>%
          str_c("_01") %>%
          as.Date(format = "%Y_%m_%d")
      )
  }

  c_xcef <- function() {
    dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)cef") & str_ends(.x, "\\.pdf|\\.xlsx")
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "xcef",
        arquivo.tipo = "xcef",
        arquivo.fonte = "cef",
        empresa = str_extract(.data$caminho, "(?<=/)[A-Z0-9]{3}(?=/)") %>%
          str_extract("[A-Z0-9]{3}"),
        data = .data$caminho %>%
          str_extract("\\d{4}_\\d{2}") %>%
          str_c("_01") %>%
          as.Date(format = "%Y_%m_%d")
      )
  }

  c_xita <- function() {
    dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)ita[uú]") & str_ends(.x, "\\.pdf|\\.xlsx")
      ) %>%
      as_tibble_col("caminho") %>%
      mutate(
        arquivo.tabela.tipo = "xita",
        arquivo.tipo = "xita",
        arquivo.fonte = "ita",
        empresa = .data$caminho %>%
          stringr::str_extract("(?<=/)[A-Z0-9]{3}(?=/)"),
        data = .data$caminho %>%
          str_extract("\\d{4}_\\d{2}") %>%
          str_c("_01") %>%
          as.Date(format = "%Y_%m_%d")
      )
  }

  c_viab <- function() {
    c(
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
          str_detect(.data$caminho, "(?i)jardim\\s?prud[eê]ncia") ~ "AMP",
          str_detect(.data$caminho, "(?i)vila\\s?s[oô]nia") ~ "AVS",
          str_detect(.data$caminho, "(?i)select") ~ "GRA",
          str_detect(
            .data$caminho, "(?i)esta[cç][aã]o\\s?vila\\s?s[oô]nia"
          ) ~ "SN2",
          TRUE ~ NA_character_
        ),
        data = as.Date(NA)
      )
  }

  arquivo.tipos.permitidos_c <- c(
    "contr", "inad", "viab", "xabc", "xcef", "xita"
  )

  if (!is.null(f_arquivo.tipo_c)) {
    if (!f_arquivo.tipo_c %in% arquivo.tipos.permitidos_c) {
      stop(
        str_c(
          "O argumento 'f_arquivo.tipo_c' precisa ser um dos seguintes valores: ",
          str_c(arquivo.tipos.permitidos_c, collapse = ", "),
          " ou NULL"
        )
      )
    }
    # Busca apenas o tipo especificado
    return(
      switch(f_arquivo.tipo_c,
        "contr" = c_contr(),
        "inad" = c_inad(),
        "viab" = c_viab(),
        "xabc" = c_xabc(),
        "xcef" = c_xcef(),
        "xita" = c_xita()
      )
    )
  }

  # Se nenhum tipo for especificado, busca e combina todos
  bind_rows(
    c_contr(),
    c_inad(),
    c_viab(),
    c_xabc(),
    c_xcef(),
    c_xita()
  )
}

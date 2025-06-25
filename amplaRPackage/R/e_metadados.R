#' @title Coleta de Metadados de Arquivos de Inadimplência
#' @description Retorna um tibble com os metadados dos arquivos de inadimplência.
#' @return Um tibble com as colunas: caminho, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte, empresa, data.
#' @export
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

#' @title Coleta de Metadados de Extratos Bancários da CEF
#' @description Retorna um tibble com os metadados dos extratos bancários (PDF) da Caixa Econômica Federal.
#' @return Um tibble com as colunas: caminho, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte, empresa, data.
#' @export
c_xcef <- function() {
  dir_ls(caminhos_pastas("financeiro"), recurse = TRUE, type = "file") %>%
    keep(
      ~ str_ends(.x, ".pdf") &
        str_detect(.x, "(?i)cont[aá]bil") &
        !str_detect(.x, "(?i)aplica[cç][aã]o|cdb|fundo|parcela|pix|simples") &
        !str_detect(.x, "(?i)6\\s?meses|comprovante|inativas|nota|nf\\s") &
        !str_detect(.x, "(?i)facil|investimento|recebiveis|sihex|topazio") &
        (str_detect(.x, "600|2362|2429") | # AMP
          str_detect(.x, "2245|2399") | # AVS
          str_detect(.x, "2480") | # GRA
          str_detect(.x, "2412|3455|129123") | # INC
          str_detect(.x, "80827") | # LUC
          str_detect(.x, "2278") | # POM
          str_detect(.x, "80924") | # SAU
          str_detect(.x, "2419") | # SN2
          str_detect(.x, "81031")) # SN4
    ) %>%
    as_tibble_col("caminho") %>%
    mutate(
      arquivo.tabela.tipo = "xcef",
      arquivo.tipo = "xcef",
      arquivo.fonte = "cef",
      empresa = NA,
      data = NA
    )
}

#' @title Coleta de Metadados de Extratos Bancários do Itaú
#' @description Retorna um tibble com os metadados dos extratos bancários (PDF) do Itaú.
#' @return Um tibble com as colunas: caminho, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte, empresa, data.
#' @export
c_xita <- function() {
  dir_ls(caminhos_pastas("extratos"), recurse = TRUE, type = "file") %>%
    keep(
      ~ str_ends(.x, ".pdf") &
        str_detect(.x, "(?i)extrato") &
        !str_detect(.x, "(?i)pix") &
        str_detect(.x, "0186|2633|5441|9756")
    ) %>%
    as_tibble_col("caminho") %>%
    mutate(
      arquivo.tabela.tipo = "xita",
      arquivo.tipo = "xita",
      arquivo.fonte = "ita",
      empresa = NA,
      data = NA
    )
}

#' @title Coleta de Metadados de Arquivos de Viabilidade
#' @description Retorna um tibble com os metadados dos arquivos de viabilidade econômica.
#' @return Um tibble com as colunas: caminho, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte, empresa, data.
#' @export
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
        str_detect(.data$caminho, "(?i)esta[cç][aã]o\\s?vila\\s?s[oô]nia") ~ "SN2",
        TRUE ~ NA
      ),
      data = NA
    )
}


#' @title Coleta de Metadados de Diversos Tipos de Arquivos
#' @description Função principal que agrega metadados de diferentes fontes.
#'   Pode retornar metadados para um tipo de arquivo específico ou para todos os tipos.
#' @param f_arquivo.tipo_c O tipo de arquivo para o qual os metadados devem ser coletados.
#'   Valores permitidos: "inad", "viab", "xcef", "xita". Se NULL (padrão),
#'   retorna metadados para todos os tipos.
#' @return Um tibble consolidado com os metadados dos arquivos solicitados.
#' @export
e_metadados <- function(f_arquivo.tipo_c = NULL) {
  arquivo.tipos.permitidos_c <- c("inad", "viab", "xcef", "xita")

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
        "inad" = c_inad(),
        "viab" = c_viab(),
        "xcef" = c_xcef(),
        "xita" = c_xita()
      )
    )
  }

  # Se nenhum tipo for especificado, busca e combina todos
  bind_rows(
    c_inad(),
    c_viab(),
    c_xcef(),
    c_xita()
  )
}

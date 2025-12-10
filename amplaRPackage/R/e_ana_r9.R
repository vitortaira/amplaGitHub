#' @title Extração do Arquivo R9 Mais Recente
#'
#' @description
#' A função e_ana_r9() identifica e extrai o arquivo R9 mais recente
#' na pasta especificada.
#'
#' @param caminho.pasta.inputs_c String do caminho da pasta "Inputs".
#'   Valor padrão: \code{caminhos_pastas("soares_in")}.
#'
#' @return Data frame com dados do arquivo R9 mais recente.
#'
#' @examples
#' \dontrun{
#' r9_df <- e_ana_r9()
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_extract
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate
#' @export
e_ana_r9 <- function(
    caminho.pasta.inputs_c = caminhos_pastas("soares_in")) {
  if (!dir.exists(caminho.pasta.inputs_c)) {
    stop("A pasta 'Inputs' não foi encontrada: ", caminho.pasta.inputs_c)
  }

  # Busca todos os arquivos r9 recursivamente
  caminhos.r9_c <- fs::dir_ls(
    caminho.pasta.inputs_c,
    recurse = TRUE,
    type = "file",
    regexp = "r9-\\d{4}_\\d{2}_\\d{2}\\.xlsx$"
  )

  if (length(caminhos.r9_c) == 0) {
    stop("Nenhum arquivo r9 encontrado na pasta Inputs.")
  }

  # Extrai as datas dos nomes dos arquivos (formato: r9-YYYY_MM_DD.xlsx)
  datas.por.arquivo_d <- sapply(caminhos.r9_c, function(caminho_c) {
    basename(caminho_c) %>%
      stringr::str_extract("\\d{4}_\\d{2}_\\d{2}") %>%
      as.Date(format = "%Y_%m_%d")
  })

  # Encontra o índice do arquivo mais recente
  indice.recente_i <- which.max(datas.por.arquivo_d)

  # Seleciona o caminho do arquivo mais recente
  caminho.arquivo.r9_c <- caminhos.r9_c[indice.recente_i]

  # Mensagem informativa
  message("Extraindo arquivo: ", basename(caminho.arquivo.r9_c))

  # Lê o arquivo Excel (col_types = "text" evita warnings de tipos mistos)
  r9_t <- readxl::read_excel(caminho.arquivo.r9_c, col_types = "text") %>%
    dplyr::slice(-n()) %>%
    rename(valor.venda = `Preço de venda`) %>%
    mutate(
      `Preço Tab.` = as.numeric(`Preço Tab.`),
      Unidade = str_remove_all(Unidade, "\\s+"),
      empresa = case_when(
        str_detect(Empreendimento, "(?i)jardim\\s?prud") ~ "AMP",
        str_detect(Empreendimento, "(?i)up\\s?vila\\s?s[oô]nia") ~ "AVS",
        str_detect(Empreendimento, "(?i)select") ~ "GRA",
        str_detect(Empreendimento, "(?i)s[aã]o\\s?lucas") ~ "LUC",
        str_detect(Empreendimento, "(?i)pomp[eé]ia") ~ "POM",
        str_detect(Empreendimento, "(?i)sa[uú]de") ~ "SAU",
        str_detect(Empreendimento, "(?i)esta[cç][aã]o.*s[oô]nia") ~ "SN2",
        str_detect(Empreendimento, "(?i)move\\s?vila\\s?s[oô]nia") ~ "SN4",
        TRUE ~ NA_character_
      ),
      especie = case_when(
        str_sub(Unidade, 1, 1) == "U" ~ "Apartamento",
        str_sub(Unidade, 1, 1) == "L" ~ "Loja",
        str_sub(Unidade, 1, 2) == "VG" ~ "Garagem",
        TRUE ~ NA_character_
      ),
      unidade = str_extract(Unidade, "\\d+") %>% as.integer(),
      valor.venda = as.numeric(valor.venda),
      id = str_c(empresa, especie, unidade, sep = "-"),
      arquivo = caminho.arquivo.r9_c,
      arquivo.tipo = "r9",
      arquivo.fonte = "ana"
    ) %>%
    select(
      id, empresa, especie, unidade, everything()
    )

  return(r9_t)
}

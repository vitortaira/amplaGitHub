#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\a_dados_rds_xlsx.R
#' Exporta os dados para um arquivo XLSX
#'
#' A função \code{a_dados_rds_xlsx()} transforma os dados obtidos pela função
#' \code{e_dados()} em um arquivo Excel (.xlsx). Cada aba do arquivo é criada
#' dinamicamente com os dados de diferentes fontes e configurada com cores e
#' nomes predefinidos.
#'
#' @param f_caminho.pasta.dados_c Caminho para a pasta onde os dados originais estão
#'   armazenados. Valor padrão: \code{caminhos_pastas("dados")}.
#'
#' @return Gera e salva um arquivo XLSX no diretório especificado, com o nome
#'   incluindo a data e a hora da criação.
#'
#' @details A função realiza as seguintes operações:
#'   \itemize{
#'     \item Valida se o diretório com os dados existe.
#'     \item Carrega os dados utilizando \code{e_dados()}.
#'     \item Cria um workbook com abas configuradas conforme a fonte dos dados (por exemplo, "cef" ou "ik")
#'           e atribui cores específicas a cada aba.
#'     \item Salva o arquivo XLSX com um nome composto pela data e hora da geração.
#'   }
#'
#' @examples
#' \dontrun{
#' a_dados_rds_xlsx()
#' }
#'
#' @export
a_dados_rds_xlsx <- function(
    f_caminho.pasta.dados_c = caminhos_pastas("dados"),
    f_dashboard = FALSE) {
  # Valida argumentos
  if (!dir.exists(f_caminho.pasta.dados_c)) {
    stop(
      str_c(
        "A pasta '.../Relatórios - Documentos/Dados/Dados originais' ",
        "não foi encontrada."
      )
    )
  }
  # Carregando os dados
  dados_l <- e_dados()
  # Criando o arquivo xlsx
  wb_x <- createWorkbook()
  # Define nomes e cores das abas
  wb.abas_l <- list(
    Aba = c(
      # Metadados
      "metadados",
      # Abas da CEF
      c("cmfcn", "dcd", "ecn_c", "ecn_e", "ecn_pj", "ecn_u", "epr", "extcef"),
      # Abas do Informakon
      c("contr", "desp", "inad", "rec"),
      # Abas do Itaú
      c("extita_l")
    ),
    Fonte = c(
      # Metadados
      "metadados",
      # Abas da CEF
      c(rep("cef", 8)),
      # Abas do Informakon
      c(rep("ik", 4)),
      # Abas do Itaú
      c(rep("ita", 1))
    ),
    Cor = c(
      # Metadados
      "darkred",
      # Abas da CEF
      c(rep("darkblue", 8)),
      # Abas do Informakon
      c(rep("darkgreen", 4)),
      # Abas do Itaú
      c(rep("darkorange", 1))
    )
  )
  # Adicionando e populando abas dinamicamente
  pwalk(
    wb.abas_l,
    function(Aba, Fonte, Cor) {
      addWorksheet(
        wb_x,
        sheet = Aba,
        gridLines = FALSE,
        tabColour = Cor
      )
      writeDataTable(
        wb_x,
        sheet = Aba,
        as.data.frame(dados_l[[Fonte]][[Aba]]),
        tableName = Aba
      )
    }
  )
  nome.xlsx_c <- str_c(
    caminhos_pastas("dados"),
    "/Dados_",
    format(Sys.time(), format = "%Y_%m_%d-%H_%M_%S"),
    ".xlsx"
  )
  saveWorkbook(wb_x, nome.xlsx_c, overwrite = FALSE)
  # Removendo arquivos da pasta "dados" em amplaShiny
  file_delete(dir_ls(path(here("amplaShiny", "inst", "dados")), recurse = TRUE))
  # Salvando dados_l como RDS em amplaShiny
  saveRDS(
    dados_l,
    file = str_c(
      caminhos_pastas("shiny"),
      "/inst/dados/",
      str_replace(basename(nome.xlsx_c), "\\.xlsx$", ".rds")
    )
  )
  if (f_dashboard) {
    rsconnect::deployApp(caminhos_pastas("shiny"))
  }
}

#' @title Extração e Consolidação de Relatórios EPR da CEF
#'
#' @description
#' A função **e_cef_eprs()** extrai e consolida os dados dos relatórios EPR da CEF
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'   Padrão: \code{file.path(dirname(dirname(here())), "Relatórios - Documentos", "Relatorios - CIWEB")}.
#'
#' @details
#' O processo busca todos os arquivos EPR com extensão \code{.pdf}, realiza a extração
#' dos dados e consolida em um único \code{tibble}.
#'
#' @return Retorna um \code{tibble} contendo as informações extraídas dos relatórios EPR.
#'
#' @examples
#' \dontrun{
#' # Usando o caminho padrão
#' e_cef_eprs()
#'
#' # Fornecendo um caminho específico:
#' e_cef_eprs("C:/caminho/personalizado/Relatorios - CIWEB")
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect str_ends
#' @importFrom purrr keep
#' @importFrom dplyr bind_rows distinct
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @export
e_cef_eprs <-
  function(f_caminho.pasta.ciweb_c =
             c_caminhos_pastas("ciweb")) {
    # Consolida os dados dos relatórios EPR da CEF na pasta "Relatorios - CIWEB"
    caminhos.epr_c <-
      dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE, type = "file") %>%
      keep(~ str_ends(.x, "CONTRATOS_EMPREEND.pdf"))
    # Identifica o arquivo mais recente de cada empreendimento
    contratos.empreendimentos.12.primeiros_c <-
      caminhos.epr_c %>%
      str_extract("\\d{12}") %>%
      unique()
    caminhos.epr.recentes_c <- map(
      contratos.empreendimentos.12.primeiros_c,
      ~ {
        i <- caminhos.epr_c %>%
          str_subset(.x) %>%
          path_file() %>%
          str_extract("^\\d{8}") %>%
          ymd() %>%
          which.max()
        caminhos.epr_c[i]
      }
    ) %>%
      flatten_chr() %>%
      unname()
    eprs_l <- list()
    eprs_t <- data.frame()
    for (
      i_caminho.cef.epr_c in caminhos.epr.recentes_c
    ) {
      eprs_l[[i_caminho.cef.epr_c]] <-
        e_cef_epr(i_caminho.cef.epr_c)
      eprs_t <-
        bind_rows(eprs_t, eprs_l[[i_caminho.cef.epr_c]])
    }
    eprs_t %<>% distinct(across(-arquivo), .keep_all = TRUE) %>%
      as_tibble() %>%
      mutate(
        arquivo.tabela.tipo = "epr",
        arquivo_tipo = "epr",
        arquivo_fonte = "cef"
      )
    return(eprs_t)
  }

# Teste -------------------------------------------------------------------

# f_caminho.pasta.ciweb_c <-
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "EPR",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
# eprs_t <- e_cef_eprs()
# shell.exec(f_caminho.pasta.ciweb_c)

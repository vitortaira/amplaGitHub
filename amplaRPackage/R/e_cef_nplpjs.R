#' @title Consolidação dos dados dos relatórios NPL/PJ da CEF
#'
#' @description
#' A função **e_cef_nplpjs** extrai e consolida os dados dos relatórios NPL/PJ da CEF
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'   Por padrão, utiliza o caminho relativo baseado na estrutura do projeto.
#' @param empresa Nome da empresa para filtrar os resultados. Se NULL, retorna
#'   todas as empresas.
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos PDF que
#' contenham "npl_pj" no nome. Para cada arquivo encontrado,
#' chama a função \code{e_cef_nplpj} para realizar a extração dos dados e,
#' posteriormente, consolida os resultados em uma única tabela. Se o parâmetro
#' empresa for fornecido, filtra os resultados apenas para a empresa especificada.
#'
#' @return
#' Retorna uma tibble com os dados consolidados dos relatórios NPL/PJ, filtrados
#' por empresa se o parâmetro empresa for fornecido.
#'
#' @examples
#' \dontrun{
#' # Retorna dados de todas as empresas
#' nplpjs <- e_cef_nplpjs(
#'   f_caminho.pasta.ciweb_c = "caminho/para/a/pasta/Relatorios - CIWEB"
#' )
#'
#' # Retorna dados apenas da empresa específica
#' nplpjs_empresa <- e_cef_nplpjs(
#'   f_caminho.pasta.ciweb_c = "caminho/para/a/pasta/Relatorios - CIWEB",
#'   empresa = "AMPLA VILA SONIA"
#' )
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom purrr keep map_dfr
#' @importFrom stringr str_ends
#' @importFrom dplyr distinct filter
#'
#' @export

e_cef_nplpjs <-
  function(f_caminho.pasta.ciweb_c = caminhos_pastas("ciweb"),
           empresa = NULL) {
    # Consolida os dados dos relatórios NPL/PJ da CEF na pasta "Relatorios - CIWEB"
    caminhos.nplpj_c <-
      dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE) %>%
      keep(~ str_ends(.x, "(?i)npl_pj.pdf"))

    # Consolida as tabelas usando map_dfr
    nplpjs_t <-
      caminhos.nplpj_c %>%
      map_dfr(~ e_cef_nplpj(.x)) %>%
      distinct()

    # Filtra por empresa se o parâmetro for fornecido
    if (!is.null(empresa)) {
      nplpjs_t <- nplpjs_t %>%
        filter(.data$empresa == empresa)
    }

    return(nplpjs_t)
  }

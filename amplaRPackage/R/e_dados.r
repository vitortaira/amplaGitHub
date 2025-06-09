#### filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_dados.R
#' @title Consolidação de Dados: CEF e Informakon
#'
#' @description
#' A função **e_dados()** consolida dados de diferentes fontes (CEF, Informakon)
#' em um único objeto, facilitando o acesso e manipulação.
#'
#' @details
#' Internamente chama \code{\link{e_cef}} e \code{\link{e_ik}} para obter
#' informações relacionadas a relatórios, extratos e dados específicos.
#'
#' @return
#' Retorna uma lista contendo duas entradas:
#' - \code{cef}: Resultado da função \code{e_cef()}
#' - \code{ik}: Resultado da função \code{e_ik()}
#'
#' @examples
#' \dontrun{
#' # Exemplo simples de chamada
#' dados <- e_dados()
#' str(dados)
#' s
#' }
#'
#' @seealso
#' \code{\link{e_cef}}, \code{\link{e_ik}}
#'
#' @importFrom stringi stri_trans_nfc
#'
#' @export
e_dados <- function() {
  plan(multisession)
  normalize_names <- function(obj) {
    # If the object has names, normalize them
    if (!is.null(names(obj))) {
      names(obj) <- stri_trans_nfc(enc2utf8(names(obj)))
    }
    # If it's a list (but not a data.frame), process each element recursively.
    if (is.list(obj) && !is.data.frame(obj)) {
      obj <- lapply(obj, normalize_names)
    }
    # For data.frames, only fix the names; leave content unchanged.
    obj
  }
  # Create the futures
  cefFut <- future(
    {
      e_cef()
    },
    seed = TRUE
  )
  ikFut <- future(
    {
      e_ik()
    },
    seed = TRUE
  )
  itaFut <- future(
    {
      e_ita()
    },
    seed = TRUE
  )
  # Manually get them with value()
  dados_l <- list(
    cef = value(cefFut),
    ik  = value(ikFut),
    ita = value(itaFut)
  )
  # Normalize names in the list
  normalize_names(dados_l)
  arquivos_c <- dados_l %>%
    flatten() %>%
    map_dfr(~ dplyr::select(
      .x,
      arquivo, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte
    )) %>%
    distinct()
  info.arquivos_t <- file_info(arquivos_c$arquivo)
  metadados_t <-
    tibble(
      arquivo = info.arquivos_t$path,
      arquivo.tabela.tipo = arquivos_c$arquivo.tabela.tipo,
      arquivo.tipo = arquivos_c$arquivo.tipo,
      arquivo.fonte = arquivos_c$arquivo.fonte,
      nome = arquivo %>% basename() %>% str_remove("\\..*$"),
      pasta = arquivo %>% dirname() %>% str_extract("[^/]+$"),
      extensao = path_ext(arquivo) %>% str_to_lower() %>% as.factor(),
      tamanho.bytes = as.numeric(info.arquivos_t$size),
      data.modificacao = info.arquivos_t$modification_time,
      data.mudanca = info.arquivos_t$change_time,
      data.acesso = info.arquivos_t$access_time,
      data.criacao = info.arquivos_t$birth_time
    )
  dados_l$metadados$metadados <- metadados_t
  dados_l$metadados$gnw_nodes <- m_nodes()$nodes
  dados_l$metadados$gnw_nodes_legends <- m_nodes()$nodes.legends
  dados_l$metadados$gnw_edges <- m_edges()
  return(dados_l)
}

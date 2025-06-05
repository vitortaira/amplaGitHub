#' @title Consolidação dos dados dos relatórios ECN da CEF
#'
#' @description
#' A função **e_cef_ecns** extrai e consolida os dados dos relatórios ECN da CEF
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'   Por padrão, utiliza o caminho relativo baseado na estrutura do projeto.
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos PDF que
#' contenham "empreendimento_construcao" no nome. Para cada arquivo encontrado,
#' chama a função \code{e_cef_ecn} para realizar a extração dos dados e,
#' posteriormente, consolida os resultados em uma lista com quatro elementos:
#' Empreendimento, Empréstimo, Consolidados e Unidades.
#'
#' @return
#' Retorna uma lista com os seguintes elementos:
#'   - Empreendimento: Tibble com os dados dos empreendimentos.
#'   - Emprestimo: Tibble com os dados dos empréstimos.
#'   - Consolidado: Tibble com os dados consolidados.
#'   - Unidades: Tibble com os dados das unidades.
#'
#' @examples
#' \dontrun{
#' ecns <- e_cef_ecns(
#'   f_caminho.pasta.ciweb_c = "caminho/para/a/pasta/Relatorios - CIWEB"
#' )
#' print(ecns)
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom purrr keep map_dfr
#' @importFrom stringr str_ends
#' @importFrom dplyr distinct
#'
#' @export

e_cef_ecns <-
  function(f_caminho.pasta.ciweb_c = c_caminhos_pastas("ciweb")) {
    # Consolida os dados dos relatórios ECN da CEF na pasta "Relatorios - CIWEB"
    caminhos.ecn_c <-
      dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE) %>%
      keep(~ str_ends(.x, "(?i)empreendimento_construcao.pdf"))
    # Tabelas não-cumulativas
    ecns.empreendimento_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Empreendimento) %>%
      distinct() %>%
      mutate(
        arquivo_tipo = "ecn",
        tabela.tipo = "ecn_e",
        arquivo_fonte = "cef"
      )
    ecns.emprestimo_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Emprestimo) %>%
      distinct() %>%
      mutate(
        tabela.tipo = "ecn_pj",
        arquivo_tipo = "ecn",
        arquivo_fonte = "cef",
      )
    # Identifica o arquivo mais recente de cada empreendimento
    contratos.empreendimentos.12.primeiros_c <-
      caminhos.ecn_c %>%
      str_extract("\\d{12}") %>%
      unique()
    caminhos.ecn.recentes_c <- map(
      contratos.empreendimentos.12.primeiros_c,
      ~ {
        i <- caminhos.ecn_c %>%
          str_subset(.x) %>%
          path_file() %>%
          str_extract("^\\d{8}") %>%
          ymd() %>%
          which.max()
        caminhos.ecn_c[i]
      }
    ) %>%
      flatten_chr() %>%
      unname()
    # Tabelas cumulativas
    ecns.consolidado_t <-
      caminhos.ecn.recentes_c %>%
      map_dfr(~ e_cef_ecn(.x)$Consolidado) %>%
      distinct() %>%
      mutate(
        tabela.tipo = "ecn_c",
        arquivo_tipo = "ecn",
        arquivo_fonte = "cef"
      )
    ecns.unidades_t <-
      caminhos.ecn.recentes_c %>%
      map_dfr(~ e_cef_ecn(.x)$Unidades) %>%
      distinct() %>%
      mutate(
        tabela.tipo = "ecn_u",
        arquivo_tipo = "ecn",
        arquivo_fonte = "cef"
      )
    ecns_l <-
      list(
        ecn_e = ecns.empreendimento_t,
        ecn_pj = ecns.emprestimo_t,
        ecn_c = ecns.consolidado_t,
        ecn_u = ecns.unidades_t
      )
    return(ecns_l)
  }

# Teste -------------------------------------------------------------------

# f_caminho.pasta.ciweb_c <-
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "ECN",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
# ecns_t <- e_cef_ecns()
# shell.exec(f_caminho.pasta.ciweb_c)

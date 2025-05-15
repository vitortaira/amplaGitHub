#' @title Create graph edges for data flow visualization
#'
#' @description
#' Creates edge definitions for the data flow diagram used with visNetwork
#'
#' @return A list containing the edges_df dataframe
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @export
m_edges <- function() {
  # Definição das arestas principais
  data.frame(
    from =
      c(
        # Origem
        "ana", "cef", "cef", "ifk", "ifk", "ifk", "ifk", "ita",
        # Arquivos
        "com",
        "ecn", "ecn", "ecn", "ecn", "fre",
        "cntr", "desp", "inad", "rec",
        "extita",
        # Tabelas
        "com_t",
        "ecn_c", "ecn_i", "ecn_pj", "ecn_u", "fre_t",
        "cntr_t", "desp_t", "inad_t", "rec_t",
        "extita_t",
        # Base de dados
        rep("dados", 2),
        # Relatório
        "relatorio.financiamento", "dfc"
      ),
    to =
      c(
        # Origem ->
        "com",
        "ecn", "fre",
        "cntr", "desp", "inad", "rec",
        "extita",
        # Arquivo ->
        "com_t",
        "ecn_c", "ecn_i", "ecn_pj", "ecn_u", "fre_t",
        "cntr_t", "desp_t", "inad_t", "rec_t",
        "extita_t",
        # Tabelas ->
        rep("dados", 11),
        # Base de dados ->
        "relatorio.financiamento", "dfc",
        # Relatório ->
        rep("decisao", 2)
      ),
    label = c(rep("Label", 34)),
    title = c(rep("Title", 34))
  ) %>%
    mutate(
      color =
        case_when(
          from %in% c("desp", "rec", "ecn") ~ "green",
          from %in% c("fre", "cef") ~ "yellow",
          TRUE ~ "red"
        )
    )
}

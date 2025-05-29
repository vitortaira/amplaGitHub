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
        # Origens ->
        rep("ana", 1), rep("cef", 6), rep("ifk", 4), rep("ita", 1),
        # Arquivos ->
        "com",
        "cmfcn", "dcd", "ecn", "ecn", "ecn", "ecn", "epr", "extcef", "fre",
        "cntr", "desp", "inad", "rec",
        "extita",
        # Tabelas ->
        "com_t",
        "cmfcn_t", "dcd_t", "ecn_c", "ecn_i", "ecn_pj", "ecn_u", "epr_t",
        "extcef_t", "fre_t",
        "cntr_t", "desp_t", "inad_t", "rec_t",
        "extita_t",
        # Base de dados ->
        rep("dados", 4),
        # Relatórios ->
        "cext", "dfc", "rinad"
      ),
    to =
      c(
        # -> Arquivos
        "com",
        "cmfcn", "dcd", "ecn", "epr", "extcef", "fre",
        "cntr", "desp", "inad", "rec",
        "extita",
        # -> Tabelas
        "com_t",
        "cmfcn_t", "dcd_t", "ecn_c", "ecn_i", "ecn_pj", "ecn_u", "epr_t",
        "extcef_t", "fre_t",
        "cntr_t", "desp_t", "inad_t", "rec_t",
        "extita_t",
        # -> Base de dados
        rep("dados", 15),
        "dash",
        # -> Relatórios
        "cext", "dfc", "rinad",
        # -> Decisões
        rep("fco", 2), "dinad"
      )
  ) %>%
    mutate(
      color =
        case_when(
          from %in% c(
            # Fontes
            "ana", "cef", "ifk", "ita",
            # Arquivos
            "com",
            "cmfcn", "dcd", "ecn", "epr", "extcef", "fre",
            "cntr", "desp", "inad", "rec",
            "extita"
          ) ~ "black",
          from %in% c(
            "com_t"
          ) |
            to %in% c(
              "cext", "dfc", "fco"
            ) ~ "red",
          from %in% c("fre_t") ~ "yellow",
          TRUE ~ "green"
        )
    )
}

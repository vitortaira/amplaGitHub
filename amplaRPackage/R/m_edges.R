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
  edges_df <-
    data.frame(
      from =
        c(
          # Arquivo
          "desp", "rec", "ecn", "fre",
          # Origem
          "informakon", "cef",
          # Base de dados
          "dados", "dados",
          # Relatório
          "relatorio.financiamento", "dfc"
        ),
      to =
        c(
          # Arquivo ->
          rep("informakon", 2), rep("cef", 2),
          # Origem ->
          rep("dados", 2),
          # Base de dados ->
          "relatorio.financiamento", "dfc",
          # Relatório ->
          rep("decisao", 2)
        )
    ) %>%
    mutate(
      color =
        case_when(
          from %in% c("desp", "rec", "ecn", "informakon") ~ "green",
          from %in% c("fre", "cef") ~ "yellow",
          TRUE ~ "red"
        ),
      label = "",
      title = ""
    )
}

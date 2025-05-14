#' @title Create graph nodes and legends for data flow visualization
#'
#' @description
#' Creates nodes and legends dataframes for use with visNetwork
#'
#' @return A list containing two dataframes:
#' \itemize{
#'   \item nodes_df: Node definitions for the flow chart
#'   \item nodes.legends_df: Legend definitions for the flow chart
#' }
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr %>%
#' @export
m_nodes <- function() {
  # Arquivos
  labels.arquivos_vc <- c(
    # Informakon
    "Despesas", "Receitas",
    # CEF
    "ECN", "FRE"
  )

  # Origens
  labels.origens_vc <- c(
    "Informakon", "Caixa Econômica Federal"
  )

  # Base de dados
  label.dados_c <- "Base de dados"

  # Relatórios
  labels.relatorios_vc <- c(
    # Decisões
    "Financiamento dos custos das obras", "Demonstrativo de fluxo de caixa"
  )

  # Decisões
  labels.decisoes_vc <- c(
    "Decisão"
  )

  # Nodes
  labels.nodes_vc <- c(
    labels.arquivos_vc,
    labels.origens_vc,
    label.dados_c,
    labels.relatorios_vc,
    labels.decisoes_vc
  )

  # Arquivos
  ids.arquivos_vc <- c(
    "desp", "rec", "ecn", "fre"
  )

  # Origens
  ids.origens_vc <- c(
    "informakon", "cef"
  )

  # Base de dados
  id.dados_c <- "dados"

  # Relatórios
  ids.relatorios_vc <- c(
    "relatorio.financiamento", "dfc"
  )

  # Decisões
  ids.decisoes_vc <- c(
    "decisao"
  )

  ids.nodes_vc <- c(
    ids.arquivos_vc,
    ids.origens_vc,
    id.dados_c,
    ids.relatorios_vc,
    ids.decisoes_vc
  )

  # nodes_df
  nodes_df <- data.frame(
    id = ids.nodes_vc,
    label = labels.nodes_vc,
    group = c(
      rep("arquivo", length(ids.arquivos_vc)),
      rep("origem", length(ids.origens_vc)),
      "base",
      rep("relatorio", length(ids.relatorios_vc)),
      rep("decisao", length(ids.decisoes_vc))
    ),
    level = c(
      rep(2, length(ids.arquivos_vc)), # Arquivo
      rep(3, length(ids.origens_vc)), # Origem
      4, # Base de dados
      rep(5, length(ids.relatorios_vc)), # Relatório
      rep(6, length(ids.decisoes_vc)) # Decisão
    )
  ) %>%
    mutate(
      color = case_when(
        level == 1 ~ "lightyellow",
        level == 2 ~ "yellow",
        level == 3 ~ "orange",
        level == 4 ~ "red",
        level == 5 ~ "blue",
        level == 6 ~ "purple"
      ),
      shape = "box",
      size = 25,
      font = case_when(
        level %in% c(5, 6) ~ list(list(
          "color" = "white",
          "size" = 25,
          "face" = "arial",
          "background" = "undefined"
        )),
        !(level %in% c(5, 6)) ~ list(list(
          "color" = "black",
          "size" = 25,
          "face" = "arial",
          "background" = "undefined"
        ))
      )
    )

  # Create legends dataframe
  nodes.levels_vn <- sort(unique(nodes_df$level))
  nodes.levels.labels_vc <- c(
    "Arquivos", "Origens", "Base de dados", "Relatórios", "Decisões"
  )
  nodes.levels.colors_vc <- c("yellow", "orange", "red", "blue", "purple")

  nodes.legends_df <- data.frame(
    label = nodes.levels.labels_vc,
    color = nodes.levels.colors_vc
  ) %>%
    mutate(
      font = case_when(
        label %in% c("Relatórios", "Decisões") ~ list(list(
          "color" = "white",
          "size" = 50,
          "face" = "arial",
          "background" = "undefined"
        )),
        !(label %in% c("Relatórios", "Decisões")) ~ list(list(
          "color" = "black",
          "size" = 50,
          "face" = "arial",
          "background" = "undefined"
        ))
      ),
      shape = "box",
      stringsAsFactors = FALSE
    )

  # Return the nodes dataframes
  list(
    nodes = nodes_df,
    nodes.legends = nodes.legends_df
  )
}

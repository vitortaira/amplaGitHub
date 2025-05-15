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
  # Origens
  labels.origens_vc <- c(
    "ANAPRO", "Caixa Econômica Federal", "Informakon", "Itaú"
  )
  # Arquivos
  labels.arquivos_vc <- c(
    # ANAPRO
    "Comercial",
    # CEF
    "ECN", "FRE",
    # Informakon
    "Contratos", "Despesas", "Inadimplentes", "Receitas",
    # Itaú
    "Extratos"
  )

  # Tabelas
  labels.tabelas_c <- c(
    # ANAPRO
    "Comercial",
    # CEF
    "ECN_C", "ECN_I", "ECN_PJ", "ECN_U", "FRE",
    # Informakon
    "Contratos", "Despesas", "Inadimplentes", "Receitas",
    # Itaú
    "Extratos"
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
    labels.origens_vc,
    labels.arquivos_vc,
    labels.tabelas_c,
    label.dados_c,
    labels.relatorios_vc,
    labels.decisoes_vc
  )

  # Origens
  ids.origens_vc <- c(
    "ana", "cef", "ifk", "ita"
  )
  # Arquivos
  ids.arquivos_vc <- c(
    # ANAPRO
    "com",
    # CEF
    "ecn", "fre",
    # Informakon
    "cntr", "desp", "inad", "rec",
    # Itaú
    "extcef"
  )

  # Tabelas
  ids.tabelas_c <- c(
    # ANAPRO
    "com_t",
    # CEF
    "ecn_c", "ecn_i", "ecn_pj", "ecn_u", "fre_t",
    # Informakon
    "cntr_t", "desp_t", "inad_t", "rec_t",
    # Itaú
    "extita_t"
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
    ids.origens_vc,
    ids.arquivos_vc,
    ids.tabelas_c,
    id.dados_c,
    ids.relatorios_vc,
    ids.decisoes_vc
  )

  # nodes_df
  nodes_df <- data.frame(
    id = ids.nodes_vc,
    label = labels.nodes_vc,
    group = c(
      rep("origem", length(ids.origens_vc)),
      rep("arquivo", length(ids.arquivos_vc)),
      rep("tabela", length(ids.tabelas_c)),
      "base",
      rep("relatorio", length(ids.relatorios_vc)),
      rep("decisao", length(ids.decisoes_vc))
    ),
    level = c(
      rep(1, length(ids.origens_vc)), # Origem
      rep(2, length(ids.arquivos_vc)), # Arquivo
      rep(3, length(ids.tabelas_c)), # Tabela
      4, # Base de dados
      rep(5, length(ids.relatorios_vc)), # Relatório
      rep(6, length(ids.decisoes_vc)) # Decisão
    )
  ) %>%
    mutate(
      color = case_when(
        level == 1 ~ "red",
        level == 2 ~ "orange",
        level == 3 ~ "yellow",
        level == 4 ~ "black",
        level == 5 ~ "lightblue",
        level == 6 ~ "gray"
      ),
      shape = "box",
      size = 25,
      font = case_when(
        level %in% c(4) ~ list(list(
          "color" = "white",
          "size" = 25,
          "face" = "arial",
          "background" = "undefined"
        )),
        !(level %in% c(4)) ~ list(list(
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
    "Origens", "Arquivos", "Tabelas", "Base de dados", "Relatórios", "Decisões"
  )
  nodes.levels.colors_vc <- c(
    "red", "orange", "yellow", "black", "lightblue", "gray"
  )

  nodes.legends_df <- data.frame(
    label = nodes.levels.labels_vc,
    color = nodes.levels.colors_vc
  ) %>%
    mutate(
      font = case_when(
        label %in% c("Base de dados") ~ list(list(
          "color" = "white",
          "size" = 50,
          "face" = "arial",
          "background" = "undefined"
        )),
        !(label %in% c("Base de dados")) ~ list(list(
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

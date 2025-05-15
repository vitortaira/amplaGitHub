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
  nodes_t <- tibble(
    id = character(),
    label = character(),
    group = character(),
    level = numeric(),
    color = character(),
    shape = character(),
    size = numeric(),
    font = list()
  )
  # Origens
  ana_n <- tibble(
    id = "ana",
    label = "ANAPRO",
    group = "origem",
    level = 1,
    color = "darkblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  cef_n <- tibble(
    id = "cef",
    label = "Caixa Econômica Federal",
    group = "origem",
    level = 1,
    color = "darkblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ifk_n <- tibble(
    id = "ifk",
    label = "Informakon",
    group = "origem",
    level = 1,
    color = "darkblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ita_n <- tibble(
    id = "ita",
    label = "Itaú",
    group = "origem",
    level = 1,
    color = "darkblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  # Arquivos
  com_n <- tibble(
    id = "com",
    label = "Comercial",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  cmfcn_n <- tibble(
    id = "cmfcn",
    label = "CMF_CN",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  dcd_n <- tibble(
    id = "dcd",
    label = "DCD",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ecn_n <- tibble(
    id = "ecn",
    label = "ECN",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  epr_n <- tibble(
    id = "epr",
    label = "EPR",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  extcef_n <- tibble(
    id = "extcef",
    label = "Extratos",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  fre_n <- tibble(
    id = "fre",
    label = "FRE",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  cntr_n <- tibble(
    id = "cntr",
    label = "Contratos",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  desp_n <- tibble(
    id = "desp",
    label = "Despesas",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  inad_n <- tibble(
    id = "inad",
    label = "Inadimplentes",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  rec_n <- tibble(
    id = "rec",
    label = "Receitas",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  extita_n <- tibble(
    id = "extita",
    label = "Extratos",
    group = "arquivo",
    level = 2,
    color = "blue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  # Tabelas
  com_t_n <- tibble(
    id = "com_t",
    label = "Comercial",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  cmfcn_t_n <- tibble(
    id = "cmfcn_t",
    label = "CMF_CN",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  dcd_t_n <- tibble(
    id = "dcd_t",
    label = "DCD",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ecn_c_n <- tibble(
    id = "ecn_c",
    label = "ECN_C",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ecn_i_n <- tibble(
    id = "ecn_i",
    label = "ECN_I",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ecn_pj_n <- tibble(
    id = "ecn_pj",
    label = "ECN_PJ",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  ecn_u_n <- tibble(
    id = "ecn_u",
    label = "ECN_U",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  epr_t_n <- tibble(
    id = "epr_t",
    label = "EPR",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  extcef_t_n <- tibble(
    id = "extcef_t",
    label = "Extratos",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  fre_t_n <- tibble(
    id = "fre_t",
    label = "FRE",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  cntr_t_n <- tibble(
    id = "cntr_t",
    label = "Contratos",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  desp_t_n <- tibble(
    id = "desp_t",
    label = "Despesas",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  inad_t_n <- tibble(
    id = "inad_t",
    label = "Inadimplentes",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  rec_t_n <- tibble(
    id = "rec_t",
    label = "Receitas",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  extita_t_n <- tibble(
    id = "extita_t",
    label = "Extratos",
    group = "tabela",
    level = 3,
    color = "lightblue",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  # Base de dados
  dados_n <- tibble(
    id = "dados",
    label = "Base de dados",
    group = "base",
    level = 4,
    color = "black",
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 50, face = "arial",
      background = "undefined"
    ))
  )
  # Relatórios
  relatorio_financiamento_n <- tibble(
    id = "relatorio.financiamento",
    label = "Financiamento dos custos das obras",
    group = "relatorio",
    level = 5,
    color = "green",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  dfc_n <- tibble(
    id = "dfc",
    label = "Demonstrativo de fluxo de caixa",
    group = "relatorio",
    level = 5,
    color = "green",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  # Decisões
  decisao_n <- tibble(
    id = "decisao",
    label = "Decisão",
    group = "decisao",
    level = 6,
    color = "gray",
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )
  # Create nodes dataframe
  nodes_t <- bind_rows(
    # Tibble placeholder
    nodes_t,
    # Origens
    ana_n, cef_n, ifk_n, ita_n,
    # Arquivos
    com_n, cmfcn_n, dcd_n, ecn_n, epr_n, extcef_n, fre_n, cntr_n, desp_n,
    inad_n, rec_n, extita_n,
    # Tabelas
    com_t_n, cmfcn_t_n, dcd_t_n, ecn_c_n, ecn_i_n, ecn_pj_n, ecn_u_n, epr_t_n,
    extcef_t_n, fre_t_n,
    cntr_t_n, desp_t_n, inad_t_n, rec_t_n, extita_t_n,
    # Base de dados
    dados_n,
    # Relatórios
    relatorio_financiamento_n, dfc_n,
    # Decisões
    decisao_n
  )
#     mutate(
#       color = case_when(
#         level == 1 ~ "darkblue",
#         level == 2 ~ "blue",
#         level == 3 ~ "lightblue",
#         level == 4 ~ "black",
#         level == 5 ~ "green",
#         level == 6 ~ "gray"
#       ),
#       shape = "box",
#       size = 25,
#       font = case_when(
#         level %in% c(4) ~ list(list(
#           "color" = "white",
#           "size" = 25,
#           "face" = "arial",
#           "background" = "undefined"
#         )),
#         !(level %in% c(4)) ~ list(list(
#           "color" = "black",
#           "size" = 25,
#           "face" = "arial",
#           "background" = "undefined"
#         ))
#       )
#     )

  # Create legends dataframe
  nodes.levels_vn <- sort(unique(nodes_t$level))
  nodes.levels.labels_vc <- c(
    "Origens", "Arquivos", "Tabelas", "Base de dados", "Relatórios", "Decisões"
  )
  nodes.levels.colors_vc <- c(
    "red", "orange", "yellow", "black", "lightblue", "gray"
  )

  nodes.legends_t <- tibble(
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
    nodes = nodes_t,
    nodes.legends = nodes.legends_t
  )
}

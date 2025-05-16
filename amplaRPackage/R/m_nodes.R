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
  dados_n <- tibble(
    id = "dados",
    label = "Base de dados",
    group = "base",
    level = 4,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f1c0",
    icon.size = 100,
    icon.color = "black",
    font = list(list(
      color = "black",
      size  = 30,
      face  = "arial"
    ))
  )

  dash_n <- tibble(
    id = "dash",
    label = "Dashboard",
    group = "base",
    level = 4,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0e4",
    icon.size = 100,
    icon.color = "black",
    font = list(list(
      color = "black", size = 30, face = "arial"
    ))
  )
  # Relatórios
  cext_n <- tibble(
    id = "cext",
    label = "Conciliação de extratos",
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
  rinad_n <- tibble(
    id = "rinad",
    label = "Inadimplência",
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
  fco_n <- tibble(
    id = "fco",
    label = "Financiamento de obras",
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
  dinad_n <- tibble(
    id = "dinad",
    label = "Política de inadimplência",
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
  land_n <- tibble(
    id = "land",
    label = "Landbanking",
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
  prdt_n <- tibble(
    id = "prdt",
    label = "Definição do produto",
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
  vbld_n <- tibble(
    id = "vbld",
    label = "Viabilidade",
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
  pqmc_n <- tibble(
    id = "pqmc",
    label = "Pesquisa de mercado",
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
  prcf_n <- tibble(
    id = "prcf",
    label = "Precificação e mix de vendas",
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
  cnst_n <- tibble(
    id = "cnst",
    label = "Construção",
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
  cmmk_n <- tibble(
    id = "cmmk",
    label = "Comercial e marketing",
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
  jdtb_n <- tibble(
    id = "jdtb",
    label = "Juridico e tributário",
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
  ctob_n <- tibble(
    id = "ctob",
    label = "Controle das obras",
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
  ctcst_n <- tibble(
    id = "ctcst",
    label = "Controle de custos",
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
    dados_n, dash_n,
    # Relatórios
    cext_n, dfc_n, rinad_n,
    # Decisões
    fco_n, dinad_n, land_n, prdt_n, vbld_n, pqmc_n, prcf_n, cnst_n,
    cmmk_n, jdtb_n, ctob_n, ctcst_n
  )

  # Get unique colors for each level/group
  origin_color <- nodes_t %>%
    filter(group == "origem") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]
  files_color <- nodes_t %>%
    filter(group == "arquivo") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]
  tables_color <- nodes_t %>%
    filter(group == "tabela") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]
  database_color <- nodes_t %>%
    filter(group == "base") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]
  reports_color <- nodes_t %>%
    filter(group == "relatorio") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]
  decisions_color <- nodes_t %>%
    filter(group == "decisao") %>%
    distinct(color) %>%
    pull(color) %>%
    .[1]

  # Create legends as individual tibbles in a more consistent way with the other nodes
  # Legend for origins
  origin_legend <- tibble(
    id = "legend_origin",
    label = "Origens",
    title = "Sistemas que fornecem os dados",
    group = "legend",
    color = origin_color,
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )

  # Legend for files
  files_legend <- tibble(
    id = "legend_files",
    label = "Arquivos",
    title = "Arquivos extraídos dos sistemas",
    group = "legend",
    color = files_color,
    shape = "box",
    size = 25,
    font = list(list(
      color = "white", size = 25, face = "arial",
      background = "undefined"
    ))
  )

  # Legend for tables
  tables_legend <- tibble(
    id = "legend_tables",
    label = "Tabelas",
    title = "Dados estruturados em tabelas",
    group = "legend",
    color = tables_color,
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )

  # Legend for reports
  reports_legend <- tibble(
    id = "legend_reports",
    label = "Relatórios",
    title = "Informações consolidadas",
    group = "legend",
    color = reports_color,
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )

  # Legend for decisions
  decisions_legend <- tibble(
    id = "legend_decisions",
    label = "Decisões",
    title = "Ações baseadas em informações",
    group = "legend",
    color = decisions_color,
    shape = "box",
    size = 25,
    font = list(list(
      color = "black", size = 25, face = "arial",
      background = "undefined"
    ))
  )

  # Combine all legend nodes
  nodes.legends_t <- bind_rows(
    origin_legend,
    files_legend,
    tables_legend,
    reports_legend,
    decisions_legend
  )

  # Return the nodes dataframes
  list(
    nodes = nodes_t,
    nodes.legends = nodes.legends_t
  )
}

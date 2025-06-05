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
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f041",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cef_n <- tibble(
    id = "cef",
    label = "Caixa Econômica Federal",
    group = "origem",
    level = 1,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f041",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ifk_n <- tibble(
    id = "ifk",
    label = "Informakon",
    group = "origem",
    level = 1,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f041",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ita_n <- tibble(
    id = "ita",
    label = "Itaú",
    group = "origem",
    level = 1,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f041",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  # arquivos
  com_n <- tibble(
    id = "com",
    label = "Comercial",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cmfcn_n <- tibble(
    id = "cmfcn",
    label = "CMF_CN",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  dcd_n <- tibble(
    id = "dcd",
    label = "DCD",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ecn_n <- tibble(
    id = "ecn",
    label = "ECN",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  epr_n <- tibble(
    id = "epr",
    label = "EPR",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  extcef_n <- tibble(
    id = "extcef",
    label = "Extratos",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  fre_n <- tibble(
    id = "fre",
    label = "FRE",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cntr_n <- tibble(
    id = "cntr",
    label = "Contratos",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  desp_n <- tibble(
    id = "desp",
    label = "Despesas",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  inad_n <- tibble(
    id = "inad",
    label = "Inadimplentes",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  rec_n <- tibble(
    id = "rec",
    label = "Receitas",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  extita_n <- tibble(
    id = "extita",
    label = "Extratos",
    group = "arquivo",
    level = 2,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  # Tabelas
  com_t_n <- tibble(
    id = "com_t",
    label = "Comercial",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cmfcn_t_n <- tibble(
    id = "cmfcn_t",
    label = "CMF_CN",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  dcd_t_n <- tibble(
    id = "dcd_t",
    label = "DCD",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ecn_c_n <- tibble(
    id = "ecn_c",
    label = "ECN_C",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ecn_i_n <- tibble(
    id = "ecn_i",
    label = "ECN_I",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ecn_pj_n <- tibble(
    id = "ecn_pj",
    label = "ECN_PJ",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ecn_u_n <- tibble(
    id = "ecn_u",
    label = "ECN_U",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  epr_t_n <- tibble(
    id = "epr_t",
    label = "EPR",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  extcef_t_n <- tibble(
    id = "extcef_t",
    label = "Extratos",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  fre_t_n <- tibble(
    id = "fre_t",
    label = "FRE",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cntr_t_n <- tibble(
    id = "cntr_t",
    label = "Contratos",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  desp_t_n <- tibble(
    id = "desp_t",
    label = "Despesas",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  inad_t_n <- tibble(
    id = "inad_t",
    label = "Inadimplentes",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  rec_t_n <- tibble(
    id = "rec_t",
    label = "Receitas",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  extita_t_n <- tibble(
    id = "extita_t",
    label = "Extratos",
    group = "tabela",
    level = 3,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  # Base
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
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f05a",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  dfc_n <- tibble(
    id = "dfc",
    label = "Demonstrativo de fluxo de caixa",
    group = "relatorio",
    level = 5,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f05a",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  rinad_n <- tibble(
    id = "rinad",
    label = "Inadimplência",
    group = "relatorio",
    level = 5,
    color = "black",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f05a",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  # Decisões
  fco_n <- tibble(
    id = "fco",
    label = "Financiamento de obras",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  dinad_n <- tibble(
    id = "dinad",
    label = "Política de inadimplência",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  land_n <- tibble(
    id = "land",
    label = "Landbanking",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  prdt_n <- tibble(
    id = "prdt",
    label = "Definição do produto",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  vbld_n <- tibble(
    id = "vbld",
    label = "Viabilidade",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  pqmc_n <- tibble(
    id = "pqmc",
    label = "Pesquisa de mercado",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  prcf_n <- tibble(
    id = "prcf",
    label = "Precificação e mix de vendas",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cnst_n <- tibble(
    id = "cnst",
    label = "Construção",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  cmmk_n <- tibble(
    id = "cmmk",
    label = "Comercial e marketing",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  jdtb_n <- tibble(
    id = "jdtb",
    label = "Juridico e tributário",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ctob_n <- tibble(
    id = "ctob",
    label = "Controle das obras",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )
  ctcst_n <- tibble(
    id = "ctcst",
    label = "Controle de custos",
    group = "decisao",
    level = 6,
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 75,
    icon.color = "black",
    font = list(list(color = "black", size = 25, face = "arial"))
  )

  # Create nodes dataframe
  nodes_t <- bind_rows(
    # Tibble placeholder
    nodes_t,
    # Origens
    ana_n, cef_n, ifk_n, ita_n,
    # arquivos
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

  # Legend for origins
  fnt <- tibble(
    id = "fnt",
    label = "Fonte",
    title = "Origem dos dados.",
    group = "legend",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f041",
    icon.size = 25,
    icon.color = "black",
    font = list(list(color = "black", size = 10, face = "arial"))
  )

  # Legend for files
  arq <- tibble(
    id = "arq",
    label = "Tipo do arquivo",
    title = "Um tipo de arquivo que precisamos consultar regularmente.",
    group = "legend",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0c5",
    icon.size = 25,
    icon.color = "black",
    font = list(list(color = "black", size = 10, face = "arial"))
  )

  # Legend for tables
  tbl <- tibble(
    id = "tbl",
    label = "Tabelas",
    title = "Dados estruturados em tabelas.",
    group = "legend",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f0ce",
    icon.size = 25,
    icon.color = "black",
    font = list(list(color = "black", size = 10, face = "arial"))
  )

  # Legend for reports
  rlt <- tibble(
    id = "rlt",
    label = "Relatório",
    title = "Uma análise dos dados.",
    group = "legend",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f05a",
    icon.size = 25,
    icon.color = "black",
    font = list(list(color = "black", size = 10, face = "arial"))
  )

  # Legend for decisions
  dcs <- tibble(
    id = "dcs",
    label = "Decisão",
    title = "Decisão a ser tomada.",
    group = "legend",
    shape = "icon",
    icon.face = "FontAwesome",
    icon.code = "f059",
    icon.size = 25,
    icon.color = "black",
    font = list(list(color = "black", size = 10, face = "arial"))
  )

  # Combine all legend nodes
  nodes.legends_t <- bind_rows(
    fnt,
    arq,
    tbl,
    rlt,
    dcs
  )

  # Return the nodes dataframes
  list(
    nodes = nodes_t,
    nodes.legends = nodes.legends_t
  )
}

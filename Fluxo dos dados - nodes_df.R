# nodes_df ----------------------------------------------------------------

# Labels
  # Variáveis
  labels.variaveis_vc <- 
    c(
      # Despesas
      "Data Doc Pagto", "Data Liberação", "Credor", "Cod. Centro",
      "Centro de Negócio", "Agente Financeiro", "N° Conta", "N° Entrada",
      "Documento", "Parcela", "Data Vencimento", "Data Vencimento Origem",
      "Valor Título", "Acréscimos", "Descontos", "Encargos",
      "Descontos Adiant.", "Multa", "Total Pago", "a/c", "Observação",
      # Receitas
      "Empreendimento", "Cliente", "Contrato", "Torre", "Apto", "Esp",
      "Parcela", "Elemento", "Vencimento", "Data Pagto", "R/F", "Agente",
      "Principal", "Juros", "Reajuste", "Encargos", "Juros de Mora", "Multa",
      "Seguro", "Desconto", "Cart.", "Total",
      # ECN
        # Empreendimento
      "Contrato", "APF", "Valor Aporte", "Total de Unidade",
      "Unidades Comercializadas", "Unidades Financiadas Construção",
      "Unidades Complementares", "Data Término de Obra", "Data Ini - Enc Fiador",
        # Empréstimo PJ
      "Número", "Data da Assinatura", "Valor Empréstimo", "Valor Reduzido",
      "Valor Utilizado", "Saldo Devedor",
        # Unidades
      "Contrato", "TP", "Data de Assinatura", "Data de Inclusão",
      "Data de Registro", "Financiamento", "Desconto Subsídio", "FGTS",
      "Recursos Próprios", "Compra / Venda", "Valor de Avaliação",
      "Valor Liberado Terreno", "Valor Liberado Obra", "Amortização",
        # Consolidado
      "Período", "Unidade", "Valor Creditado", "Valor Desbloqueado",
      "Valor Amortizado", "Encargo Quitado do PJ",
      # FRE
      "Custo total do empreendimento", "Valor geral de vendas"
    )
  # Arquivos
  labels.arquivos_vc <- 
    c(
      # Informakon
      "Despesas", "Receitas",
      # CEF
      "ECN", "FRE"
    )
  # Origens
  labels.origens_vc <- 
    c(
      "Informakon", "Caixa Econômica Federal"
    )
  # Base de dados
  label.dados_c <- "Base de dados"
  # Relatórios
  labels.relatorios_vc <- 
    c(
      # Decisões
      "Financiamento dos custos das obras", "Demonstrativo de fluxo de caixa"
    )
  # Decisões
  labels.decisoes_vc <- 
    c(
      "Decisão"
    )
  # Nodes
  labels.nodes_vc <- 
    c(
      labels.variaveis_vc,
      labels.arquivos_vc,
      labels.origens_vc,
      label.dados_c,
      labels.relatorios_vc,
      labels.decisoes_vc
    )
# IDs
  # Variáveis
  ids.variaveis_vc <- 
    c(
      # Despesas
      "desp.data.pagto", "desp.data.liberacao", "desp.credor",
      "desp.cod.centro", "desp.centro", "desp.agente", "desp.conta",
      "desp.entrada", "desp.documento", "desp.parcela", "desp.data.vencimento",
      "desp.data.vencimento.origem", "desp.titulo", "desp.acrescimos",
      "desp.descontos", "desp.encargos", "desp.descontos.adiant", "desp.multa",
      "desp.pago", "desp.ac", "desp.obs",
      # Receitas
      "rec.empreendimento", "rec.cliente", "rec.contrato", "rec.torre",
      "rec.apto", "rec.esp", "rec.parcela", "rec.elemento", "rec.vencimento",
      "rec.data.pagto", "rec.rf", "rec.agente", "rec.principal", "rec.juros",
      "rec.reajuste", "rec.encargos", "rec.juros.mora", "rec.multa",
      "rec.seguro", "rec.desconto", "rec.cart", "rec.total",
      # ECN
        # Empreendimento
      "contrato.pj", "apf", "aporte", "unidades", "unidades.comercializadas", 
      "unidades.financiadas.contrucao", "unidades.complementares",
      "data.termino.obra", "data.ini.enc.fiador",
        # Empréstimo PJ
      "numero", "data.assinatura.pj", "valor.emprestimo", "valor.reduzido",
      "valor.utilizado", "saldo.devedor",
        # Unidades
      "contrato.unidade", "tp", "data.assinatura.unidade", "data.inclusao", "data.registro",
      "financiamento", "desconto.subsidio", "fgts", "recursos.proprios",
      "compra.venda", "valor.avaliacao", "valor.liberado.terreno",
      "valor.liberado.obra", "amortizacao",
        # Consolidado
      "periodo", "unidade", "valor.creditado", "valor.desbloqueado",
      "valor.amortizado", "encargo.quitado.pj",
      # FRE
      "custo.empreendimento", "vgv"
    )
  # Arquivos
  ids.arquivos_vc <- 
    c(
      "desp", "rec", "ecn", "fre"
    )
  # Origens
  ids.origens_vc <- 
    c(
      "informakon", "cef"
    )
  # Base de dados
  id.dados_c <- "dados"
  # Relatórios
  ids.relatorios_vc <- 
    c(
      "relatorio.financiamento", "dfc"
    )
  # Decisões
  ids.decisoes_vc <- 
    c(
      "decisao"
    )
ids.nodes_vc <- 
  c(
    ids.variaveis_vc,
    ids.arquivos_vc,
    ids.origens_vc,
    id.dados_c,
    ids.relatorios_vc,
    ids.decisoes_vc
  )
# nodes_df
nodes_df <- 
  data.frame(
    id = ids.nodes_vc,
    label = labels.nodes_vc,
    group = 
      c(
        rep("variavel", length(ids.variaveis_vc)),
        rep("arquivo", length(ids.arquivos_vc)),
        rep("origem", length(ids.origens_vc)),
        "base",
        rep("relatorio", length(ids.relatorios_vc)),
        rep("decisao", length(ids.decisoes_vc))
      ),
    level = c(
      rep(1, length(ids.variaveis_vc)),  # Variável     
      rep(2, length(ids.arquivos_vc)),   # Arquivo      
      rep(3, length(ids.origens_vc)),    # Origem       
      4,                                 # Base de dados
      rep(5, length(ids.relatorios_vc)), # Relatório    
      rep(6, length(ids.decisoes_vc))    # Decisão      
    )
  ) %>% 
  mutate(
    color =
      case_when(
        level == 1 ~ "lightyellow",
        level == 2 ~ "yellow",
        level == 3 ~ "orange",
        level == 4 ~ "red",
        level == 5 ~ "blue",
        level == 6 ~ "purple"
      ),
    shape = "box",
    size = 25,
    font = 
      case_when(
        level %in% c(5, 6) ~
          list(list(
            "color" = "white",
            "size" = 14,
            "face" = "arial",
            "background" = "undefined"
          )),
        !(level %in% c(5, 6)) ~
          list(list(
            "color" = "black",
            "size" = 14,
            "face" = "arial",
            "background" = "undefined"
          )),
      )
  )

# nodes.legends_df
nodes.levels_vn <- sort(unique(nodes_df$level))
nodes.levels.labels_vc <-
  c(
    "Variáveis", "Arquivos", "Origens", "Base de dados", "Relatórios", "Decisões"
  )
nodes.levels.colors_vc <-
  c("lightyellow", "yellow", "orange", "red", "blue", "purple")
nodes.legends_df <- 
  data.frame(
    label = nodes.levels.labels_vc,
    color = nodes.levels.colors_vc) %>% 
  mutate(
    font = 
      case_when(
        label %in% c("Relatórios", "Decisões") ~
          list(list(
            "color" = "white",
            "size" = 14,
            "face" = "arial",
            "background" = "undefined"
          )),
        !(label %in% c("Relatórios", "Decisões")) ~
          list(list(
            "color" = "black",
            "size" = 14,
            "face" = "arial",
            "background" = "undefined"
          )),
      ),
        shape = "box",
    stringsAsFactors = F
  )

# Excluir objetos redundantes
rm(
  # Variáveis
  "labels.variaveis_vc", "ids.variaveis_vc",
  # Arquivos
  "labels.arquivos_vc", "ids.arquivos_vc",
  # Origens
  "labels.origens_vc", "ids.origens_vc",
  # Base de dados
  "label.dados_c", "id.dados_c",
  # Relatórios
  "labels.relatorios_vc", "ids.relatorios_vc",
  # Decisões
  "labels.decisoes_vc", "ids.decisoes_vc",
  # Legendas
  "node.legenda.variaveis_df", "node.legenda.arquivos_df", 
  "node.legenda.origens_df", "node.legenda.relatorios_df", 
  "node.legenda.decisoes_df",
  # Subconjuntos de nodes_df
  "nodes.legendas_df", "nodes.conteudo_df"
)
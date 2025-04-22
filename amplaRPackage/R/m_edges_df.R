# Definição das arestas principais
edges_df <- 
  data.frame(
    from = 
      c(
#        # Variável
#          # Despesas
#        "desp.data.pagto", "desp.data.liberacao", "desp.credor",
#        "desp.cod.centro", "desp.centro", "desp.agente", "desp.conta",
#        "desp.entrada", "desp.documento", "desp.parcela", "desp.data.vencimento",
#        "desp.data.vencimento.origem", "desp.titulo", "desp.acrescimos",
#        "desp.descontos", "desp.encargos", "desp.descontos.adiant", "desp.multa",
#        "desp.pago",
#          # Receitas
#        "rec.empreendimento", "rec.cliente", "rec.contrato", "rec.torre",
#        "rec.apto", "rec.esp", "rec.parcela", "rec.elemento", "rec.vencimento",
#        "rec.data.pagto", "rec.rf", "rec.agente", "rec.principal", "rec.juros",
#        "rec.reajuste", "rec.encargos", "rec.juros.mora", "rec.multa",
#        "rec.seguro", "rec.desconto", "rec.cart", "rec.total",
#        # ECN
#          # Empreendimento
#        "contrato.pj", "apf", "aporte", "unidades", "unidades.comercializadas", 
#        "unidades.financiadas.contrucao", "unidades.complementares",
#        "data.termino.obra", "data.ini.enc.fiador",
#          # Empréstimo PJ
#        "numero", "data.assinatura.pj", "valor.emprestimo", "valor.reduzido",
#        "valor.utilizado", "saldo.devedor",
#          # Unidades
#        "contrato.unidade", "tp", "data.assinatura.unidade", "data.inclusao", "data.registro",
#        "financiamento", "desconto.subsidio", "fgts", "recursos.proprios",
#        "compra.venda", "valor.avaliacao", "valor.liberado.terreno",
#        "valor.liberado.obra", "amortizacao",
#          # Consolidado
#        "periodo", "unidade", "valor.creditado", "valor.desbloqueado",
#        "valor.amortizado", "encargo.quitado.pj",
#        # FRE
#        "custo.empreendimento", "vgv",
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
#        # Variável ->
#          # Despesas
#        rep("desp", 19),
#          # Receitas
#        rep("rec", 22),
#        # ECN
#        rep("ecn", 35),
#          # FRE
#        rep("fre", 2),
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
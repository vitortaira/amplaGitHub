# Descrição ---------------------------------------------------------------

### RESUMO ###

# e_cef_dcd_resumo() extrai os dados de um relatório DCD da CEF em PDF.

### UTILIZAÇÃO ###

# e_cef_dcd_resumo(
#   f_caminho.arquivo_c
# )

### ARGUMENTOS ###

# f_caminho.arquivo_c: String do caminho de um relatório DCD da CEF em PDF.

e_cef_dcd_resumo <-
  function(f_caminho.arquivo_c) {
    # Define paginas_l
    paginas_l <-
      pdf_text(f_caminho.arquivo_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    linhas_c <- unlist(paginas_l, use.names = FALSE)
    indice.fim_i <- linhas_c %>%
      str_which("(?i)^base\\s?de\\s?calculo\\s?para\\s?geracao\\s?do\\s?cronograma") %>%
      nth(1) - 1
    linhas.resumo_c <-
      linhas_c[1:indice.fim_i]
    # Primeira coluna
    `NUMERO DO CONTRATO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i)numero\\s?do\\s?pedido\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i)numero\\s?do\\s?empreendimento\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `QUANTIDADE DE PARCELAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i)quantidade\\s?de\\s?parcelas\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO APF` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?apf")) %>%
      str_remove("(?i)numero\\s?do\\s?apf\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGC` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgc")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgc\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SRE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sre")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sre\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGP` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgp")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgp\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGT` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgt")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgt\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `QUANTIDADE DE UNIDADES` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?unidades")) %>%
      str_remove("(?i)quantidade\\s?de\\s?unidades\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR CUSTO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i)vr\\s?custo\\s?obra\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `ORCAMENTO COMPRA/VENDA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento\\s?compra/venda")) %>%
      str_remove("(?i)orcamento\\s?compra/venda\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR COMPRA/VENDA UNIDADE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?unidade")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?unidade\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR COMPRA/VENDA TERRENO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?terreno")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?terreno\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO MUTUARIO (PF)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pf\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pf\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO APORTE CONSTRUTORA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?aporte\\s?construtora")) %>%
      str_remove("(?i)saldo\\s?aporte\\s?construtora\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO MUTUARIO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO DEVEDOR (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?devedor\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?devedor\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SUBSIDIO RES. 460` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio\\s?res.\\s?460")) %>%
      str_remove("(?i)subsidio\\s?res.\\s?460\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `TOTAL SUPLEMENTACAO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)total\\s?suplementacao\\s?\\(pj\\)")) %>%
      str_remove("(?i)total\\s?suplementacao\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `MAXIMO LIB. GERAL (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `MAXIMO LIB. ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `RECOMPOSICAO ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)recomposicao\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    # Segunda coluna
    `EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove(".*\\d{12}\\s?") %>%
      str_remove("(?i)\\s?origem\\s?de\\s?recurso.*")
    `CODIGO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i).*codigo\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?linha\\s?de\\s?financiamento\\s?.*")
    `SITUACAO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i).*situacao\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?tipo\\s?de\\s?financiamento\\s?.*")
    `DATA TERMINO SUSPENSIVA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i).*data\\s?termino\\s?suspensiva\\s?") %>%
      str_remove("(?i)\\s?regencia\\s?de\\s?critica\\s?.*")
    `DT INICIO ROTINA ATRASO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?apf")) %>%
      str_remove("(?i).*dt\\s?inicio\\s?rotina\\s?atraso\\s?obra\\s?") %>%
      str_remove("(?i)\\s?prazo\\s?de\\s?obra\\s?atual\\s?.*")
    `APOLICE SEGURO SGC` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgc")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgc\\s?") %>%
      str_remove("(?i)\\s?prazo\\s?de\\s?obra\\s?original\\s?.*")
    `APOLICE SEGURO SRE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sre")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sre\\s?") %>%
      str_remove("(?i)\\s?percentual\\s?minimo\\s?de\\s?obra\\s?.*")
    `APOLICE SEGURO SGP` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgp")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgp\\s?") %>%
      str_remove("(?i)\\s?adiantamento/defasagem\\s?obra\\s?.*")
    `APOLICE SEGURO SGT` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgt")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgt\\s?") %>%
      str_remove("(?i)\\s?tipo\\s?rotina\\s?.*")
    `QUANTIDADE UNID FINANCIADAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?unidades")) %>%
      str_remove("(?i).*quantidade\\s?unid\\s?financiadas\\s?") %>%
      str_remove("(?i)\\s?quantidade\\s?unid\\s?comercializadas\\s?.*")
    `TOTAL DE FINANCIAMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i).*total de financiamento\\s?") %>%
      str_remove("(?i)\\s?desp leg terr financ\\s?.*")
    `TOTAL DE FGTS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*total de fgts\\s?") %>%
      str_remove("(?i)\\s?desp leg terr fgts\\s?.*")
    `TOTAL R. PROPRIO MUTUARIO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*total r proprio mutuario\\s?") %>%
      str_remove("(?i)\\s?desp leg terr r\\. proprio\\s?.*")
    `PERC OBRA EXECUTADA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*perc obra executada\\s?") %>%
      str_remove("(?i)\\s?vr financ outro agente\\s?.*")
    `DATA DE ASSINATURA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*data de assinatura\\s?") %>%
      str_remove("(?i)\\s?vr terr outro agente\\s?.*")
    `DATA INICIO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*data inicio obra\\s?") %>%
      str_remove("(?i)\\s?vr aporte construtora\\s?.*")
    `DATA TERMINO OBRA ORIGINAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra original\\s?") %>%
      str_remove("(?i)\\s?vr financiamento \\(pj\\)\\s?.*")
    `DATA TERMINO OBRA ATUAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra atual\\s?") %>%
      str_remove("(?i)\\s?garantia termino obra\\s?.*")
    `SUBSIDIO CONVENIOS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*subsidio convenios\\s?") %>%
      str_remove("(?i)\\s?custo do terreno\\s?.*")
    `REDUCAO MAX GERAL (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max geral\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr min garantia hip\\s?\\(130\\%\\)\\s?.*")
    `REDUCAO MAX ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max etapa\\s?\\(pj\\)\\s?")
    `AMORT. RECOMP. ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao etapa \\(pj\\)")) %>%
      str_remove("(?i).*amort\\.\\s?recomp\\. etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?recomp\\.?\\s?s.*")
    `PERC ANTEC (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec\\s?\\(pj\\)\\s?")) %>%
      str_remove("(?i).*perc antec\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr total antec \\(pj\\)\\s?.*")
    # Terceira coluna
    `ORIGEM DE RECURSO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do contrato")) %>%
      str_remove("(?i).*origem de recurso\\s?")
    `LINHA DE FINANCIAMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do pedido")) %>%
      str_remove("(?i).*linha de financiamento\\s?")
    `TIPO DE FINANCIAMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do empreendimento")) %>%
      str_remove("(?i).*tipo de financiamento\\s?")
    `REGENCIA DE CRITICA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de parcelas")) %>%
      str_remove("(?i).*regencia de critica\\s?")
    `PRAZO DE OBRA ATUAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do apf")) %>%
      str_remove("(?i).*prazo de obra atual\\s?")
    `PRAZO DE OBRA ORIGINAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgc")) %>%
      str_remove("(?i).*prazo de obra original\\s?")
    `PERCENTUAL MINIMO DE OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sre")) %>%
      str_remove("(?i).*percentual minimo de obra\\s?")
    `ADIANTAMENTO/DEFASAGEM OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgp")) %>%
      str_remove("(?i).*adiantamento/defasagem obra\\s?\\:\\s?")
    `TIPO ROTINA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgt")) %>%
      str_remove("(?i).*tipo rotina\\s?")
    `QUANTIDADE UNID COMERCIALIZADAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de unidades")) %>%
      str_remove("(?i).*quantidade unid comercializadas\\s?")
    `DESP LEG TERR FINANC` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr custo obra")) %>%
      str_remove("(?i).*desp leg terr financ\\s?")
    `DESP LEG TERR FGTS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*desp leg terr fgts\\s?")
    `DESP LEG TERR R. PROPRIO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*desp leg terr r\\.\\s?proprio\\s?")
    `VR FINANC OUTRO AGENTE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*vr financ outro agente\\s?")
    `VR TERR OUTRO AGENTE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*vr terr outro agente\\s?")
    `VR APORTE CONSTRUTORA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*vr aporte construtora\\s?")
    `VR FINANCIAMENTO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*vr financiamento \\(pj\\)\\s?")
    `GARANTIA TERMINO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*garantia termino obra\\s?")
    `CUSTO DO TERRENO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*custo do terreno\\s?")
    `VR MIN GARANTIA HIP (130%)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo lib\\. geral \\(pj\\)")) %>%
      str_remove("(?i).*vr min garantia hip\\s?\\(130%\\)\\s?")
    `RECOMP. S/ REG ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*recomp\\.\\s?s/\\s?reg\\s?etapa\\s?\\(pj\\)\\s?")
    `VR TOTAL ANTEC (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec \\(pj\\)")) %>%
      str_remove("(?i).*vr total antec \\(pj\\)\\s?")
    # Consolidando todas as variáveis em uma tabela
    dados.cef.dcd.resumo_t <-
      tibble(
        # Primeira coluna
        `NUMERO DO CONTRATO` = "NUMERO DO CONTRATO",
        `NUMERO DO PEDIDO` = "NUMERO DO PEDIDO",
        `NUMERO DO EMPREENDIMENTO` = "NUMERO DO EMPREENDIMENTO",
        `QUANTIDADE DE PARCELAS` = "QUANTIDADE DE PARCELAS",
        `NUMERO DO APF` = "NUMERO DO APF",
        `CODIGO SEGURADORA SGC` = "CODIGO SEGURADORA SGC",
        `CODIGO SEGURADORA SRE` = "CODIGO SEGURADORA SRE",
        `CODIGO SEGURADORA SGP` = "CODIGO SEGURADORA SGP",
        `CODIGO SEGURADORA SGT` = "CODIGO SEGURADORA SGT",
        `QUANTIDADE DE UNIDADES` = "QUANTIDADE DE UNIDADES",
        `VR CUSTO OBRA` = "VR CUSTO OBRA",
        `ORCAMENTO COMPRA/VENDA` = "ORCAMENTO COMPRA/VENDA",
        `VR COMPRA/VENDA UNIDADE` = "VR COMPRA/VENDA UNIDADE",
        `VR COMPRA/VENDA TERRENO` = "VR COMPRA/VENDA TERRENO",
        `SALDO MUTUARIO (PF)` = "SALDO MUTUARIO (PF)",
        `SALDO APORTE CONSTRUTORA` = "SALDO APORTE CONSTRUTORA",
        `SALDO MUTUARIO (PJ)` = "SALDO MUTUARIO (PJ)",
        `SALDO DEVEDOR (PJ)` = "SALDO DEVEDOR (PJ)",
        `SUBSIDIO RES. 460` = "SUBSIDIO RES. 460",
        `TOTAL SUPLEMENTACAO (PJ)` = "TOTAL SUPLEMENTACAO (PJ)",
        `MAXIMO LIB. GERAL (PJ)` = "MAXIMO LIB. GERAL (PJ)",
        `MAXIMO LIB. ETAPA (PJ)` = "MAXIMO LIB. ETAPA (PJ)",
        `RECOMPOSICAO ETAPA (PJ)` = "RECOMPOSICAO ETAPA (PJ)",
        # Segunda coluna
        `EMPREENDIMENTO` = "EMPREENDIMENTO",
        `CODIGO DO PEDIDO` = "CODIGO DO PEDIDO",
        `SITUACAO DO PEDIDO` = "SITUACAO DO PEDIDO",
        `DATA TERMINO SUSPENSIVA` = "DATA TERMINO SUSPENSIVA",
        `DT INICIO ROTINA ATRASO OBRA` = "DT INICIO ROTINA ATRASO OBRA",
        `APOLICE SEGURO SGC` = "APOLICE SEGURO SGC",
        `APOLICE SEGURO SRE` = "APOLICE SEGURO SRE",
        `APOLICE SEGURO SGP` = "APOLICE SEGURO SGP",
        `APOLICE SEGURO SGT` = "APOLICE SEGURO SGT",
        `QUANTIDADE UNID FINANCIADAS` = "QUANTIDADE UNID FINANCIADAS",
        `TOTAL DE FINANCIAMENTO` = "TOTAL DE FINANCIAMENTO",
        `TOTAL DE FGTS` = "TOTAL DE FGTS",
        `TOTAL R. PROPRIO MUTUARIO` = "TOTAL R. PROPRIO MUTUARIO",
        `PERC OBRA EXECUTADA` = "PERC OBRA EXECUTADA",
        `DATA DE ASSINATURA` = "DATA DE ASSINATURA",
        `DATA INICIO OBRA` = "DATA INICIO OBRA",
        `DATA TERMINO OBRA ORIGINAL` = "DATA TERMINO OBRA ORIGINAL",
        `DATA TERMINO OBRA ATUAL` = "DATA TERMINO OBRA ATUAL",
        `SUBSIDIO CONVENIOS` = "SUBSIDIO CONVENIOS",
        `REDUCAO MAX GERAL (PJ)` = "REDUCAO MAX GERAL (PJ)",
        `REDUCAO MAX ETAPA (PJ)` = "REDUCAO MAX ETAPA (PJ)",
        `AMORT. RECOMP. ETAPA (PJ)` = "AMORT. RECOMP. ETAPA (PJ)",
        `PERC ANTEC (PJ)` = "PERC ANTEC (PJ)",
        # Terceira coluna
        `ORIGEM DE RECURSO` = "ORIGEM DE RECURSO",
        `LINHA DE FINANCIAMENTO` = "LINHA DE FINANCIAMENTO",
        `TIPO DE FINANCIAMENTO` = "TIPO DE FINANCIAMENTO",
        `REGENCIA DE CRITICA` = "REGENCIA DE CRITICA",
        `PRAZO DE OBRA ATUAL` = "PRAZO DE OBRA ATUAL",
        `PRAZO DE OBRA ORIGINAL` = "PRAZO DE OBRA ORIGINAL",
        `PERCENTUAL MINIMO DE OBRA` = "PERCENTUAL MINIMO DE OBRA",
        `ADIANTAMENTO/DEFASAGEM OBRA` = "ADIANTAMENTO/DEFASAGEM OBRA",
        `TIPO ROTINA` = "TIPO ROTINA",
        `QUANTIDADE UNID COMERCIALIZADAS` = "QUANTIDADE UNID COMERCIALIZADAS",
        `DESP LEG TERR FINANC` = "DESP LEG TERR FINANC",
        `DESP LEG TERR FGTS` = "DESP LEG TERR FGTS",
        `DESP LEG TERR R. PROPRIO` = "DESP LEG TERR R. PROPRIO",
        `VR FINANC OUTRO AGENTE` = "VR FINANC OUTRO AGENTE",
        `VR TERR OUTRO AGENTE` = "VR TERR OUTRO AGENTE",
        `VR APORTE CONSTRUTORA` = "VR APORTE CONSTRUTORA",
        `VR FINANCIAMENTO (PJ)` = "VR FINANCIAMENTO (PJ)",
        `GARANTIA TERMINO OBRA` = "GARANTIA TERMINO OBRA",
        `CUSTO DO TERRENO` = "CUSTO DO TERRENO",
        `VR MIN GARANTIA HIP (130%)` = "VR MIN GARANTIA HIP (130%)",
        `RECOMP. S/ REG ETAPA (PJ)` = "RECOMP. S/ REG ETAPA (PJ)",
        `VR TOTAL ANTEC (PJ)` = "VR TOTAL ANTEC (PJ)"
      )
    return(dados.cef.dcd.resumo_t)
  }

# Teste -------------------------------------------------------------------
f_caminho.arquivo_c <- "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - CIWEB/1. UP Vila Sonia/04.04/DCD/20250404_082919_722_PP_177770014920_DEMONST_CRONOGRAMA.pdf"
# caminhos.dcds_c <-
#   list.files(
#     file.path(
#       dirname(dirname(here())),
#       "Relatórios - Documentos", "Relatorios - CIWEB"
#     ),
#     full.names = TRUE, recursive = TRUE
#   ) %>%
#   keep(~ str_detect(.x, "_DEMONST_CRONOGRAMA.pdf"))
# f_caminho.arquivo_c <- caminhos.dcds_c[1]
## shell.exec(f_caminho.arquivo_c)

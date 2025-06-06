#' @title Extração dos dados de um relatório DCD da CEF
#'
#' @description
#' A função **e_cef_dcd_resumo()** extrai e organiza dados de um relatório
#' DCD (Demonstração de Cronograma de Desembolso) em PDF da CEF,
#' consolidando informações e retornando um tibble com os valores
#' encontrados (custo de obra, saldos, suplementações e detalhes do contrato).
#'
#' @param f_caminho.arquivo_c Caminho para o arquivo PDF do DCD.
#'
#' @details
#' São utilizadas funções da \pkg{pdftools} para leitura do PDF, além de
#' expressões regulares para filtrar linhas com informações específicas.
#'
#' @return Um tibble reunindo as variáveis extraídas (números,
#' datas de assinatura, codificações internas, etc.).
#'
#' @examples
#' \dontrun{
#' dcd_res <- e_cef_dcd_resumo("C:/caminho/para/o/arq.pdf")
#' }
#'
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_remove_all str_starts str_which str_detect
#' @importFrom stringr str_split str_squish
#' @export
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
      str_which(
        "(?i)^base\\s?de\\s?calculo\\s?para\\s?geracao\\s?do\\s?cronograma"
      ) %>%
      nth(1) - 1
    linhas.resumo_c <-
      linhas_c[1:indice.fim_i]
    # Primeira coluna
    numero.contrato <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.factor()
    numero.pedido <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i)numero\\s?do\\s?pedido\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    numero.empreendimento <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i)numero\\s?do\\s?empreendimento\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.factor()
    quantidade.parcelas <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i)quantidade\\s?de\\s?parcelas\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.integer()
    numero.apf <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?apf")) %>%
      str_remove("(?i)numero\\s?do\\s?apf\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    codigo.seguradora.sgc <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgc")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgc\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    codigo.seguradora.sre <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sre")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sre\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    codigo.seguradora.sgp <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgp")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgp\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    codigo.seguradora.sgt <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgt")) %>%
      str_remove("(?i)codigo\\s?seguradora\\s?sgt\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    quantidade.unidades <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?unidades")) %>%
      str_remove("(?i)quantidade\\s?de\\s?unidades\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.integer()
    valor.custo.obra <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i)vr\\s?custo\\s?obra\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `orcamento.compra/venda` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento\\s?compra/venda")) %>%
      str_remove("(?i)orcamento\\s?compra/venda\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `valor.compra/venda.unidade` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?unidade")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?unidade\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `valor.compra/venda.terreno` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?terreno")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?terreno\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    saldo.mutuario.pf <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pf\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pf\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    saldo.aporte.construtora <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?aporte\\s?construtora")) %>%
      str_remove("(?i)saldo\\s?aporte\\s?construtora\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    saldo.mutuario.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    saldo.devedor.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?devedor\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?devedor\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    subsidio.res.460 <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio\\s?res.\\s?460")) %>%
      str_remove("(?i)subsidio\\s?res.\\s?460\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    total.suplementacao.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)total\\s?suplementacao\\s?\\(pj\\)")) %>%
      str_remove("(?i)total\\s?suplementacao\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    maximo.lib.geral.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    maximo.lib.etapa.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    recomposicao.etapa.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)recomposicao\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Segunda coluna
    empreendimento <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove(".*\\d{12}\\s?") %>%
      str_remove("(?i)\\s?origem\\s?de\\s?recurso.*") %>%
      as.factor()
    codigo.pedido <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i).*codigo\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?linha\\s?de\\s?financiamento\\s?.*")
    situacao.pedido <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i).*situacao\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?tipo\\s?de\\s?financiamento\\s?.*") %>%
      as.factor()
    data.termino.suspensiva <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i).*data\\s?termino\\s?suspensiva\\s?") %>%
      str_remove("(?i)\\s?regencia\\s?de\\s?critica\\s?.*") %>%
      dmy()
    data.inicio.rotina.atraso.obra <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?apf")) %>%
      str_remove("(?i).*dt\\s?inicio\\s?rotina\\s?atraso\\s?obra\\s?") %>%
      str_remove("(?i)\\s?prazo\\s?de\\s?obra\\s?atual\\s?.*") %>%
      dmy()
    apolice.seguro.sgc <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgc")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgc\\s?") %>%
      str_remove("(?i)\\s?prazo\\s?de\\s?obra\\s?original\\s?.*")
    apolice.seguro.sre <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sre")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sre\\s?") %>%
      str_remove("(?i)\\s?percentual\\s?minimo\\s?de\\s?obra\\s?.*")
    apolice.seguro.sgp <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgp")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgp\\s?") %>%
      str_remove("(?i)\\s?adiantamento/defasagem\\s?obra\\s?.*")
    apolice.seguro.sgt <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo\\s?seguradora\\s?sgt")) %>%
      str_remove("(?i).*apolice\\s?seguro\\s?sgt\\s?") %>%
      str_remove("(?i)\\s?tipo\\s?rotina\\s?.*")
    quantidade.unidades.financiadas <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?unidades")) %>%
      str_remove("(?i).*quantidade\\s?unid\\s?financiadas\\s?") %>%
      str_remove("(?i)\\s?quantidade\\s?unid\\s?comercializadas\\s?.*") %>%
      as.integer()
    total.financiamento <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i).*total de financiamento\\s?") %>%
      str_remove("(?i)\\s?desp leg terr financ\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    total.fgts <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*total de fgts\\s?") %>%
      str_remove("(?i)\\s?desp leg terr fgts\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    total.recurso.proprio.mutuario <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*total r proprio mutuario\\s?") %>%
      str_remove("(?i)\\s?desp leg terr r\\. proprio\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    percentual.obra.executada <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*perc obra executada\\s?") %>%
      str_remove("(?i)\\s?vr financ outro agente\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    data.assinatura <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*data de assinatura\\s?") %>%
      str_remove("(?i)\\s?vr terr outro agente\\s?.*") %>%
      dmy()
    data.inicio.obra <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*data inicio obra\\s?") %>%
      str_remove("(?i)\\s?vr aporte construtora\\s?.*") %>%
      dmy()
    data.termino.obra.original <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra original\\s?") %>%
      str_remove("(?i)\\s?vr financiamento \\(pj\\)\\s?.*") %>%
      dmy()
    data.termino.obra.atual <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra atual\\s?") %>%
      str_remove("(?i)\\s?garantia termino obra\\s?.*") %>%
      dmy()
    subsidio.convenios <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*subsidio convenios\\s?") %>%
      str_remove("(?i)\\s?custo do terreno\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    reducao.max.geral.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max geral\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr min garantia hip\\s?\\(130\\%\\)\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    reducao.max.etapa.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max etapa\\s?\\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    amort.recomp.etapa.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao etapa \\(pj\\)")) %>%
      str_remove("(?i).*amort\\.\\s?recomp\\. etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?recomp\\.?\\s?s.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    percentual.antec.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec\\s?\\(pj\\)\\s?")) %>%
      str_remove("(?i).*perc antec\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr total antec \\(pj\\)\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Terceira coluna
    origem.recurso <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do contrato")) %>%
      str_remove("(?i).*origem de recurso\\s?")
    linha.financiamento <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do pedido")) %>%
      str_remove("(?i).*linha de financiamento\\s?")
    tipo.financiamento <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do empreendimento")) %>%
      str_remove("(?i).*tipo de financiamento\\s?")
    regencia.critica <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de parcelas")) %>%
      str_remove("(?i).*regencia de critica\\s?")
    prazo.obra.atual <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do apf")) %>%
      str_remove("(?i).*prazo de obra atual\\s?") %>%
      as.integer()
    prazo.obra.original <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgc")) %>%
      str_remove("(?i).*prazo de obra original\\s?") %>%
      as.integer()
    percentual.minimo.obra <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sre")) %>%
      str_remove("(?i).*percentual minimo de obra\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `adiantamento/defasagem.obra` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgp")) %>%
      str_remove("(?i).*adiantamento/defasagem obra\\s?\\:\\s?") %>%
      str_remove_all(" |\\%") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    tipo.rotina <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgt")) %>%
      str_remove("(?i).*tipo rotina\\s?") %>%
      as.factor()
    quantidade.unidades.comercializadas <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de unidades")) %>%
      str_remove("(?i).*quantidade unid comercializadas\\s?") %>%
      as.integer()
    desp.leg.terr.financ <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr custo obra")) %>%
      str_remove("(?i).*desp leg terr financ\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    desp.leg.terr.fgts <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*desp leg terr fgts\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    desp.leg.terr.recurso.proprio <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*desp leg terr r\\.\\s?proprio\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    valor.financ.outro.agente <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*vr financ outro agente\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    valor.terr.outro.agente <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*vr terr outro agente\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    valor.aporte.construtora <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*vr aporte construtora\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    valor.financiamento.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*vr financiamento \\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    garantia.termino.obra <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*garantia termino obra\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    custo.terreno <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*custo do terreno\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `valor.minimo.garantia.hip.130%` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo lib\\. geral \\(pj\\)")) %>%
      str_remove("(?i).*vr min garantia hip\\s?\\(130%\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `recomp.s/.reg.etapa.pj` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*recomp\\.\\s?s/\\s?reg\\s?etapa\\s?\\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    valor.total.antec.pj <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec \\(pj\\)")) %>%
      str_remove("(?i).*vr total antec \\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Consolidando todas as variáveis em uma tabela
    dados.cef.dcd.resumo_t <-
      tibble(
        # Primeira coluna
        numero.contrato = numero.contrato,
        numero.pedido = numero.pedido,
        numero.empreendimento = numero.empreendimento,
        quantidade.parcelas = quantidade.parcelas,
        numero.apf = numero.apf,
        codigo.seguradora.sgc = codigo.seguradora.sgc,
        codigo.seguradora.sre = codigo.seguradora.sre,
        codigo.seguradora.sgp = codigo.seguradora.sgp,
        codigo.seguradora.sgt = codigo.seguradora.sgt,
        quantidade.unidades = quantidade.unidades,
        valor.custo.obra = valor.custo.obra,
        `orcamento.compra/venda` = `orcamento.compra/venda`,
        `valor.compra/venda.unidade` = `valor.compra/venda.unidade`,
        `valor.compra/venda.terreno` = `valor.compra/venda.terreno`,
        saldo.mutuario.pf = saldo.mutuario.pf,
        saldo.aporte.construtora = saldo.aporte.construtora,
        saldo.mutuario.pj = saldo.mutuario.pj,
        saldo.devedor.pj = saldo.devedor.pj,
        subsidio.res.460 = subsidio.res.460,
        total.suplementacao.pj = total.suplementacao.pj,
        maximo.lib.geral.pj = maximo.lib.geral.pj,
        maximo.lib.etapa.pj = maximo.lib.etapa.pj,
        recomposicao.etapa.pj = recomposicao.etapa.pj,
        # Segunda coluna
        empreendimento = empreendimento,
        codigo.pedido = codigo.pedido,
        situacao.pedido = situacao.pedido,
        data.termino.suspensiva = data.termino.suspensiva,
        data.inicio.rotina.atraso.obra = data.inicio.rotina.atraso.obra,
        apolice.seguro.sgc = apolice.seguro.sgc,
        apolice.seguro.sre = apolice.seguro.sre,
        apolice.seguro.sgp = apolice.seguro.sgp,
        apolice.seguro.sgt = apolice.seguro.sgt,
        quantidade.unidades.financiadas = quantidade.unidades.financiadas,
        total.financiamento = total.financiamento,
        total.fgts = total.fgts,
        total.recurso.proprio.mutuario = total.recurso.proprio.mutuario,
        percentual.obra.executada = percentual.obra.executada,
        data.assinatura = data.assinatura,
        data.inicio.obra = data.inicio.obra,
        data.termino.obra.original = data.termino.obra.original,
        data.termino.obra.atual = data.termino.obra.atual,
        subsidio.convenios = subsidio.convenios,
        reducao.max.geral.pj = reducao.max.geral.pj,
        reducao.max.etapa.pj = reducao.max.etapa.pj,
        amort.recomp.etapa.pj = amort.recomp.etapa.pj,
        percentual.antec.pj = percentual.antec.pj,
        # Terceira coluna
        origem.recurso = origem.recurso,
        linha.financiamento = linha.financiamento,
        tipo.financiamento = tipo.financiamento,
        regencia.critica = regencia.critica,
        prazo.obra.atual = prazo.obra.atual,
        prazo.obra.original = prazo.obra.original,
        percentual.minimo.obra = percentual.minimo.obra,
        `adiantamento/defasagem.obra` = `adiantamento/defasagem.obra`,
        tipo.rotina = tipo.rotina,
        quantidade.unidades.comercializadas =
          quantidade.unidades.comercializadas,
        desp.leg.terr.financ = desp.leg.terr.financ,
        desp.leg.terr.fgts = desp.leg.terr.fgts,
        desp.leg.terr.recurso.proprio = desp.leg.terr.recurso.proprio,
        valor.financ.outro.agente = valor.financ.outro.agente,
        valor.terr.outro.agente = valor.terr.outro.agente,
        valor.aporte.construtora = valor.aporte.construtora,
        valor.financiamento.pj = valor.financiamento.pj,
        garantia.termino.obra = garantia.termino.obra,
        custo.terreno = custo.terreno,
        `valor.minimo.garantia.hip.130%` = `valor.minimo.garantia.hip.130%`,
        `recomp.s/.reg.etapa.pj` = `recomp.s/.reg.etapa.pj`,
        valor.total.antec.pj = valor.total.antec.pj,
        # Metadados
        data.consulta = linhas_c[2] %>%
          str_extract("(?<=\\s)\\d{2}/\\d{2}/\\d{2}\\s?\\d{2}:\\d{2}") %>%
          dmy_hm(),
        arquivo = f_caminho.arquivo_c
      )
    return(dados.cef.dcd.resumo_t)
  }

# Teste -------------------------------------------------------------------
# f_caminho.arquivo_c <- "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - CIWEB/1. UP Vila Sonia/04.04/DCD/20250404_082919_722_PP_177770014920_DEMONST_CRONOGRAMA.pdf"
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

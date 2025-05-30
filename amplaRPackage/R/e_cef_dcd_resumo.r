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
    `NUMERO DO CONTRATO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.factor()
    `NUMERO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i)numero\\s?do\\s?pedido\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i)numero\\s?do\\s?empreendimento\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.factor()
    `QUANTIDADE DE PARCELAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i)quantidade\\s?de\\s?parcelas\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      as.integer()
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
      str_remove("\\s?[A-Za-z].*$") %>%
      as.integer()
    `VR CUSTO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i)vr\\s?custo\\s?obra\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `ORCAMENTO COMPRA/VENDA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento\\s?compra/venda")) %>%
      str_remove("(?i)orcamento\\s?compra/venda\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR COMPRA/VENDA UNIDADE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?unidade")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?unidade\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR COMPRA/VENDA TERRENO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?compra/venda\\s?terreno")) %>%
      str_remove("(?i)vr\\s?compra/venda\\s?terreno\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `SALDO MUTUARIO (PF)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pf\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pf\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `SALDO APORTE CONSTRUTORA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?aporte\\s?construtora")) %>%
      str_remove("(?i)saldo\\s?aporte\\s?construtora\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `SALDO MUTUARIO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?mutuario\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?mutuario\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `SALDO DEVEDOR (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo\\s?devedor\\s?\\(pj\\)")) %>%
      str_remove("(?i)saldo\\s?devedor\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `SUBSIDIO RES. 460` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio\\s?res.\\s?460")) %>%
      str_remove("(?i)subsidio\\s?res.\\s?460\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `TOTAL SUPLEMENTACAO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)total\\s?suplementacao\\s?\\(pj\\)")) %>%
      str_remove("(?i)total\\s?suplementacao\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `MAXIMO LIB. GERAL (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?geral\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `MAXIMO LIB. ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)maximo\\s?lib.\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `RECOMPOSICAO ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i)recomposicao\\s?etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Segunda coluna
    `EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?contrato")) %>%
      str_remove("(?i)numero\\s?do\\s?contrato\\s?") %>%
      str_remove(".*\\d{12}\\s?") %>%
      str_remove("(?i)\\s?origem\\s?de\\s?recurso.*") %>%
      as.factor()
    `CODIGO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?pedido")) %>%
      str_remove("(?i).*codigo\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?linha\\s?de\\s?financiamento\\s?.*")
    `SITUACAO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?empreendimento")) %>%
      str_remove("(?i).*situacao\\s?do\\s?pedido\\s?") %>%
      str_remove("(?i)\\s?tipo\\s?de\\s?financiamento\\s?.*") %>%
      as.factor()
    `DATA TERMINO SUSPENSIVA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade\\s?de\\s?parcelas")) %>%
      str_remove("(?i).*data\\s?termino\\s?suspensiva\\s?") %>%
      str_remove("(?i)\\s?regencia\\s?de\\s?critica\\s?.*") %>%
      dmy()
    `DT INICIO ROTINA ATRASO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero\\s?do\\s?apf")) %>%
      str_remove("(?i).*dt\\s?inicio\\s?rotina\\s?atraso\\s?obra\\s?") %>%
      str_remove("(?i)\\s?prazo\\s?de\\s?obra\\s?atual\\s?.*") %>%
      dmy()
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
      str_remove("(?i)\\s?quantidade\\s?unid\\s?comercializadas\\s?.*") %>%
      as.integer()
    `TOTAL DE FINANCIAMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr\\s?custo\\s?obra")) %>%
      str_remove("(?i).*total de financiamento\\s?") %>%
      str_remove("(?i)\\s?desp leg terr financ\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `TOTAL DE FGTS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*total de fgts\\s?") %>%
      str_remove("(?i)\\s?desp leg terr fgts\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `TOTAL R. PROPRIO MUTUARIO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*total r proprio mutuario\\s?") %>%
      str_remove("(?i)\\s?desp leg terr r\\. proprio\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `PERC OBRA EXECUTADA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*perc obra executada\\s?") %>%
      str_remove("(?i)\\s?vr financ outro agente\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `DATA DE ASSINATURA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*data de assinatura\\s?") %>%
      str_remove("(?i)\\s?vr terr outro agente\\s?.*") %>%
      dmy()
    `DATA INICIO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*data inicio obra\\s?") %>%
      str_remove("(?i)\\s?vr aporte construtora\\s?.*") %>%
      dmy()
    `DATA TERMINO OBRA ORIGINAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra original\\s?") %>%
      str_remove("(?i)\\s?vr financiamento \\(pj\\)\\s?.*") %>%
      dmy()
    `DATA TERMINO OBRA ATUAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*data termino obra atual\\s?") %>%
      str_remove("(?i)\\s?garantia termino obra\\s?.*") %>%
      dmy()
    `SUBSIDIO CONVENIOS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*subsidio convenios\\s?") %>%
      str_remove("(?i)\\s?custo do terreno\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `REDUCAO MAX GERAL (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?geral\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max geral\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr min garantia hip\\s?\\(130\\%\\)\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `REDUCAO MAX ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo\\s?lib\\.\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*reducao max etapa\\s?\\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `AMORT. RECOMP. ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao etapa \\(pj\\)")) %>%
      str_remove("(?i).*amort\\.\\s?recomp\\. etapa\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?recomp\\.?\\s?s.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `PERC ANTEC (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec\\s?\\(pj\\)\\s?")) %>%
      str_remove("(?i).*perc antec\\s?\\(pj\\)\\s?") %>%
      str_remove("(?i)\\s?vr total antec \\(pj\\)\\s?.*") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
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
      str_remove("(?i).*prazo de obra atual\\s?") %>%
      as.integer()
    `PRAZO DE OBRA ORIGINAL` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgc")) %>%
      str_remove("(?i).*prazo de obra original\\s?") %>%
      as.integer()
    `PERCENTUAL MINIMO DE OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sre")) %>%
      str_remove("(?i).*percentual minimo de obra\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `ADIANTAMENTO/DEFASAGEM OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgp")) %>%
      str_remove("(?i).*adiantamento/defasagem obra\\s?\\:\\s?") %>%
      str_remove_all(" |\\%") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `TIPO ROTINA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgt")) %>%
      str_remove("(?i).*tipo rotina\\s?") %>%
      as.factor()
    `QUANTIDADE UNID COMERCIALIZADAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de unidades")) %>%
      str_remove("(?i).*quantidade unid comercializadas\\s?") %>%
      as.integer()
    `DESP LEG TERR FINANC` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr custo obra")) %>%
      str_remove("(?i).*desp leg terr financ\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `DESP LEG TERR FGTS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i).*desp leg terr fgts\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `DESP LEG TERR R. PROPRIO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i).*desp leg terr r\\.\\s?proprio\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR FINANC OUTRO AGENTE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i).*vr financ outro agente\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR TERR OUTRO AGENTE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i).*vr terr outro agente\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR APORTE CONSTRUTORA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i).*vr aporte construtora\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR FINANCIAMENTO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i).*vr financiamento \\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `GARANTIA TERMINO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i).*garantia termino obra\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `CUSTO DO TERRENO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i).*custo do terreno\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR MIN GARANTIA HIP (130%)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo lib\\. geral \\(pj\\)")) %>%
      str_remove("(?i).*vr min garantia hip\\s?\\(130%\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `RECOMP. S/ REG ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao\\s?etapa\\s?\\(pj\\)")) %>%
      str_remove("(?i).*recomp\\.\\s?s/\\s?reg\\s?etapa\\s?\\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    `VR TOTAL ANTEC (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)perc antec \\(pj\\)")) %>%
      str_remove("(?i).*vr total antec \\(pj\\)\\s?") %>%
      parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
    # Consolidando todas as variáveis em uma tabela
    dados.cef.dcd.resumo_t <-
      tibble(
        # Primeira coluna
        `NUMERO DO CONTRATO` = `NUMERO DO CONTRATO`,
        `NUMERO DO PEDIDO` = `NUMERO DO PEDIDO`,
        `NUMERO DO EMPREENDIMENTO` = `NUMERO DO EMPREENDIMENTO`,
        `QUANTIDADE DE PARCELAS` = `QUANTIDADE DE PARCELAS`,
        `NUMERO DO APF` = `NUMERO DO APF`,
        `CODIGO SEGURADORA SGC` = `CODIGO SEGURADORA SGC`,
        `CODIGO SEGURADORA SRE` = `CODIGO SEGURADORA SRE`,
        `CODIGO SEGURADORA SGP` = `CODIGO SEGURADORA SGP`,
        `CODIGO SEGURADORA SGT` = `CODIGO SEGURADORA SGT`,
        `QUANTIDADE DE UNIDADES` = `QUANTIDADE DE UNIDADES`,
        `VR CUSTO OBRA` = `VR CUSTO OBRA`,
        `ORCAMENTO COMPRA/VENDA` = `ORCAMENTO COMPRA/VENDA`,
        `VR COMPRA/VENDA UNIDADE` = `VR COMPRA/VENDA UNIDADE`,
        `VR COMPRA/VENDA TERRENO` = `VR COMPRA/VENDA TERRENO`,
        `SALDO MUTUARIO (PF)` = `SALDO MUTUARIO (PF)`,
        `SALDO APORTE CONSTRUTORA` = `SALDO APORTE CONSTRUTORA`,
        `SALDO MUTUARIO (PJ)` = `SALDO MUTUARIO (PJ)`,
        `SALDO DEVEDOR (PJ)` = `SALDO DEVEDOR (PJ)`,
        `SUBSIDIO RES. 460` = `SUBSIDIO RES. 460`,
        `TOTAL SUPLEMENTACAO (PJ)` = `TOTAL SUPLEMENTACAO (PJ)`,
        `MAXIMO LIB. GERAL (PJ)` = `MAXIMO LIB. GERAL (PJ)`,
        `MAXIMO LIB. ETAPA (PJ)` = `MAXIMO LIB. ETAPA (PJ)`,
        `RECOMPOSICAO ETAPA (PJ)` = `RECOMPOSICAO ETAPA (PJ)`,
        # Segunda coluna
        `EMPREENDIMENTO` = `EMPREENDIMENTO`,
        `CODIGO DO PEDIDO` = `CODIGO DO PEDIDO`,
        `SITUACAO DO PEDIDO` = `SITUACAO DO PEDIDO`,
        `DATA TERMINO SUSPENSIVA` = `DATA TERMINO SUSPENSIVA`,
        `DT INICIO ROTINA ATRASO OBRA` = `DT INICIO ROTINA ATRASO OBRA`,
        `APOLICE SEGURO SGC` = `APOLICE SEGURO SGC`,
        `APOLICE SEGURO SRE` = `APOLICE SEGURO SRE`,
        `APOLICE SEGURO SGP` = `APOLICE SEGURO SGP`,
        `APOLICE SEGURO SGT` = `APOLICE SEGURO SGT`,
        `QUANTIDADE UNID FINANCIADAS` = `QUANTIDADE UNID FINANCIADAS`,
        `TOTAL DE FINANCIAMENTO` = `TOTAL DE FINANCIAMENTO`,
        `TOTAL DE FGTS` = `TOTAL DE FGTS`,
        `TOTAL R. PROPRIO MUTUARIO` = `TOTAL R. PROPRIO MUTUARIO`,
        `PERC OBRA EXECUTADA` = `PERC OBRA EXECUTADA`,
        `DATA DE ASSINATURA` = `DATA DE ASSINATURA`,
        `DATA INICIO OBRA` = `DATA INICIO OBRA`,
        `DATA TERMINO OBRA ORIGINAL` = `DATA TERMINO OBRA ORIGINAL`,
        `DATA TERMINO OBRA ATUAL` = `DATA TERMINO OBRA ATUAL`,
        `SUBSIDIO CONVENIOS` = `SUBSIDIO CONVENIOS`,
        `REDUCAO MAX GERAL (PJ)` = `REDUCAO MAX GERAL (PJ)`,
        `REDUCAO MAX ETAPA (PJ)` = `REDUCAO MAX ETAPA (PJ)`,
        `AMORT. RECOMP. ETAPA (PJ)` = `AMORT. RECOMP. ETAPA (PJ)`,
        `PERC ANTEC (PJ)` = `PERC ANTEC (PJ)`,
        # Terceira coluna
        `ORIGEM DE RECURSO` = `ORIGEM DE RECURSO`,
        `LINHA DE FINANCIAMENTO` = `LINHA DE FINANCIAMENTO`,
        `TIPO DE FINANCIAMENTO` = `TIPO DE FINANCIAMENTO`,
        `REGENCIA DE CRITICA` = `REGENCIA DE CRITICA`,
        `PRAZO DE OBRA ATUAL` = `PRAZO DE OBRA ATUAL`,
        `PRAZO DE OBRA ORIGINAL` = `PRAZO DE OBRA ORIGINAL`,
        `PERCENTUAL MINIMO DE OBRA` = `PERCENTUAL MINIMO DE OBRA`,
        `ADIANTAMENTO/DEFASAGEM OBRA` = `ADIANTAMENTO/DEFASAGEM OBRA`,
        `TIPO ROTINA` = `TIPO ROTINA`,
        `QUANTIDADE UNID COMERCIALIZADAS` = `QUANTIDADE UNID COMERCIALIZADAS`,
        `DESP LEG TERR FINANC` = `DESP LEG TERR FINANC`,
        `DESP LEG TERR FGTS` = `DESP LEG TERR FGTS`,
        `DESP LEG TERR R. PROPRIO` = `DESP LEG TERR R. PROPRIO`,
        `VR FINANC OUTRO AGENTE` = `VR FINANC OUTRO AGENTE`,
        `VR TERR OUTRO AGENTE` = `VR TERR OUTRO AGENTE`,
        `VR APORTE CONSTRUTORA` = `VR APORTE CONSTRUTORA`,
        `VR FINANCIAMENTO (PJ)` = `VR FINANCIAMENTO (PJ)`,
        `GARANTIA TERMINO OBRA` = `GARANTIA TERMINO OBRA`,
        `CUSTO DO TERRENO` = `CUSTO DO TERRENO`,
        `VR MIN GARANTIA HIP (130%)` = `VR MIN GARANTIA HIP (130%)`,
        `RECOMP. S/ REG ETAPA (PJ)` = `RECOMP. S/ REG ETAPA (PJ)`,
        `VR TOTAL ANTEC (PJ)` = `VR TOTAL ANTEC (PJ)`,
        # Metadados
        `Data de consulta` = linhas_c[2] %>%
          str_extract("(?<=\\s)\\d{2}/\\d{2}/\\d{2}\\s?\\d{2}:\\d{2}") %>%
          dmy_hm(),
        Arquivo = f_caminho.arquivo_c
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

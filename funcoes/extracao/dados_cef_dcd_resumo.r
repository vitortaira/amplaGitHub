# Descrição ---------------------------------------------------------------

### RESUMO ###

# dados_cef_dcd_resumo() extrai os dados de um relatório DCD da CEF em PDF.

### UTILIZAÇÃO ###

# dados_cef_dcd_resumo(
#   f_caminho.arquivo_c
# )

### ARGUMENTOS ###

# f_caminho.arquivo_c: String do caminho de um relatório DCD da CEF em PDF.

# Pacotes -----------------------------------------------------------------

library(pdftools) # Funções para extração de dados em PDF

# Função ------------------------------------------------------------------

# Define a função
dados_cef_dcd_resumo <-
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
      str_which("(?i)^base de calculo para geracao do cronograma") %>%
      nth(1) - 1
    linhas.resumo_c <-
      linhas_c[1:indice.fim_i]
    # Primeira coluna
    `NUMERO DO CONTRATO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do contrato")) %>%
      str_remove("(?i)numero do contrato\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO PEDIDO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do pedido")) %>%
      str_remove("(?i)numero do pedido\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do empreendimento")) %>%
      str_remove("(?i)numero do empreendimento\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `QUANTIDADE DE PARCELAS` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de parcelas")) %>%
      str_remove("(?i)quantidade de parcelas\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `NUMERO DO APF` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do apf")) %>%
      str_remove("(?i)numero do apf\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGC` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgc")) %>%
      str_remove("(?i)codigo seguradora sgc\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SRE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sre")) %>%
      str_remove("(?i)codigo seguradora sre\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGP` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgp")) %>%
      str_remove("(?i)codigo seguradora sgp\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `CODIGO SEGURADORA SGT` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)codigo seguradora sgt")) %>%
      str_remove("(?i)codigo seguradora sgt\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `QUANTIDADE DE UNIDADES` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)quantidade de unidades")) %>%
      str_remove("(?i)quantidade de unidades\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR CUSTO OBRA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr custo obra")) %>%
      str_remove("(?i)vr custo obra\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `ORCAMENTO COMPRA/VENDA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)orcamento compra/venda")) %>%
      str_remove("(?i)orcamento compra/venda\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR COMPRA/VENDA UNIDADE` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda unidade")) %>%
      str_remove("(?i)vr compra/venda unidade\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `VR COMPRA/VENDA TERRENO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)vr compra/venda terreno")) %>%
      str_remove("(?i)vr compra/venda terreno\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO MUTUARIO (PF)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pf\\)")) %>%
      str_remove("(?i)saldo mutuario \\(pf\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO APORTE CONSTRUTORA` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo aporte construtora")) %>%
      str_remove("(?i)saldo aporte construtora\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO MUTUARIO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo mutuario \\(pj\\)")) %>%
      str_remove("(?i)saldo mutuario \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SALDO DEVEDOR (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)saldo devedor \\(pj\\)")) %>%
      str_remove("(?i)saldo devedor \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `SUBSIDIO RES. 460` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)subsidio res. 460")) %>%
      str_remove("(?i)subsidio res. 460\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `TOTAL SUPLEMENTACAO (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)total suplementacao \\(pj\\)")) %>%
      str_remove("(?i)total suplementacao \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `MAXIMO LIB. GERAL (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo lib. geral \\(pj\\)")) %>%
      str_remove("(?i)maximo lib. geral \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `MAXIMO LIB. ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)maximo lib. etapa \\(pj\\)")) %>%
      str_remove("(?i)maximo lib. etapa \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    `RECOMPOSICAO ETAPA (PJ)` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)recomposicao etapa \\(pj\\)")) %>%
      str_remove("(?i)recomposicao etapa \\(pj\\)\\s?") %>%
      str_remove("\\s?[A-Za-z].*$")
    # Segunda coluna
    `EMPREENDIMENTO` <- linhas.resumo_c %>%
      keep(~ str_starts(.x, "(?i)numero do contrato")) %>%
      str_remove("(?i)numero do contrato\\s?")
    `CODIGO DO PEDIDO` <- linhas.resumo_c %>%
      `SITUACAO DO PEDIDO`() <- linhas.resumo_c %>%
      `DATA TERMINO SUSPENSIVA`() <- linhas.resumo_c %>%
      `DT INICIO ROTINA ATRASO OBRA`() <- linhas.resumo_c %>%
      `APOLICE SEGURO SGC`() <- linhas.resumo_c %>%
      `APOLICE SEGURO SRE`() <- linhas.resumo_c %>%
      `APOLICE SEGURO SGP`() <- linhas.resumo_c %>%
      `APOLICE SEGURO SGT`() <- linhas.resumo_c %>%
      `QUANTIDADE UNID FINANCIADAS`() <- linhas.resumo_c %>%
      `TOTAL DE FINANCIAMENTO`() <- linhas.resumo_c %>%
      `TOTAL DE FGTS`() <- linhas.resumo_c %>%
      `TOTAL R. PROPRIO MUTUARIO`() <- linhas.resumo_c %>%
      `PERC OBRA EXECUTADA`() <- linhas.resumo_c %>%
      `DATA DE ASSINATURA`() <- linhas.resumo_c %>%
      `DATA INICIO OBRA`() <- linhas.resumo_c %>%
      `DATA TERMINO OBRA ORIGINAL`() <- linhas.resumo_c %>%
      `DATA TERMINO OBRA ATUAL`() <- linhas.resumo_c %>%
      `SUBSIDIO CONVENIOS`() <- linhas.resumo_c %>%
      `REDUCAO MAX GERAL (PJ)`() <- linhas.resumo_c %>%
      `REDUCAO MAX ETAPA (PJ)`() <- linhas.resumo_c %>%
      `AMORT. RECOMP. ETAPA (PJ)`() <- linhas.resumo_c %>%
      `PERC ANTEC (PJ)`() <- linhas.resumo_c %>%
      # Terceira coluna
      # Consolidando todas as variéveis em uma tabela
      dados.cef.dcd.resumo_t() <-
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
        `RECOMPOSICAO ETAPA (PJ)` = "RECOMPOSICAO ETAPA (PJ)"
        # Segunda coluna
      )
    return(dados.cef.dcd.resumo_t)
  }

# Teste -------------------------------------------------------------------

caminhos.dcds_c <-
  list.files(
    here("Relatórios - Documentos", "Relatorios - CIWEB"),
    full.names = TRUE, recursive = T
  ) %>%
  keep(~ str_detect(.x, "_DEMONST_CRONOGRAMA.pdf"))
f_caminho.arquivo_c <- caminhos.dcds_c[1]
#  here(
#    "Relatórios - Documentos", "Relatorios - Extratos",
#    "Estação", "Fevereiro 2025", "CAIXA -  2419 - FEVEREIRO.pdf"
#  )
#  here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
extrato <- dados_cef_extrato(f_caminho.arquivo.extrato_cef_c)
# shell.exec(f_caminho.arquivo.extrato_cef_c)

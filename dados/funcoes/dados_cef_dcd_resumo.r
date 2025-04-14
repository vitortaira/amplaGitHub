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
      str_extract("\\d{12}")
    `NUMERO DO PEDIDO` <- linhas.resumo_c %>%
    `NUMERO DO EMPREENDIMENTO` <- linhas.resumo_c %>%
    `QUANTIDADE DE PARCELAS` <- linhas.resumo_c %>%
    `NUMERO DO APF` <- linhas.resumo_c %>%
    `CODIGO SEGURADORA SGC` <- linhas.resumo_c %>%
    `CODIGO SEGURADORA SRE` <- linhas.resumo_c %>%
    `CODIGO SEGURADORA SGP` <- linhas.resumo_c %>%
    `CODIGO SEGURADORA SGT` <- linhas.resumo_c %>%
    `QUANTIDADE DE UNIDADES` <- linhas.resumo_c %>%
    `VR CUSTO OBRA` <- linhas.resumo_c %>%
    `ORCAMENTO COMPRA/VENDA` <- linhas.resumo_c %>%
    `VR COMPRA/VENDA UNIDADE` <- linhas.resumo_c %>%
    `VR COMPRA/VENDA TERRENO` <- linhas.resumo_c %>%
    `SALDO MUTUARIO (PF)` <- linhas.resumo_c %>%
    `SALDO APORTE CONTRUTORA` <- linhas.resumo_c %>%
    `SALDO MUTUARIO (PJ)` <- linhas.resumo_c %>%
    `SALDO DEVEDOR (PJ)` <- linhas.resumo_c %>%
    `SUBSIDIO RES. 460` <- linhas.resumo_c %>%
    `TOTAL SUPLEMENTACAO (PJ)` <- linhas.resumo_c %>%
    `MAXIMO LIB. GERAL (PJ)` <- linhas.resumo_c %>%
    `MAXIMO LIB. ETAPA (PJ)` <- linhas.resumo_c %>%
    `RECOMPOSICAO ETAPA (PJ)` <- linhas.resumo_c %>%
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

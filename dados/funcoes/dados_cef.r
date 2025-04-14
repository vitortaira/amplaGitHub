source(
  here(
    "Controladoria - Documentos", "Ampla_Github", "dados", "funcoes",
    "dados_cef_ecn.R"
  )
)
dados_cef <-
  function() {
    dados.cef_l <-
      list(
        cmfcn = dados_cef_cmfcns(),
        ecnE = dados_cef_ecn()$Empreendimento,
        ecnPJ = dados_cef_ecn()$Emprestimo,
        ecnU = dados_cef_ecn()$Unidades,
        ecnC = dados_cef_ecn()$Consolidado,
        epr = dados_cef_eprs(),
        extCEF = dados_cef_extratos()
      )
    return(dados.cef_l)
  }

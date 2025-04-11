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
        ecnE = dados_cef_ecn()$Empreendimento,
        ecnPJ = dados_cef_ecn()$Emprestimo,
        ecnU = dados_cef_ecn()$Unidades,
        ecnC = dados_cef_ecn()$Consolidado
      )
    return(dados.cef_l)
  }

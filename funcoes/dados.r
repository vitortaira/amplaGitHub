source(
  here(
    "Controladoria - Documentos", "Ampla_Github", "dados", "funcoes",
    "dados_cef.R"
  )
)

source(
  here(
    "Controladoria - Documentos", "Ampla_Github", "dados", "funcoes",
    "dados_ik.R"
  )
)

dados <-
  function() {
    dados_l <-
      list(
        "cef" = dados_cef(),
        "ik" = dados_ik()
      )
    return(dados_l)
  }

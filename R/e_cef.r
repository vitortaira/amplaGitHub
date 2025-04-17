source(here::here("R", "e_cef_cmfcns.R"))
source(here::here("R", "e_cef_ecns.R"))
source(here::here("R", "e_cef_eprs.R"))
source(here::here("R", "e_cef_extratos.R"))

e_cef <-
  function() {
    dados.cef_l <-
      list(
        cmfcn  = e_cef_cmfcns(),
        ecnE   = e_cef_ecns()$Empreendimento,
        ecnPJ  = e_cef_ecns()$Emprestimo,
        ecnU   = e_cef_ecns()$Unidades,
        ecnC   = e_cef_ecns()$Consolidado,
        epr    = e_cef_eprs(),
        extCEF = e_cef_extratos()
      )
    return(dados.cef_l)
  }

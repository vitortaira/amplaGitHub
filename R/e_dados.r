source(
  here(
    "Controladoria - Documentos", "AmplaR", "R", "e_cef.R"
  )
)

source(
  here(
    "Controladoria - Documentos", "AmplaR", "R", "e_ik.R"
  )
)
usethis::create_package("Controladoria - Documentos/AmplaR/AmplaR")

e_dados <-
  function() {
    dados_l <-
      list(
        "cef" = e_cef(),
        "ik" = e_ik()
      )
    return(dados_l)
  }

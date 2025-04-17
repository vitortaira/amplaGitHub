source(
  here::here(
    "R", "e_cef.R"
  )
)

source(
  here::here(
    "R", "e_ik.R"
  )
)

e_dados <-
  function() {
    dados_l <-
      list(
        "cef" = e_cef(),
        "ik" = e_ik()
      )
    return(dados_l)
  }

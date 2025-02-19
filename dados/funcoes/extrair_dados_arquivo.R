extrair_dados_arquivo <- 
  function(caminho_arquivo.c) {
    caminhos.extratos.vc <- 
      paste0(
        "Extratos/",
        list.files(caminho_pasta_extratos_c)
      )
    instituicoes.vc <- 
      caminhos.extratos.vc %>% 
      str_extract("(?<=/).*") %>% 
      str_extract("^[^_]+") %>% 
      unique()
    instituicoes.prontas.vc <- 
      c("Nubank")
    dados.pasta.l <- list()
    for (caminho.extrato.c in caminhos.extratos.vc) {
      if (caminho.extrato.c %>% 
          str_extract("(?<=/).*") %>% 
          str_extract("^[^_]+") %in% 
          instituicoes.prontas.vc) {
        if (caminho.extrato.c %>% 
            str_extract("(?<=/).*") %>% 
            str_extract("^[^_]+") ==
            "Nubank") {
          dados.pasta.l[["Nubank"]] <- 
            rbind(dados.pasta.l[["Nubank"]],
                  extrair_extrato_nubank(caminho.extrato.c))
        }
      } else {
        print(paste(caminho.extrato.c, "NÃO foi extraído."))
      }
    }
    return(dados.pasta.l)
  }

# Descrição ---------------------------------------------------------------

### RESUMO ###

# extrair_dados_relatorio_ciweb() compila numa lista todos os dados dos arquivos
# a serem extraídos da CIWEB para a elaboração do relatório.

### UTILIZAÇÃO ###

# extrair_dados_relatorio_ciweb(
#   caminhos_ciweb_relatorio.l
# )

### ARGUMENTOS ###

# caminhos_ciweb_relatorio.l: Uma nested list de strings com os caminhos dos 
# relatórios a serem extraídos.

# Função para listar todos os dados da CIWEB do relatório
extrair_dados_relatorio_ciweb <- 
  function(caminhos_ciweb_relatorio.l) {
    # Criar lista para armazenar todos os dados do relatório
    dados.ciweb.relatorio_l <- list()
    # Iterar por empreendimento
    for (empreendimento.c in caminhos_ciweb_relatorio.l$Empreendimentos) {
      # Criar lista para armazenar todos os dados do empreendimento
      dados.ciweb.relatorio_l[[empreendimento.c]] <- list()
      # Extrair tipos de relatórios a serem analisados
      tipos_vc <- 
        setdiff(names(caminhos_ciweb_relatorio.l), c("Empreendimentos", "Contagem"))
      # Iterar por tipo de relatório a ser analisado
      for (tipo.c in tipos_vc) {
      # Preencher a lista com os dados por empreendimento e tipo de relatório
        dados.ciweb.relatorio_l[[empreendimento.c]][[tipo.c]] <- lapply(
          caminhos_ciweb_relatorio.l[[tipo.c]][[empreendimento.c]],
          \(caminho) extrair_dados_arquivo_ecn(caminho, tipo.c)
        )
      }
    }
    return(dados.ciweb.relatorio_l)
  }

# Teste -------------------------------------------------------------------

#str(extrair_dados_relatorio_ciweb(extrair_caminhos_relatorio_ciweb(quais.c = "Mais recentes")))
if (file.exists("~/.Rprofile")) {
  print("Global .Rprofile found at ~/.Rprofile")
}

if (file.exists(".Rprofile")) {
  print("Project-specific .Rprofile found in current directory")
}

print(Sys.getenv("R_PROFILE_USER"))  # Show the path of the active .Rprofile

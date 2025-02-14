# Descrição ---------------------------------------------------------------

### RESUMO ###

# extrair_dados_relatorio() compila numa lista todos os dados do relatório

### UTILIZAÇÃO ###

# extrair_dados_relatorio(
#   caminhos_relatorio.l
# )

### ARGUMENTOS ###

# caminhos_relatorio.l: Uma nested list de strings com os caminhos dos 
# relatórios a serem extraídos.

# extrair_dados_relatorio() ------------------------------------------------

# Criar função para compilar numa lista todos os dados do relatório
extrair_dados_relatorio <- function(caminhos_relatorio.l) {
  ## Criar lista para armazenar todos os dados do relatório
  dados.relatorio_l <- list()
  ## Iterar por empreendimento
  for (empreendimento.c in caminhos_relatorio.l$Empreendimentos) {
    ### Criar lista para armazenar todos os dados do empreendimento
    dados.relatorio_l[[empreendimento.c]] <- list()
    ### Extrair tipos de relatórios a serem analisados
    tipos_vc <- 
      setdiff(names(caminhos_relatorio.l), c("Empreendimentos", "Contagem"))
    ### Iterar por tipo de relatório a ser analisado
    for (tipo.c in tipos_vc) {
    ### Preencher a lista com os dados por empreendimento e tipo de relatório
      dados.relatorio_l[[empreendimento.c]][[tipo.c]] <- lapply(
        caminhos_relatorio.l[[tipo.c]][[empreendimento.c]],
        \(caminho) extrair_dados_arquivo_ecn(caminho, tipo.c)
      )
    }
  }
  return(dados.relatorio_l)
}

# Teste -------------------------------------------------------------------

#extrair_dados_relatorio(extrair_caminhos_relatorio())
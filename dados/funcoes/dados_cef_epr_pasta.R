# Descrição ---------------------------------------------------------------

### RESUMO ###

# dados_cef_epr_pasta() extrai e consolida os dados dos relatórios EPR da CEF
# que estão na pasta "Relatorios - CIWEB".

### UTILIZAÇÃO ###

# dados_cef_epr_pasta(
#   f_caminho.pasta.ciweb_c
# )

### ARGUMENTOS ###

# f_caminho.pasta.ciweb_c: String do caminho da pasta "Relatorios - CIWEB".

source(here("dados", "funcoes", "dados_cef_epr.R"))

# Pacotes -----------------------------------------------------------------

library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(pdftools) # Funções para extração de dados em PDF
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2

# Função ------------------------------------------------------------------

# Define a função
dados_cef_epr_pasta <- 
  function(
    f_caminho.pasta.ciweb_c = 
      here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB")
  ) {
    # Consolida os dados dos relatórios EPR da CEF na pasta "Relatorios - CIWEB"
    caminhos.cef.epr_c <- 
      list.files(
        f_caminho.pasta.ciweb_c, full.names = TRUE, recursive = T
      ) %>%
      keep(~ str_ends(.x, "CONTRATOS_EMPREEND.pdf"))
    eprs_l <- list()
    eprs_t <- data.frame()
    for (
      i_caminho.cef.epr_c in caminhos.cef.epr_c
    ) {
      eprs_l[[i_caminho.cef.epr_c]] <- 
        dados_cef_epr(i_caminho.cef.epr_c)
      eprs_t <- 
        bind_rows(eprs_t, eprs_l[[i_caminho.cef.epr_c]])
    }
    eprs_t %<>% distinct
    return(eprs_t)
  }

# Teste -------------------------------------------------------------------

#f_caminho.pasta.ciweb_c <-
#  here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "EPR",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
#eprs_t <- dados_cef_epr_pasta()
#shell.exec(f_caminho.pasta.ciweb_c)
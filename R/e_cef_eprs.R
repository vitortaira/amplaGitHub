# Descrição ---------------------------------------------------------------

### RESUMO ###

# e_cef_eprs() extrai e consolida os dados dos relatórios EPR da CEF
# que estão na pasta "Relatorios - CIWEB".

### UTILIZAÇÃO ###

# e_cef_eprs(
#   f_caminho.pasta.ciweb_c
# )

### ARGUMENTOS ###

# f_caminho.pasta.ciweb_c: String do caminho da pasta "Relatorios - CIWEB".

# Pacotes -----------------------------------------------------------------

library(pdftools) # Funções para extração de dados em PDF

# Função ------------------------------------------------------------------

# Define a função
e_cef_eprs <-
  function(f_caminho.pasta.ciweb_c =
             file.path(dirname(dirname(here())), "Relatórios - Documentos", "Relatorios - CIWEB")) {
    # Consolida os dados dos relatórios EPR da CEF na pasta "Relatorios - CIWEB"
    caminhos.cef.epr_c <-
      list.files(
        f_caminho.pasta.ciweb_c,
        full.names = TRUE, recursive = T
      ) %>%
      keep(~ str_ends(.x, "CONTRATOS_EMPREEND.pdf"))
    eprs_l <- list()
    eprs_t <- data.frame()
    for (
      i_caminho.cef.epr_c in caminhos.cef.epr_c
    ) {
      eprs_l[[i_caminho.cef.epr_c]] <-
        e_cef_epr(i_caminho.cef.epr_c)
      eprs_t <-
        bind_rows(eprs_t, eprs_l[[i_caminho.cef.epr_c]])
    }
    eprs_t %<>% distinct %>% as_tibble()
    return(eprs_t)
  }

# Teste -------------------------------------------------------------------

# f_caminho.pasta.ciweb_c <-
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "EPR",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
# eprs_t <- e_cef_eprs()
# shell.exec(f_caminho.pasta.ciweb_c)

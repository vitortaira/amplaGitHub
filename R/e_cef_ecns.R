# Descrição ---------------------------------------------------------------

### RESUMO ###

# e_cef_ecns() extrai e consolida os dados dos relatórios ECN da CEF
# que estão na pasta "Relatorios - CIWEB".

### UTILIZAÇÃO ###

# e_cef_ecns(
#   f_caminho.pasta.ciweb_c
# )

### ARGUMENTOS ###

# f_caminho.pasta.ciweb_c: String do caminho da pasta "Relatorios - CIWEB".

source(
  here(
    "Controladoria - Documentos", "AmplaR", "R", "e_cef_ecn.R"
  )
)

# Função ------------------------------------------------------------------

# Define a função
e_cef_ecns <-
  function(f_caminho.pasta.ciweb_c =
             here("Relatórios - Documentos", "Relatorios - CIWEB")) {
    # Consolida os dados dos relatórios ECN da CEF na pasta "Relatorios - CIWEB"
    caminhos.ecn_c <-
      list.files(f_caminho.pasta.ciweb_c, full.names = TRUE, recursive = T) %>%
      keep(~ str_ends(.x, "(?i)empreendimento_construcao.pdf"))
    ecns.empreendimento_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Empreendimento) %>%
      distinct()
    ecns.emprestimo_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Emprestimo) %>%
      distinct()
    ecns.consolidado_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Consolidado) %>%
      distinct()
    ecns.unidades_t <-
      caminhos.ecn_c %>%
      map_dfr(~ e_cef_ecn(.x)$Unidades) %>%
      distinct()
    ecns_l <-
      list(
        Empreendimento = ecns.empreendimento_t,
        Emprestimo = ecns.emprestimo_t,
        Consolidado = ecns.consolidado_t,
        Unidades = ecns.unidades_t
      )
    return(ecns_l)
  }

# Teste -------------------------------------------------------------------

# f_caminho.pasta.ciweb_c <-
#  here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "ECN",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
# ecns_t <- e_cef_ecns()
# shell.exec(f_caminho.pasta.ciweb_c)

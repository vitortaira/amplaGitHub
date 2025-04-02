# Descrição ---------------------------------------------------------------

### RESUMO ###

# dados_cef_epr() extrai os dados de um relatório EPR da CEF em PDF.

### UTILIZAÇÃO ###

# dados_cef_epr(
#   f_caminho.epr_c
# )

### ARGUMENTOS ###

# f_caminho.epr_c: String do caminho de um relatório EPR.

# Pacotes -----------------------------------------------------------------

library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(pdftools) # Funções para extração de dados em PDF
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2

# Função ------------------------------------------------------------------

# Define a função
dados_cef_epr <- 
  function(f_caminho.epr_c) {
    # Define paginas_l
    paginas_l <- 
      pdf_text(f_caminho.epr_c) %>% 
      map(
        ~ str_split(.x, "\n")[[1]] %>% 
          str_squish() %>% 
          discard(~ .x == "")
      )
    # Define linhas_c
    epr_t <- 
      paginas_l %>%
      unlist(use.names = FALSE) %>% 
      keep(~ str_starts(.x, "\\d{12}")) %>% 
      as_tibble_col(column_name = "Linhas") %>% 
      mutate(
        CONTRATO = Linhas %>% str_extract("^\\d{12}"),
        Linhas = Linhas %>% str_remove("^\\d{12}\\s?"),
        `NOME MUTUARIO` = Linhas %>% str_remove("\\d{5}.*") %>% str_trim,
        Linhas = Linhas %>% str_extract("\\d{5}.*"),
        UNO = Linhas %>% str_extract("^\\d{5}"),
        Linhas = Linhas %>% str_remove("^\\d{5}\\s?"),
        ORR = Linhas %>% str_extract("^\\d{3}"),
        Linhas = Linhas %>% str_remove("^\\d{3}\\s?"),
        TO = Linhas %>% str_extract("^\\d{1}") %>% as.integer,
        Linhas = Linhas %>% str_remove("^\\d{1}\\s?"),
        COD = Linhas %>% str_extract("^\\d{3}"),
        Linhas = Linhas %>% str_remove("^\\d{3}\\s?"),
        `DT. ASSIN` = 
          Linhas %>%
          str_extract("^\\d{2}/\\d{2}/\\d{2}") %>%
          as.Date(format = "%d/%m/%y"),
        Linhas = Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{2}\\s?"),
        `DT. INC. CTR` = 
          Linhas %>%
          str_extract("\\d{2}/\\d{2}/\\d{2}") %>%
          as.Date(format = "%d/%m/%y"),
        Linhas = Linhas %>% str_remove("\\d{2}/\\d{2}/\\d{2}\\s?"),
        `DT. INC. REG` =
          Linhas %>%
          str_extract("\\d{2}/\\d{2}/\\d{2}") %>%
          as.Date(format = "%d/%m/%y"),
        Linhas = Linhas %>% str_remove("\\d{2}/\\d{2}/\\d{2}\\s?"),
        `VR RETIDO` = 
          Linhas %>%
          str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric,
        Linhas = Linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?"),
        `VR AMORTIZ` = 
          Linhas %>%
          str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric,
        Linhas = Linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?"),
        AMO = Linhas %>% word(-1),
        Linhas = Linhas %>% str_remove("\\s?\\S+$"),
        `GAR. AUT` = Linhas %>% word(-1) %>% as.integer,
        `TIPO UND` = 
          if_else(
            Linhas %>% as.character %>% str_count("\\S+") == 1,
            NA_character_,
            Linhas %>% as.character %>% word(1)
          )
      ) %>% 
      select(
        CONTRATO, `NOME MUTUARIO`, UNO, ORR, TO, COD, `DT. ASSIN`,
        `TIPO UND`, `GAR. AUT`, `DT. INC. CTR`, `DT. INC. REG`, `VR RETIDO`,
        `VR AMORTIZ`, AMO
      )
    return(epr_t)
  }

# Teste -------------------------------------------------------------------

#f_caminho.epr_c <-
#  here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "EPR",
#    "20250311_123902_696_PP_177770014920_CONTRATOS_EMPREEND.pdf"
#  )
#  here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
#extrato <- dados_epr(f_caminho.epr_c)
#shell.exec(f_caminho.epr_c)
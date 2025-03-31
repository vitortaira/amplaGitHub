# Descrição ---------------------------------------------------------------

### RESUMO ###

# extrair_dados_arquivo_cmfcn() extrai os dados de um arquivo CMF_CN.

### UTILIZAÇÃO ###

# extrair_dados_arquivo_cmfcn(
#   f_f_caminho.arquivo.cmfcn_c
# )

### ARGUMENTOS ###

# f_caminho.arquivo.cmfcn_c: String do caminho do arquivo CMF_CN.

# Pacotes -----------------------------------------------------------------

library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(pdftools) # Funções para extração de dados em PDF
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2

# Função ------------------------------------------------------------------

# Define a função
extrair_dados_arquivo_cmfcn <- 
  function(f_caminho.arquivo.cmfcn_c) {

# paginas_l e linhas_c ----------------------------------------------------

  paginas_l <- 
    pdf_text(f_caminho.arquivo.cmfcn_c) %>% 
    map(
      ~ str_split(.x, "\n")[[1]] %>% 
        str_squish() %>% 
        discard(~ .x == "")
    )
  emitente_c <- paginas_l[[1]] %>% last() %>% str_remove("^Emitente: ")
  linhas_c <- 
    unlist(paginas_l, use.names = FALSE) %>%
    keep(~ !str_starts(.x, "Emitente:")) %>% 
    as_tibble_col(column_name = "Linhas") %>%
    # Concatenar linhas do cabeçalho
    mutate(
      Linhas = 
        if_else(str_detect(Linhas, "^CONTRATO"),
                str_c(Linhas, lead(Linhas), sep = " "),
                Linhas),
      Drop = lag(str_detect(Linhas, "^CONTRATO"), default = FALSE)
    ) %>% 
    filter(!Drop) %>% 
    select(Linhas) %>%
    # Concatenar linhas iniciadas em contratos com iniciadas em conta SIDEC/NSGD
    mutate(
      Linhas = 
        if_else(str_detect(Linhas, "^[0-9]{12}"),
                str_c(Linhas, lead(Linhas), sep = " "),
                Linhas),
      Drop = lag(str_detect(Linhas, "^[0-9]{12}"), default = FALSE)
    ) %>% 
    filter(!Drop) %>% 
    pull(Linhas)
  linhas.blocos_t <-
    linhas_c %>% 
    as_tibble_col(column_name = "Linhas") %>% 
    mutate(
      Quebras = 
        if_else(
          str_detect(Linhas, "^[A-Z]") & !str_detect(Linhas, "^CIF|^CL"),
          TRUE,
          FALSE
        ),
      Blocos = cumsum(Quebras)
    )
  linhas.blocos_l <- 
    linhas.blocos_t %>% 
    group_split(Blocos) %>% 
    setNames(linhas.blocos_t %>% pull(Blocos) %>% unique)

# Índices -----------------------------------------------------------------

  indices.cabecalho_i <- linhas_c %>% str_which("^CONTRATO")
  indices.conta_i <- linhas_c %>% str_which("^[0-9]{5}\\.")
  indices.contrato_i <- linhas_c %>% str_which("^[0-9]{12}")
  indices.total.dt.rem_i <- linhas_c %>% str_which("^TOTAL DT.REM")
  indices.totais_i <- linhas_c %>% str_which("^TOTAIS")
  
# Totais ------------------------------------------------------------------
  
  linhas.blocos.totais_t <-
    linhas.blocos_l %>% 
    keep(~ str_detect(.x$Linhas[1], "^TOTAIS")) %>% 
    map2(
      seq_along(.),
      ~ mutate(.x, Blocos.totais = .y)
    ) %>% 
    bind_rows() %>% 
    select(-Quebras)
  
  linhas.blocos.total.dt.rem_t <-
    linhas.blocos_l %>% 
    keep(~ str_detect(.x$Linhas[1], "^TOTAL")) %>% 
    map2(
      seq_along(.),
      ~ mutate(.x, Blocos.total.dt.rem = .y)
    ) %>%
    bind_rows() %>%
    select(-Quebras) %>% 
    filter(Blocos < min(linhas.blocos.totais_t$Blocos))
    #keep(~ .x$Blocos < names(linhas.blocos.totais_l)[1])
  
# Lançamentos -------------------------------------------------------------

  #linhas.lancamentos_c <- linhas_c %>% head(indices.totais_i[1] - 1)
  linhas.blocos.contrato_t <- 
    linhas.blocos_l %>% 
    keep(~ str_detect(.x$Linhas[1], "^CONTRATO") & nrow(.x) > 1) %>% 
    map2(
      seq_along(.),
      ~ mutate(.x, Blocos.contrato = .y)
    ) %>% 
    map(~ slice(.x, -1)) %>% 
    bind_rows() %>% 
    select(-Quebras) %>% 
    mutate(
      CONTRATO = Linhas %>% str_extract("^[0-9]{12}"),
      Linhas = Linhas %>% str_remove("^[0-9]{12}") %>% str_trim,
      `DT. LANCTO` = 
        Linhas %>% 
          str_extract("^\\d{2}/\\d{2}/\\d{4}") %>% 
          as.Date(format = "%d/%m/%Y"),
      Linhas = 
        Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim,
      `DT. REMES.` = 
        Linhas %>% 
          str_extract("^\\d{2}/\\d{2}/\\d{4}") %>% 
        as.Date(format = "%d/%m/%Y"),
      Linhas = 
        Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim,
      LANCAMENTOS = Linhas %>% str_extract("^.*?(?=\\s\\d{2}\\s)"),
      Linhas = Linhas %>% str_remove("^.*?(?=\\s\\d{2}\\s)") %>% str_trim,
      NP = Linhas %>% word() %>% as.integer,
      Linhas = 
        Linhas %>% str_remove(str_c("^", word(.))) %>% str_trim,
      `CONTA SIDEC/NSGD` = Linhas %>% word,
      Linhas = Linhas %>% str_remove(str_c("^", word(.))) %>% str_trim,
      VALOR = 
        Linhas %>% 
          word() %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric,
      Linhas = Linhas %>% str_remove(str_c("^", word(.))) %>% str_trim,
      SITUACAO = Linhas %>% str_sub(1, -4),
      `MOT.` = Linhas %>% str_sub(-2, -1) %>% as.integer
    ) %>% 
    select(-Linhas) %>% 
    filter(Blocos < min(linhas.blocos.totais_t$Blocos))
  
  }

# Teste -------------------------------------------------------------------

comeco.linhas_c <- word(linhas_c)
comeco.linhas.letras_c <- 
  comeco.linhas_c %>%
  keep(~ str_detect(.x, "^[A-Z]") & !str_detect(.x, "^CIF|^CL"))
comeco.linhas.numeros_c <- 
  comeco.linhas_c %>% keep(~ str_detect(.x, "^\\d"))
sort(unique(diff(str_which(linhas.lancamentos_c, "^CONTRATO"))))
f_caminho.arquivo.cmfcn_c <- 
  here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
    "1. UP Vila Sonia", "11.03.25",
    "CMF", "20250311_123855_693_PP_177770014920_MOV_FINANC_CN.pdf"
  )
#extrair_dados_arquivo_cmfcn(f_caminho.arquivo.cmfcn_c)
#shell.exec(f_caminho.arquivo.cmfcn_c)
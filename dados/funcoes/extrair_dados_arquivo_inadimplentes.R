# Descrição ---------------------------------------------------------------

# Condições para o funcionamento:
  # Arquivos em ".xlsx": 
    # (1) Estejam na pasta ".../Controladoria - Docmentos/Ampla_Github/dados/cef
    # /inadimplentes
    # (2) Sejam nomeados somente com um breve nome do empreendimento
    # (eg "sonia1", "pompeia", etc.)

library(here) # Facilita a identificação de caminhos
library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(openxlsx) # Funções para preencher arquivos .xlsx
library(readxl) # Importação de arquivos em Excel, e.g. read_excel()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2
library(viridisLite) # Mapeamento de cores

extrair_dados_arquivo_inadimplentes <-
  function(caminho_arquivo_inadimplentes.c) {
    # Extraindo o vetor de linhas
    linhas_vc <- 
      read_excel(caminho_arquivo_inadimplentes.c, col_names = F) %>% 
      unite("linhas", everything(), sep = " ", remove = T) %>%
      unlist() %>% 
      str_remove_all("NA") %>% 
      str_trim() %>% 
      suppressMessages()
    linhas_vc <- 
      linhas_vc[linhas_vc != ""] %>% 
      sapply(function(x) str_replace_all(x, "\\s+", " ")) %>% 
      unlist() %>% 
      str_trim() %>% 
      unname()
    # Fora da tabela
    indice.data.impressao_vn <- 
      linhas_vc %>% 
      str_which("Impresso em:")
    data.impressao_p <- 
      linhas_vc[indice.data.impressao_vn[1]] %>% 
      str_remove(".* Impresso em: ") %>% 
      str_trim() %>% 
      as.POSIXct(format = "%d/%m/%Y %H:%M:%S")
    indice.linhas.remover_vn <- 
      linhas_vc %>% 
      str_which("^Folha|^Relatório de Inadimplência|^Título|^Esp ")
    linhas_vc <- 
      linhas_vc[-indice.linhas.remover_vn]
    indice.clientes_vn <- 
      linhas_vc %>% 
      str_which("^Cliente: ")
    clientes_vc <- 
      linhas_vc[indice.clientes_vn] %>% 
      str_remove(".*Cliente: ") %>% 
      str_remove(" Contrato: .*") %>% 
      str_trim()
    contratos_vc <- 
      linhas_vc[indice.clientes_vn] %>% 
      str_remove(".* Contrato: ")
    telefones_vc <- 
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove("^Telefones: ") %>% 
      str_remove(" Unidade:.*") %>% 
      str_remove(" ") %>% 
      str_remove("0xx") %>% 
      str_trim()
    unidades_vc <- 
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove(".*Unidade: ") %>% 
      str_trim()
    indice.fim.clientes_vn <- 
      linhas_vc %>% 
      str_which("Parcela")
    abertas_vi <- 
      linhas_vc[indice.fim.clientes_vn] %>% 
      str_remove(".*: ") %>% 
      str_remove(" Parcelas") %>% 
      str_remove(" Parcela") %>% 
      str_extract("^[^ ]+")
    indice.parcelas_l <- list()
    for (i in seq_along(clientes_vc)) {
      # Índice das linhas de parcelas com todos os clientes
      indice.parcelas_l[[i]] <-
        (indice.clientes_vn[i] + 2):(indice.fim.clientes_vn[i] - 1)
    }
    parcelas.clientes_vn <- c()
    for (i in seq_along(clientes_vc)) {
      parcelas.clientes_vn[i] <- 
        (indice.fim.clientes_vn[i] - 1) - (indice.clientes_vn[i] + 2) + 1
    }
    parcelas.clientes_vc <- 
      rep(clientes_vc, parcelas.clientes_vn)
    parcelas.contratos_vc <- 
      rep(contratos_vc, parcelas.clientes_vn)
    parcelas.unidades_vc <- 
      rep(unidades_vc, parcelas.clientes_vn)
    parcelas.telefones_vc <- 
      rep(telefones_vc, parcelas.clientes_vn)
    parcelas.abertas_vi <- 
      rep(abertas_vi, parcelas.clientes_vn)
    indice.parcelas_vn <- indice.parcelas_l %>% unlist()
    parcelas_df <- 
      linhas_vc[indice.parcelas_vn] %>% 
      str_remove("Imobiliaria/Corretor: ") %>% 
      as_tibble() %>% 
      separate_wider_delim(
        cols = everything(),
        names = 
          c(
            "Esp", "Parcela", "Ele", "Vencto", "Atraso", "R/F", "Principal",
            "Juros", "Encargos", "Juros de Mora", "Multa", "Seguro", "Total"
          ),
        delim = " "
      ) %>% 
      mutate(
        Cliente = parcelas.clientes_vc,
        Esp = as.character(Esp),
        Parcela = as.character(Parcela),
        `Quantidade de parcelas` = as.integer(parcelas.abertas_vi),
        Ele = as.character(Ele),
        Vencto = as.character(Vencto) %>% as.Date(format = "%d/%m/%Y"),
        `R/F` = as.character(`R/F`),
        Principal =
          Principal %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        Juros =
          Juros %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        Encargos =
          Encargos %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        `Juros de Mora` =
          `Juros de Mora` %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        Multa =
          Multa %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        Seguro =
          Seguro %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        Total =
          Total %>% 
          str_remove_all("\\.") %>% 
          str_replace("\\,", "\\.") %>% 
          as.numeric(),
        `Data da consulta` = 
          as.POSIXct(data.impressao_p, format = "%Y-%m-%d %H:%M:%S"),
        Atraso = as.integer(Atraso),
        Empreendimento = basename(caminho_arquivo_inadimplentes.c) %>%
          str_remove(".xlsx"),
        Contrato = as.character(parcelas.contratos_vc),
        Unidade = as.character(parcelas.unidades_vc),
        Telefone = as.character(parcelas.telefones_vc)
      ) %>% 
      select(
        Empreendimento, Contrato, Unidade, Cliente, Telefone, Esp, Parcela, 
        `Quantidade de parcelas`, Ele, Vencto, Atraso, `R/F`, Principal, Juros,
        Encargos, `Juros de Mora`, Multa, Seguro, Total, `Data da consulta`
      )
    return("Parcelas" = parcelas_df)
  }

# Teste -------------------------------------------------------------------

#caminho_arquivo_inadimplentes.c <-
#  here("dados", "cef", "inadimplentes", "pompeia.xlsx")
#str(extrair_dados_arquivo_inadimplentes(caminho_arquivo_inadimplentes.c))
#View(extrair_dados_arquivo_inadimplentes(caminho_arquivo_inadimplentes.c)$Parcelas)
#View(extrair_dados_arquivo_inadimplentes(caminho_arquivo_inadimplentes.c)$Consolidado)

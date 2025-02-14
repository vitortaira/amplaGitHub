rm(list = ls())

library(here) # Facilita a identificação de caminhos
library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(openxlsx) # Funções para preencher arquivos .xlsx
#library(purrr) # Funções de manipulação, e.g. keep()
library(readxl) # Importação de arquivos em Excel, e.g. read_excel()
library(stringr) # Funções para formatar códigos, e.g. style_file()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2
library(viridisLite)

extrair_dados_inadimplentes <- 
  function(caminho_arquivo_inadimplentes.c) {
    linhas_vc <- 
      read_excel(caminho_arquivo_inadimplentes.c) %>% 
      unite("linhas", everything(), sep = " ", remove = T) %>%
      unlist() %>% 
      str_remove_all("NA") %>% 
      str_trim() %>% 
      suppressMessages()
    linhas_vc <- 
      linhas_vc[str_trim(linhas_vc) != ""] %>% 
      str_trim() %>% 
      sapply(function(x) str_replace_all(x, "\\s+", " ")) %>% 
      unlist() %>% 
      str_trim() %>% 
      unname()
    indice.data.impressao_vn <- 
      linhas_vc %>% 
      str_which("Impresso em:")
    data.impressao_p <- 
      linhas_vc[indice.data.impressao_vn[1]] %>% 
      str_remove(".* Impresso em: ") %>% 
      str_trim() %>% 
      as.POSIXct(format = "%d/%m/%Y %H:%M:%S")
    if (linhas_vc %>% str_which("^Folha") %>% length() > 0) {
      # Linhas a serem removidas
      linhas.remover_vc <- 
        c("^Folha", "^Relatório de Inadimplência", "^Título", "^Esp ")
      # Função para remover linhas com os padrões acima
      remover_linhas <- function(linhas.vc) {
        reduce(linhas.remover_vc, ~ .x[-str_which(.x, .y)], .init = linhas.vc)
      }
      # Aplica a função remover_linhas()
      linhas_vc <- remover_linhas(linhas_vc)
    }
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
    indice.fim.clientes_l <- list()
    for (i in seq_along(clientes_vc)) {
      # Índice das últimas linhas dos blocos por cliente
      indice.fim.clientes_l[[i]] <-
        linhas_vc %>% 
        str_which(paste0("^", clientes_vc[i]))
    }
    # Se um cliente tem parcelas inadimplentes em mais de um contrato
    if (indice.fim.clientes_l %>% unlist() %>% length() > 
        length(indice.fim.clientes_l)) {
      # Filtrando só as entradas da lista que possuem mais de uma entrada
      indice.entradas.multiplas.fim.clientes_vn <- 
        which(map_int(indice.fim.clientes_l, length) > 1)
      for (i in indice.entradas.multiplas.fim.clientes_vn) {
        indice.fim.clientes_l[[i]] <- indice.fim.clientes_l[[i]][i]
      }
    }
    indice.fim.clientes_vn <- indice.fim.clientes_l %>% unlist()
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
    parcelas.clientes_vc <- c()
    parcelas.clientes_vc <- 
        rep(clientes_vc, parcelas.clientes_vn)
    indice.parcelas_vn <- indice.parcelas_l %>% unlist()
    parcelas_df <- 
      linhas_vc[indice.parcelas_vn] %>% 
      str_remove("Imobiliaria/Corretor: ") %>% 
      as.tibble() %>% 
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
        `Data da consulta` = as.character(data.impressao_p),
        Atraso = as.integer(Atraso)
      ) %>% 
      select(Cliente, everything())
    consolidado_df <- 
      linhas_vc[indice.fim.clientes_vn] %>% 
      str_remove(".*: ") %>% 
      str_remove(" Parcelas") %>% 
      str_remove(" Parcela") %>% 
      as.tibble() %>% 
      separate_wider_delim(
        cols = everything(),
        names = 
          c(
            "Parcelas", "Principal", "Juros", "Encargos", "Juros de Mora",
            "Multa", "Seguro", "Total"
          ),
        delim = " "
      ) %>%
      mutate(
        Cliente = clientes_vc,
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
        Parcelas = as.integer(Parcelas),
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
        Contrato = contratos_vc,
        Unidade = unidades_vc,
        Telefone = telefones_vc,
        `Data da consulta` = as.character(data.impressao_p)
      ) %>% 
      select(Cliente, everything())
    return(list("Parcelas" = parcelas_df, "Consolidado" = consolidado_df))
  }

# Formatar todos os arquivos da pasta -------------------------------------

caminhos.arquivos_vc <-
  setdiff(
    list.files(here("inadimplentes"), full.names = T),
    c(
      here("inadimplentes", "formatar_inadimplentes.R"),
      list.files(
        here("inadimplentes"),
        full.names = T)[str_which(list.files(here("inadimplentes")),
                                  "inadimplentes")]
    )
  )
dados.pasta_l <- list()
for (caminho_arquivo.c in caminhos.arquivos_vc) {
  dados.pasta_l[[basename(caminho_arquivo.c)]] <- 
    extrair_dados_inadimplentes(caminho_arquivo.c)
 print(paste(caminho_arquivo.c, "extraído com sucesso.")) 
}
cores.arquivos_vc <- 
  viridis(length(caminhos.arquivos_vc)) %>% 
  str_remove("FF")

# Salvando num xlsx -------------------------------------------------------

xlsx <- 
  createWorkbook()
nomes.abas.xlsx_l <- 
  list(
    "Parcelas" =
      paste0(
        caminhos.arquivos_vc %>% 
          basename() %>% 
          str_remove_all("\\.xlsx$"),
        " - Parcelas"
      ),
    "Consolidado" =
      paste0(
        caminhos.arquivos_vc %>% 
          basename() %>% 
          str_remove_all("\\.xlsx$"),
        " - Consolidado"
      )
  )
  
for (i in seq_along(caminhos.arquivos_vc)) {
  # Aba "Parcelas"
  addWorksheet(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    gridLines = F,
    tabColour = cores.arquivos_vc[i]
  )
    # Popular a aba
  writeData(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    dados.pasta_l[[i]]$Parcelas
  )
    # Formatação geral da tabela
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Parcelas) + 1,
    cols = 1:ncol(dados.pasta_l[[i]]$Parcelas),
    gridExpand = T
  )
    # Formatar largura das colunas da tabela
  setColWidths(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    cols = 1:ncol(dados.pasta_l[[i]]$Parcelas),
    widths = 18
  )
    # Congelar a primeira linha
  freezePane(xlsx, nomes.abas.xlsx_l[["Parcelas"]][i], firstRow = T)
    # Adicionar filtro à tabela
  addFilter(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    cols = 1:ncol(dados.pasta_l[[i]]$Parcelas),
    rows = 1
  )
    # Formatar cabeçalho 
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style =
      createStyle(
        border = "TopBottomLeftRight",
        fontSize = 12,
        fontColour = "white",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "darkgray"
      ),
    rows = 1,
    cols = 1:ncol(dados.pasta_l[[i]]$Parcelas),
    gridExpand = T
  )
    # Formatar a coluna "Cliente"
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "left",
        valign = "center",
        wrapText = T
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Parcelas) + 1,
    cols = which(colnames(dados.pasta_l[[i]]$Parcelas) == "Cliente"),
    gridExpand = T
  )
    # Formatar a coluna "Vencto" como data
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        numFmt = "DD/MM/YYYY"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Parcelas) + 1,
    cols = which(colnames(dados.pasta_l[[i]]$Parcelas) == "Vencto"),
    gridExpand = T
  )
    # Formatar a coluna "Atraso" como inteiro
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        numFmt = "#,##0"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Parcelas) + 1,
    cols = which(colnames(dados.pasta_l[[i]]$Parcelas) == "Atraso"),
    gridExpand = T
  )
    # Formatar colunas com valores monetários
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Parcelas"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        numFmt = "#,##0.00"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Parcelas) + 1,
    cols = 
      which(
        colnames(dados.pasta_l[[i]]$Parcelas) %in%
          c("Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
            "Seguro", "Total")
        ),
    gridExpand = T
  )
  # Aba "Consolidado"
  addWorksheet(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    gridLines = F,
    tabColour = cores.arquivos_vc[i]
  )
    # Popular a aba
  writeData(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    dados.pasta_l[[i]]$Consolidado
  )
    # Formatação geral da tabela
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Consolidado) + 1,
    cols = 1:ncol(dados.pasta_l[[i]]$Consolidado),
    gridExpand = T
  )
    # Formatar largura das colunas da tabela
  setColWidths(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    cols = 1:ncol(dados.pasta_l[[i]]$Consolidado),
    widths = 18
  )
  # Congelar a primeira linha
  freezePane(xlsx, nomes.abas.xlsx_l[["Consolidado"]][i], firstRow = T)
    # Adicionar filtro à tabela
  addFilter(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    cols = 1:ncol(dados.pasta_l[[i]]$Consolidado),
    rows = 1
  )
    # Formatar cabeçalho 
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    style =
      createStyle(
        border = "TopBottomLeftRight",
        fontSize = 12,
        fontColour = "white",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "darkgray"
      ),
    rows = 1,
    cols = 1:ncol(dados.pasta_l[[i]]$Consolidado),
    gridExpand = T
  )
    # Formatar as colunas "Cliente" e "Unidade"
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "left",
        valign = "center",
        wrapText = T
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Consolidado) + 1,
    cols = 
      which(
        colnames(dados.pasta_l[[i]]$Consolidado) %in%
          c("Cliente", "Unidade")
        ),
      ,
    gridExpand = T
  )
    # Formatar a coluna "Parcelas" como inteiro
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        numFmt = "#,##0"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Consolidado) + 1,
    cols = which(colnames(dados.pasta_l[[i]]$Consolidado) == "Parcelas"),
    gridExpand = T
  )
    # Formatar colunas com valores monetários
  addStyle(
    xlsx,
    sheet = nomes.abas.xlsx_l[["Consolidado"]][i],
    style = 
      createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center",
        numFmt = "#,##0.00"
      ),
    rows = 1:nrow(dados.pasta_l[[i]]$Consolidado) + 1,
    cols = 
      which(
        colnames(dados.pasta_l[[i]]$Consolidado) %in%
          c("Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
            "Seguro", "Total")
        ),
    gridExpand = T
  )
}
nome.xlsx_c <-
  paste0(
    "inadimplentes ",
    Sys.time() %>% str_sub(1,-11),
    ".xlsx"
  ) %>% 
  str_remove_all("\\:")
saveWorkbook(
  xlsx,
  paste0("inadimplentes/", nome.xlsx_c),
  overwrite = T)

# Teste -------------------------------------------------------------------

#caminho_arquivo_inadimplentes.c <-
#  here("inadimplentes", "inads. pomp.xlsx")
#str(extrair_dados_inadimplentes(caminho_arquivo_inadimplentes.c))

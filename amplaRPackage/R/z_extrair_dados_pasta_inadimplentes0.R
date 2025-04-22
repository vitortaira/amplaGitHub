library(here) # Facilita a identificação de caminhos
library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(openxlsx) # Funções para preencher arquivos .xlsx
library(readxl) # Importação de arquivos em Excel, e.g. read_excel()
library(stringr) # Funções para formatar códigos, e.g. style_file()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2
library(viridisLite) # Mapeamento de cores

source(here::here("R", "z_extrair_dados_arquivo_inadimplentes0.R"))

extrair_dados_pasta_inadimplentes <-
  function(caminho_pasta_inadimplentes.c = here::here("dados", "cef", "inadimplentes")) {
    # Formatar todos os arquivos da pasta -------------------------------------

    caminhos.arquivos_vc <-
      setdiff(
        list.files(caminho_pasta_inadimplentes.c, full.names = T),
        c(
          here::here("dados", "cef", "inadimplentes", "formatados"),
          list.files(
            caminho_pasta_inadimplentes.c,
            full.names = T
          )[str_which(
            list.files(caminho_pasta_inadimplentes.c),
            "inadimplentes"
          )]
        )
      )
    dados.pasta_l <- list()
    dados.pasta_df <- c()
    for (caminho_arquivo.c in caminhos.arquivos_vc) {
      dados.pasta_l[[basename(caminho_arquivo.c)]] <-
        e_cef_inad(caminho_arquivo.c)
      print(paste(caminho_arquivo.c, "extraído com sucesso."))
      dados.pasta_df <-
        rbind(dados.pasta_df, dados.pasta_l[[basename(caminho_arquivo.c)]][["Parcelas"]])
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
      # Formatar as colunas "Cliente"
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
              c(
                "Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
                "Seguro", "Total"
              )
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
          ), ,
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
              c(
                "Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
                "Seguro", "Total"
              )
          ),
        gridExpand = T
      )
    }
    nome.xlsx_c <-
      paste0(
        "inadimplentes ",
        format(Sys.time(), "%Y_%m_%d %H_%M_%S"),
        ".xlsx"
      )
    saveWorkbook(
      xlsx,
      paste0(here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c)),
      overwrite = T
    )
  }

# Teste -------------------------------------------------------------------

# caminho_arquivo_inadimplentes.c <-
#  here::here("dados", "cef", "inadimplentes", "inads. pomp.xlsx")
# str(extrair_dados_inadimplentes(caminho_arquivo_inadimplentes.c))

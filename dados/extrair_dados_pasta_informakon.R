# Descrição ---------------------------------------------------------------

### RESUMO ###

# extrair_dados_pasta_informakon() extrai os dados de um arquivo de despesas do
# Informakon.

### UTILIZAÇÃO ###

# extrair_dados_pasta_informakon(
#   caminho_pasta_informakon.c
# )

### ARGUMENTOS ###

# caminho_pasta_informakon.c: String do caminho da pasta "informakon".

# Pacotes -----------------------------------------------------------------

library(here) # Facilita a identificação de caminhos
library(lubridate)
library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(openxlsx) # Funções para preencher arquivos .xlsx
library(purrr)
library(readxl) # Funções para a importação de arquivos em Excel, e.g. read_excel()
library(styler) # Funções para formatar códigos, e.g. style_file()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2

extrair_dados_pasta_informakon <- 
  function(caminho_pasta_informakon.c = here("informakon")) {
    caminhos.arquivos.informakon_vc <- 
      list.files(
        caminho_pasta_informakon.c,
        full.names = T,
        recursive = T
      )
    dados.pasta.informakon_l <- list()
    for (caminho_arquivo_informakon.c in caminhos.arquivos.informakon_vc) {

# Despesas ----------------------------------------------------------------

      if (caminho_arquivo_informakon.c %>% basename %>% str_detect("^despesas")) {
        dados.pasta.informakon_l[["Despesas"]] <- 
          read_excel(caminho_arquivo_informakon.c) %>% 
          mutate(
            `a/c`                    = as.character(`a/c`),
            Acréscimos               = as.numeric(Acréscimos),
            `Agente Financeiro`      = as.character(`Agente Financeiro`),
            `Centro de Negócio`      = as.character(`Centro de Negócio`),
            `Cod. Centro`            = as.character(`Cod. Centro`),
            Credor                   = as.character(Credor),
            `Data Doc Pagto`         = as.Date(`Data Doc Pagto`),
            `Data Liberação`         = as.Date(`Data Liberação`),
            `Data Vencimento`        = as.Date(`Data Vencimento`),
            `Data Vencimento Origem` = as.Date(`Data Vencimento Origem`),
            Descontos                = as.numeric(Descontos),
            `Descontos Adiant.`      = as.numeric(`Descontos Adiant.`),
            Documento                = as.character(`Documento`),
            Empresa                  = `Cod. Centro` %>% str_sub(1,3),
            Encargos                 = as.numeric(Encargos),
            Mês                      = floor_date(`Data Doc Pagto`, "month"),
            Multa                    = as.numeric(Multa),
            `N° Conta`               = as.character(`N° Conta`),
            `Nº Entrada`             = as.integer(`Nº Entrada`),
            Observação               = as.character(Observação),
            Parcela                  = as.character(Parcela),
            `Total Pago`             = as.numeric(`Total Pago`),
            `Valor Titulo`           = as.numeric(`Valor Titulo`)
          ) %>% 
          filter(!is.na(`Cod. Centro`))
      }

# Receitas ----------------------------------------------------------------

      if (caminho_arquivo_informakon.c %>% basename %>% str_detect("^receitas")) {
        dados.pasta.informakon_l[["Receitas"]] <- 
          read_excel(caminho_arquivo_informakon.c, skip = 3) %>% 
          mutate(
            Agente          = as.character(Agente),
            Apto            = as.integer(Apto),
            `Cart.`         = as.factor(`Cart.`),
            Cliente         = as.character(Cliente),
            Contrato        = as.character(Contrato),
            `Data Pagto`    = as.Date(`Data Pagto`, format = "%d/%m/%Y"),
            Desconto        = as.numeric(Desconto),
            Elemento        = as.character(Elemento),
            Empreendimento  = as.character(Empreendimento),
            Encargos        = as.numeric(Encargos),
            Esp             = as.character(Esp),
            Juros           = as.numeric(Juros),
            `Juros de Mora` = as.numeric(`Juros de Mora`),
            Mês             = floor_date(`Data Pagto`, "month"),
            Multa           = as.numeric(Multa),
            Parcela         = as.character(Parcela),
            Principal       = as.numeric(Principal),
            `R/F`           = as.factor(`R/F`),
            Reajuste        = as.numeric(Reajuste),
            Seguro          = as.numeric(Seguro),
            Torre           = as.character(Torre),
            Total           = as.numeric(Total),
            Vencimento      = as.Date(Vencimento)
          )
      }
    }

# Balanço patrimonial -----------------------------------------------------

    # Despesas
    dados.despesas.bp.informakon_df <- 
      dados.pasta.informakon_l[["Despesas"]] %>%
      group_by(`Centro de Negócio`, Mês) %>% 
      summarise(
        `Total Pago no Mês` = sum(`Total Pago`, na.rm = T),
        .groups = "drop"
      ) %>% 
      ungroup()
    # Receitas
    dados.receitas.bp.informakon_df <- 
      dados.pasta.informakon_l[["Receitas"]] %>%
      group_by(Empreendimento, Mês) %>% 
      summarise(
        Total = sum(Total, na.rm = T),
        .groups = "drop"
      ) %>% 
      ungroup()

# xlsx --------------------------------------------------------------------

    # Criando o arquivo xlsx
    xlsx <- 
      createWorkbook()
    # Aba "Despesas"
    addWorksheet(
      xlsx,
      sheet = "Despesas",
      gridLines = F,
      tabColour = "red"
    )
    # Popular a aba
    writeData(
      xlsx,
      sheet = "Despesas",
      dados.pasta.informakon_l[["Despesas"]]
    )
    # Congelar a primeira linha
    freezePane(xlsx, "Despesas", firstRow = T)
    # Adicionar filtro à tabela
    addFilter(
      xlsx,
      "Despesas",
      rows = 1,
      cols = 1:ncol(dados.pasta.informakon_l[["Despesas"]])
    )
    # Formatar largura das colunas da tabela
    setColWidths(
      xlsx,
      sheet = "Despesas",
      cols = 1:ncol(dados.pasta.informakon_l[["Despesas"]]),
      widths = 18
    )
    # Formatação geral da tabela
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 1:nrow(dados.pasta.informakon_l[["Despesas"]]) + 1,
      cols = 1:ncol(dados.pasta.informakon_l[["Despesas"]]),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center"
        )
    )
    # Formatar cabeçalho
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 1,
      cols = 1:ncol(dados.pasta.informakon_l[["Despesas"]]),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          fontColour = "white",
          fontSize = 12,
          textDecoration = "bold",
          fgFill = "darkgray",
          halign = "center",
          valign = "center",
          wrapText = T
        )
    )
    # Formatar colunas do tipo data
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 2:nrow(dados.pasta.informakon_l[["Despesas"]]) + 1,
      cols = 
        dados.pasta.informakon_l[["Despesas"]] %>%
        summarise(across(everything(), ~ inherits(.x, "Date"))) %>% 
        unlist() %>% 
        which(),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "DD/MM/YYYY"
        )
    )
    # Formatar colunas do tipo numérico
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 2:nrow(dados.pasta.informakon_l[["Despesas"]]) + 1,
      cols = 
        dados.pasta.informakon_l[["Despesas"]] %>%
        summarise(across(everything(), ~ inherits(.x, "numeric"))) %>% 
        unlist() %>% 
        which(),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "#,##0.00"
        )
    )
    # Formatar colunas do tipo inteiro
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 2:nrow(dados.pasta.informakon_l[["Despesas"]]) + 1,
      cols = 
        dados.pasta.informakon_l[["Despesas"]] %>%
        summarise(across(everything(), ~ inherits(.x, "integer"))) %>% 
        unlist() %>% 
        which(),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "#,##0"
        )
    )
    # Formatar colunas do tipo texto
    addStyle(
      xlsx,
      sheet = "Despesas",
      rows = 2:nrow(dados.pasta.informakon_l[["Despesas"]]) + 1,
      cols = 
        dados.pasta.informakon_l[["Despesas"]] %>%
        summarise(across(everything(), ~ inherits(.x, "character"))) %>% 
        unlist() %>% 
        which(),
      gridExpand = T,
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "left",
          valign = "center"
        )
    )
    nome.xlsx_c <-
      paste0(
        "informakon/Informakon ",
        Sys.time() %>% str_sub(1,-10),
        ".xlsx"
      ) %>% 
      str_remove_all("\\:")
    saveWorkbook(xlsx, nome.xlsx_c, overwrite = T)
    return(dados.pasta.informakon_l)
  }

# Teste -------------------------------------------------------------------

#caminho_arquivo_informakon.c <- 
#  here("informakon", "receitas_informakon_20180101_20250131.xlsx")
# dados.pasta.informakon_l <- extrair_dados_pasta_informakon()
#str(extrair_dados_pasta_informakon())
#View(extrair_dados_pasta_informakon()$Despesas)
#View(extrair_dados_pasta_informakon()$Receitas)

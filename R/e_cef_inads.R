library(openxlsx) # Funções para preencher arquivos .xlsx
library(readxl) # Importação de arquivos em Excel, e.g. read_excel()
library(viridisLite) # Mapeamento de cores

# source(here::here("Controladoria - Documentos", "AmplaR", "R", "e_cef_inad.R"))
source(here::here("Controladoria - Documentos", "AmplaR", "R", "e_cef_inad.R"))

e_cef_inads <-
  function(caminho_pasta_inadimplentes.c = here::here("Controladoria - Documentos", "AmplaR", "dados", "cef", "inadimplentes"),
           xlsx = TRUE) {
    # Formatar todos os arquivos da pasta -------------------------------------

    # Todos os arquivos na pasta "inadimplentes"
    arquivos.caminhos.todos_vc <-
      list.files(caminho_pasta_inadimplentes.c, full.names = T)
    # Arquivos a serem desconsiderados
    arquivos.caminhos.excluir_vc <-
      # Pasta "formatados"
      here::here("Controladoria - Documentos", "AmplaR", "dados", "cef", "inadimplentes", "formatados")
    # Arquivos a serem analisados
    arquivos.caminhos.analisar_vc <-
      setdiff(arquivos.caminhos.todos_vc, arquivos.caminhos.excluir_vc)
    # Extrair todos os dados dos arquivos relevantes da pasta "inadimplentes"
    dados.pasta_df <-
      arquivos.caminhos.analisar_vc %>%
      map(
        ~ {
          dados.pasta_df <-
            tryCatch(
              e_cef_inad(.x),
              error = function(e) {
                warning(paste0("Erro ao extrair ", .x, ":", e$message))
                NULL
              }
            )
          if (!is.null(dados.pasta_df)) {
            message(.x, " extraído com sucesso.")
          }
          dados.pasta_df
        }
      ) %>%
      bind_rows(.)

    # Salvando num xlsx -------------------------------------------------------

    if (xlsx == TRUE) {
      # Definindo o nome do arquivo dinamicamente
      nome.xlsx_c <-
        paste0(
          "inadimplentes ",
          format(Sys.time(), "%Y_%m_%d %H_%M_%S"),
          ".xlsx"
        )
      # Criando uma cópia de "Template.xlsx"
      file.copy(
        here::here("Controladoria - Documentos", "AmplaR", "dados", "cef", "inadimplentes", "formatados", "Template.xlsx"),
        # here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c),
        paste0("C:/Users/Ampla/Documents/", nome.xlsx_c),
        overwrite = T
      )
      # Definir a cópia criada como o workbook ativo
      xlsx <-
        loadWorkbook(
          paste0("C:/Users/Ampla/Documents/", nome.xlsx_c)
          # here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c)
        )
      deleteNamedRegion(xlsx, name = "parcelas")
      # Preenchendo os dados da aba "Parcelas"
      writeData(
        xlsx,
        sheet = "Parcelas",
        dados.pasta_df
      )
      # Nomear os dados na aba "Parcelas"
      createNamedRegion(
        xlsx,
        sheet = "Parcelas",
        rows = 1:(nrow(dados.pasta_df) + 1),
        cols = 1:ncol(dados.pasta_df),
        name = "parcelas"
      )
      # Formatação geral da tabela
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            halign = "center",
            valign = "center"
          ),
        rows = 1:(nrow(dados.pasta_df) + 1),
        cols = 1:ncol(dados.pasta_df),
        gridExpand = T
      )
      # Formatar largura das colunas da tabela
      setColWidths(
        xlsx,
        sheet = "Parcelas",
        cols = 1:ncol(dados.pasta_df),
        widths = 18
      )
      # Adicionar filtro à tabela
      addFilter(
        xlsx,
        sheet = "Parcelas",
        rows = 1,
        cols = 1:ncol(dados.pasta_df)
      )
      # Formatar cabeçalho
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            fontSize = 12,
            halign = "center",
            valign = "center",
            textDecoration = "bold",
            fgFill = "darkgray",
            wrapText = T
          ),
        rows = 1,
        cols = 1:ncol(dados.pasta_df),
        gridExpand = T
      )
      # Formatar as colunas "Cliente" e "Unidade"
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            halign = "left",
            valign = "center",
            wrapText = T
          ),
        rows = 2:(nrow(dados.pasta_df) + 1),
        cols = which(colnames(dados.pasta_df) %in% c("Cliente", "Unidade")),
        gridExpand = T
      )
      # Formatar a coluna "Vencto" como data
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            halign = "center",
            valign = "center",
            numFmt = "DD/MM/YYYY"
          ),
        rows = 2:(nrow(dados.pasta_df) + 1),
        cols = which(colnames(dados.pasta_df) == "Vencto"),
        gridExpand = T
      )
      # Formatar a coluna "Data da consulta" como uma data com horário
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            halign = "center",
            valign = "center",
            numFmt = "YYYY-MM-DD HH:MM:SS"
          ),
        rows = 2:(nrow(dados.pasta_df) + 1),
        cols = which(colnames(dados.pasta_df) == "Data da consulta"),
        gridExpand = T
      )
      # Formatar colunas com valores monetários
      addStyle(
        xlsx,
        sheet = "Parcelas",
        style =
          createStyle(
            border = "TopBottomLeftRight",
            halign = "center",
            valign = "center",
            numFmt = "#,##0.00"
          ),
        rows = 1:nrow(dados.pasta_df) + 1,
        cols =
          which(
            colnames(dados.pasta_df) %in%
              c(
                "Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
                "Seguro", "Total"
              )
          ),
        gridExpand = T
      )
      # Congelar a primeira linha
      freezePane(xlsx, sheet = "Parcelas", firstRow = T, firstActiveRow = 2)
      # Salvar a planilha localmente
      saveWorkbook(
        xlsx,
        paste0("C:/Users/Ampla/Documents/", nome.xlsx_c),
        overwrite = T
      )
      # Caminho da planilha na pasta local
      caminho.xlsx_c <-
        paste0("C:/Users/Ampla/Documents/", nome.xlsx_c) %>%
        normalizePath(winslash = "/", mustWork = F)
      # Comando no PowerShell para clicar em "Atualizar tudo" na planilha
      ps_cmd <-
        paste0(
          "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8;",
          "$excel = New-Object -ComObject Excel.Application;",
          "Start-Sleep -Seconds 2;",
          # Repare que o caminho está entre aspas simples
          "$wb = $excel.Workbooks.Open('", caminho.xlsx_c, "');",
          "$wb.RefreshAll();",
          "Start-Sleep -Seconds 3;",
          "$wb.Save();",
          "$wb.Close();",
          "$excel.Quit();",
          "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null;",
          "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null;"
        )
      # Executar o comando do PowerShell pelo R
      system2("powershell", args = c("-Command", ps_cmd))
      # Movendo a planilha da pasta local para o OneDrive
      file.rename(
        caminho.xlsx_c,
        here::here("Controladoria - Documentos", "AmplaR", "dados", "cef", "inadimplentes", "formatados", nome.xlsx_c)
      )
    }
    return(dados.pasta_df)
  }


# Teste -------------------------------------------------------------------

# caminho_arquivo_inadimplentes.c <-
#  here::here("dados", "cef", "inadimplentes", "inads. pomp.xlsx")
# str(extrair_dados_inadimplentes(caminho_arquivo_inadimplentes.c))
# teste=e_cef_inads(xlsx=F)

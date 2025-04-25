#' @title Cruzamento de dados dos extratos da CEF e relatórios CMF_CN
#'
#' @description
#' A função **t_cruzar_extcef_cmfcn** realiza o cruzamento dos dados dos
#' extratos da CEF e dos relatórios CMF_CN, gerando um arquivo consolidado
#' em formato `.xlsx`.
#'
#' @param f_caminho.pasta.extratos_c Caminho para a pasta "Relatorios - Extratos".
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'
#' @details
#' A função executa as seguintes etapas:
#' 1. Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos".
#' 2. Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB".
#' 3. Cruza os dados consolidados com base em campos comuns.
#' 4. Gera um arquivo `.xlsx` com os dados cruzados na pasta "Extratos conciliados".
#'
#' @return
#' Retorna um tibble com os dados cruzados dos extratos da CEF e relatórios CMF_CN.
#'
#' @examples
#' \dontrun{
#' f_caminho.pasta.extratos_c <- "caminho/para/a/pasta/Relatorios - Extratos"
#' f_caminho.pasta.ciweb_c <- "caminho/para/a/pasta/Relatorios - CIWEB"
#' resultado <- t_cruzar_extcef_cmfcn(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c)
#' print(resultado)
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom here here
#' @importFrom magrittr %>%
#' @importFrom openxlsx createWorkbook addWorksheet writeData addStyle
#' @importFrom openxlsx setColWidths addFilter freezePane saveWorkbook
#' @importFrom pdftools pdf_text
#' @importFrom dplyr mutate rename bind_rows inner_join
#' @importFrom stringr str_detect str_ends str_pad str_sub
#'
#' @export

t_cruzar_extcef_cmfcn <-
  function(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c) {
    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    extratos_t <- e_cef_extratos()
    # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
    cmfcns_t <- e_cef_cmfcns()
    # Cruza os dados consolidados
    extratos.cruzados_t <-
      inner_join(
        extratos_t,
        cmfcns_t,
        by = c("Data de movimento", "Contrato_6", "Valor")
      ) %>%
      select(
        # Interseção
        Contrato_6, `Data de movimento`, Valor,
        # Extratos
        "Data de lan\u00e7amento", Documento, "Hist\u00f3rico", Saldo,
        Conta_interno, Conta, "Ag\u00eancia",
        Produto, CNPJ, Cliente, "Per\u00edodo_in\u00edcio", "Per\u00edodo_fim",
        `Data_consulta`,
        # CMF_CNs
        CONTRATO, `DT. LANCTO`, `LANCAMENTOS`, NP, `CONTA SIDEC/NSGD`,
        SITUACAO, `MOT.`
      ) %>%
      mutate(
        id_extcef = paste0(
          # Interseção
          Contrato_6, `Data de movimento`, Valor,
          # Extratos
          "Data de lan\u00e7amento", Documento, "Hist\u00f3rico", Saldo
        ),
        id_cmfcn = paste0(
          # Interseção
          Contrato_6, `Data de movimento`, Valor,
          # CMF_CNs
          `CONTRATO`, `DT. LANCTO`, `LANCAMENTOS`, NP, `CONTA SIDEC/NSGD`,
          SITUACAO, `MOT.`
        )
      )
    # Colunas que identificam linhas cruzadas em extratos_t e cmfcns_t
    extratos_t %<>% mutate(
      cruzada = if_else(
        paste0(
          # Interseção
          Contrato_6, `Data de movimento`, Valor,
          # Extratos
          "Data de lan\u00e7amento", Documento, "Hist\u00f3rico", Saldo
        ) %in% extratos.cruzados_t$id_extcef,
        "sim",
        "não"
      )
    )
    cmfcns_t %<>% mutate(
      cruzada = if_else(
        paste0(
          # Interseção
          Contrato_6, `Data de movimento`, Valor,
          # CMF_CNs
          `CONTRATO`, `DT. LANCTO`, `LANCAMENTOS`, NP, `CONTA SIDEC/NSGD`,
          SITUACAO, `MOT.`
        ) %in% extratos.cruzados_t$id_cmfcn,
        "sim",
        "não"
      )
    )
    extratos.cruzados_t %<>% select(-id_extcef, -id_cmfcn)

    # Salvando num xlsx -------------------------------------------------------

    # Definindo o nome do arquivo dinamicamente
    nome.xlsx_c <-
      paste0(
        "extratos_cruzados-",
        format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
        ".xlsx"
      )
    #  # Criando uma cópia de "Template.xlsx"
    #  file.copy(
    #    here::here("dados", "cef", "inadimplentes", "formatados", "Template.xlsx"),
    #    #here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c),
    #    paste0("C:/Users/Ampla/Documents/", nome.xlsx_c),
    #    overwrite = T
    #  )
    # Definir o workbook ativo
    xlsx <-
      createWorkbook()
    # Aba "Cruzados"
    addWorksheet(
      xlsx,
      sheetName = "Cruzados",
      gridLines = FALSE,
      tabColour = "purple"
    )
    # Popular a aba "Cruzados"
    writeData(
      xlsx,
      sheet = "Cruzados",
      extratos.cruzados_t
    )
    # Formatação geral da tabela
    addStyle(
      xlsx,
      sheet = "Cruzados",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center"
        ),
      rows = 1:(nrow(extratos.cruzados_t) + 1),
      cols = 1:ncol(extratos.cruzados_t),
      gridExpand = T
    )
    # Formatar largura das colunas da tabela
    setColWidths(
      xlsx,
      sheet = "Cruzados",
      cols = 1:ncol(extratos.cruzados_t),
      widths = 18
    )
    # Adicionar filtro à tabela
    addFilter(
      xlsx,
      sheet = "Cruzados",
      rows = 1,
      cols = 1:ncol(extratos.cruzados_t)
    )
    # Formatar cabeçalho
    addStyle(
      xlsx,
      sheet = "Cruzados",
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
      cols = 1:ncol(extratos.cruzados_t),
      gridExpand = T
    )
    # Formatar "Cliente", "LANCAMENTOS" e "CONTA SIDEC/NSGD"
    addStyle(
      xlsx,
      sheet = "Cruzados",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "left",
          valign = "center"
        ),
      rows = 2:(nrow(extratos.cruzados_t) + 1),
      cols =
        which(
          colnames(extratos.cruzados_t) %in%
            c("Cliente", "LANCAMENTOS", "CONTA SIDEC/NSGD")
        ),
      gridExpand = T
    )
    setColWidths(
      xlsx,
      sheet = "Cruzados",
      cols = which(colnames(extratos.cruzados_t) == "CONTA SIDEC/NSGD"),
      widths = 25
    )
    setColWidths(
      xlsx,
      sheet = "Cruzados",
      cols =
        which(
          colnames(extratos.cruzados_t) %in%
            c("Cliente", "LANCAMENTOS")
        ),
      widths = 45
    )
    # Formatar datas
    addStyle(
      xlsx,
      sheet = "Cruzados",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "DD/MM/YYYY"
        ),
      rows = 2:(nrow(extratos.cruzados_t) + 1),
      cols =
        which(
          colnames(extratos.cruzados_t) %in%
            c(
              "Data de lan\u00e7amento", "Data de movimento", "Periodoin\u00edcio",
              "Periodoin\u00edciofim", "DT. LANCTO"
            )
        ),
      gridExpand = T
    )
    # Formatar data-hora
    addStyle(
      xlsx,
      sheet = "Cruzados",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "YYYY-MM-DD HH:MM"
        ),
      rows = 2:(nrow(extratos.cruzados_t) + 1),
      cols = which(colnames(extratos.cruzados_t) == "Data_consulta"),
      gridExpand = T
    )
    # Formatar valores monetários
    addStyle(
      xlsx,
      sheet = "Cruzados",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "#,##0.00"
        ),
      rows = 1:nrow(extratos.cruzados_t) + 1,
      cols =
        which(
          colnames(extratos.cruzados_t) %in%
            c("Valor", "Saldo")
        ),
      gridExpand = T
    )
    # Congelar a primeira linha
    freezePane(xlsx, sheet = "Cruzados", firstRow = T, firstActiveRow = 2)
    # Aba "Extratos"
    addWorksheet(
      xlsx,
      sheetName = "Extratos",
      gridLines = FALSE,
      tabColour = "red"
    )
    # Popular a aba "Extratos"
    writeData(
      xlsx,
      sheet = "Extratos",
      extratos_t
    )
    # Formatação geral da tabela
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center"
        ),
      rows = 1:(nrow(extratos_t) + 1),
      cols = 1:ncol(extratos_t),
      gridExpand = T
    )
    # Formatar largura das colunas da tabela
    setColWidths(
      xlsx,
      sheet = "Extratos",
      cols = 1:ncol(extratos_t),
      widths = 18
    )
    # Adicionar filtro à tabela
    addFilter(
      xlsx,
      sheet = "Extratos",
      rows = 1,
      cols = 1:ncol(extratos_t)
    )
    # Formatar cabeçalho
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          fontSize = 12,
          halign = "center",
          valign = "center",
          textDecoration = "bold",
          fgFill = "red",
          wrapText = T
        ),
      rows = 1,
      cols = 1:ncol(extratos_t),
      gridExpand = T
    )
    # Formatar "Cliente"
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "left",
          valign = "center"
        ),
      rows = 2:(nrow(extratos_t) + 1),
      cols =
        which(
          colnames(extratos_t) %in%
            c("Cliente")
        ),
      gridExpand = T
    )
    setColWidths(
      xlsx,
      sheet = "Extratos",
      cols = which(colnames(extratos_t) == "Hist\u00f3rico"),
      widths = 25
    )
    setColWidths(
      xlsx,
      sheet = "Extratos",
      cols =
        which(
          colnames(extratos_t) %in%
            c("Cliente")
        ),
      widths = 45
    )
    # Formatar datas
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "DD/MM/YYYY"
        ),
      rows = 2:(nrow(extratos_t) + 1),
      cols =
        which(
          colnames(extratos_t) %in%
            c(
              "Data de lan\u00e7amento", "Data de movimento", "Periodoin\u00edcio",
              "Periodoin\u00edciofim"
            )
        ),
      gridExpand = TRUE
    )
    # Formatar data-hora
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "YYYY-MM-DD HH:MM"
        ),
      rows = 2:(nrow(extratos_t) + 1),
      cols = which(colnames(extratos_t) == "Data_consulta"),
      gridExpand = T
    )
    # Formatar valores monetários
    addStyle(
      xlsx,
      sheet = "Extratos",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "#,##0.00"
        ),
      rows = 1:nrow(extratos_t) + 1,
      cols =
        which(
          colnames(extratos_t) %in%
            c("Valor", "Saldo")
        ),
      gridExpand = T
    )
    # Congelar a primeira linha
    freezePane(xlsx, sheet = "Extratos", firstRow = T, firstActiveRow = 2)
    # Aba "CMF_CNs"
    addWorksheet(
      xlsx,
      sheetName = "CMF_CNs",
      gridLines = FALSE,
      tabColour = "blue"
    )
    # Popular a aba "CMF_CNs"
    writeData(
      xlsx,
      sheet = "CMF_CNs",
      cmfcns_t
    )
    # Formatação geral da tabela
    addStyle(
      xlsx,
      sheet = "CMF_CNs",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center"
        ),
      rows = 1:(nrow(cmfcns_t) + 1),
      cols = 1:ncol(cmfcns_t),
      gridExpand = T
    )
    # Formatar largura das colunas da tabela
    setColWidths(
      xlsx,
      sheet = "CMF_CNs",
      cols = 1:ncol(cmfcns_t),
      widths = 18
    )
    # Adicionar filtro à tabela
    addFilter(
      xlsx,
      sheet = "CMF_CNs",
      rows = 1,
      cols = 1:ncol(cmfcns_t)
    )
    # Formatar cabeçalho
    addStyle(
      xlsx,
      sheet = "CMF_CNs",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          fontSize = 12,
          halign = "center",
          valign = "center",
          textDecoration = "bold",
          fgFill = "blue",
          wrapText = T
        ),
      rows = 1,
      cols = 1:ncol(cmfcns_t),
      gridExpand = T
    )
    # Formatar "LANCAMENTOS" e "CONTA SIDEC/NSGD"
    addStyle(
      xlsx,
      sheet = "CMF_CNs",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "left",
          valign = "center"
        ),
      rows = 2:(nrow(cmfcns_t) + 1),
      cols =
        which(
          colnames(cmfcns_t) %in%
            c("LANCAMENTOS", "CONTA SIDEC/NSGD")
        ),
      gridExpand = T
    )
    setColWidths(
      xlsx,
      sheet = "CMF_CNs",
      cols = which(colnames(cmfcns_t) == "CONTA SIDEC/NSGD"),
      widths = 25
    )
    setColWidths(
      xlsx,
      sheet = "CMF_CNs",
      cols =
        which(
          colnames(cmfcns_t) %in%
            c("LANCAMENTOS")
        ),
      widths = 45
    )
    # Formatar datas
    addStyle(
      xlsx,
      sheet = "CMF_CNs",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "DD/MM/YYYY"
        ),
      rows = 2:(nrow(cmfcns_t) + 1),
      cols =
        which(
          colnames(cmfcns_t) %in%
            c("DT. LANCTO", "Data de movimento")
        ),
      gridExpand = TRUE
    )
    # Formatar valores monetários
    addStyle(
      xlsx,
      sheet = "CMF_CNs",
      style =
        createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          numFmt = "#,##0.00"
        ),
      rows = 1:nrow(cmfcns_t) + 1,
      cols =
        which(
          colnames(cmfcns_t) %in%
            c("Valor")
        ),
      gridExpand = T
    )
    # Congelar a primeira linha
    freezePane(xlsx, sheet = "CMF_CNs", firstRow = T, firstActiveRow = 2)
    # Salvar a planilha localmente
    saveWorkbook(
      xlsx,
      paste0(
        c_caminhos_pastas("extratos"),
        "/Extratos cruzados/",
        nome.xlsx_c
      ),
      overwrite = TRUE
    )
    #  # Caminho da planilha na pasta local
    #  caminho.xlsx_c <-
    #    paste0("C:/Users/Ampla/Documents/", nome.xlsx_c) %>%
    #    normalizePath(winslash = "/", mustWork = F)
    #  # Comando no PowerShell para clicar em "Atualizar tudo" na planilha
    #  ps_cmd <-
    #    paste0(
    #      "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8;",
    #      "$excel = New-Object -ComObject Excel.Application;",
    #      "Start-Sleep -Seconds 2;",
    #      # Repare que o caminho está entre aspas simples
    #      "$wb = $excel.Workbooks.Open('", caminho.xlsx_c, "');",
    #      "$wb.RefreshAll();",
    #      "Start-Sleep -Seconds 3;",
    #      "$wb.Save();",
    #      "$wb.Close();",
    #      "$excel.Quit();",
    #      "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null;",
    #      "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null;"
    #    )
    #  # Executar o comando do PowerShell pelo R
    #  system2("powershell", args = c("-Command", ps_cmd))
    #  # Movendo a planilha da pasta local para o OneDrive
    #  file.rename(
    #    caminho.xlsx_c,
    #    here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c)
    #  )
    return(extratos.cruzados_t)
  }

# Teste -------------------------------------------------------------------

# t_cruzar_extcef_cmfcn()
# f_caminho.arquivo.extrato_cef_c <-
#   here::here(
#     "..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#     "Estação", "Fevereiro 2025", "CAIXA -  2419 - FEVEREIRO.pdf"
#   )
# f_caminho.arquivo.extrato_cef_c <-
#   here::here(
#     "..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#     "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#   )
# View(extrair_dados_arquivo_extrato_cef(caminhos.extratos.cef_c[2])$Dados)
# extrato <- extrair_dados_arquivo_extrato_cef(f_caminho.arquivo.extrato_cef_c)
# teste <- extrair_dados_arquivo_extrato_cef(f_caminho.arquivo.extrato_cef_c)
# shell.exec(f_caminho.arquivo.extrato_cef_c)

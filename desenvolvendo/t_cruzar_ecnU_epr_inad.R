# Descrição ---------------------------------------------------------------

### RESUMO ###

# cruzar_inadimplentes_ecn():
# (1) Consolida os dados dos inadimplentes
# (2) Consolida os dados dos relatórios ECN mais recentes
# (3) Cruza esses dados consolidados
# (4) Gera um xlsx com os dados cruzados na pasta "inadimplentes/formatados"

### UTILIZAÇÃO ###

# cruzar_inadimplentes_ecn(
#   f_caminho.pasta.inadimplentes_c,
#   f_caminho.pasta.ciweb_c
# )

### ARGUMENTOS ###

# f_caminho.pasta.inadimplentes_c: Caminho da pasta "inadimplentes/formatados"
# f_caminho.pasta.ciweb_c: Caminho da pasta "Relatorios - CIWEB"

# source(
#  here::here(
#    "R",
#    "extrair_dados_pasta_inadimplentes.R"
#  )
# )

cruzar_inadimplentes_repasses <-
  function() {
    # Consolida os dados dos inadimplentes da pasta "inadimplentes"
    inads_t <-
      e_ik_inads(xlsx = FALSE) %>%
      rename(Contrato_Ampla = "Contrato")
    contrs_t <- e_ik_contrs()
    # Cruza inads_t e contrs_t
    inadimplentes.repasses_t <-
      inads_t %>%
      left_join(
        contrs_t,
        by = c("Contrato_Ampla", "Empreendimento")
      ) %>%
      mutate(
        Repassado = if_else(Repassado == "Repassado", "Sim", "Não")
      ) %>%
      distinct()
    ecns_t <-
      e_cef_ecns()$Unidades %>%
      mutate(CONTRATO_12 = Contrato %>% str_sub(1, -3))
    eprs_t <-
      e_cef_eprs() %>%
      rename(
        CONTRATO_12 = "CONTRATO",
        `Data de Assinatura` = "DT. ASSIN",
        `Data de Inclusão` = "DT. INC. CTR",
        `Data de Registro` = "DT. INC. REG"
      )
    ecn.epr_t <-
      full_join(
        ecns_t,
        eprs_t,
        by = c("CONTRATO_12", "Data de Assinatura", "Data de Inclusão")
      ) %>%
      rename(Cliente = "NOME MUTUARIO")
    inadimplentes.ecn.epr_t <-
      stringdist_inner_join(
        inads_t,
        ecn.epr_t,
        by = "Cliente",
        method = "jw",
        distance_col = "dist"
      ) %>%
      select(Cliente.x, Cliente.y, dist) %>%
      distinct() %>%
      filter(dist < 0.3) %>%
      arrange(Cliente.x, dist) %>%
      distinct(Cliente.x, .keep_all = TRUE)

    # Salvando num xlsx -------------------------------------------------------

    # Definindo o nome do arquivo dinamicamente
    nome.xlsx_c <-
      paste0(
        "Extratos cruzados ",
        format(Sys.time(), "%Y_%m_%d %H_%M_%S"),
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
              "Data de lançamento", "Data de movimento", "Período_início",
              "Período_fim", "DT. LANCTO"
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
      cols = which(colnames(extratos_t) == "Histórico"),
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
              "Data de lançamento", "Data de movimento", "Período_início",
              "Período_fim"
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
        here::here(
          dirname(dirname(here())), "Relatórios - Documentos",
          "Relatorios - Extratos", "Extratos conciliados"
        ),
        "/",
        nome.xlsx_c
      ),
      overwrite = T
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

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

r_inad <-
  function() {
    # Consolida os dados dos inadimplentes da pasta "inadimplentes"
    inads_t <-
      e_ik_inads(xlsx = FALSE) %>%
      rename(Contrato_Ampla = "Contrato")
    caminho.inads_c <-
      dir_ls(c_caminhos_pastas("cobranca"), recurse = TRUE, type = "file") %>%
      keep(
        ~ str_detect(.x, "(?i)inadimpl.ncia-.*\\.xlsx") &
          !str_detect(.x, "(?i)consolidado")
      )
    empreendimentos_c <- unique(
      inads_t$Empreendimento
    )
    caminhos.inads_t <- caminho.inads_c %>%
      map_dfr(~ {
        # Extrair a data do nome do arquivo no formato %Y_%m (ex: 2025_04)
        data_str <- str_extract(.x, "-\\d{4}_\\d{2}") %>%
          str_remove("-")
        data <- suppressWarnings(as.Date(paste0(data_str, "_01"), format = "%Y_%m_%d"))
        tibble(caminho = .x, data = data, nome = fs::path_file(.x))
      }) %>%
      arrange(desc(data)) %>%
      distinct(nome, .keep_all = TRUE)
    caminhos.inads.recentes_c <- caminhos.inads_t$caminho
    contrs_t <- e_ik_contrs()
    # Cruza inads_t e contrs_t
    r_inad.parcelas_t <-
      inads_t %>%
      left_join(
        contrs_t %>%
          select(-c(
            "Arquivo_tipo_tabela", "Arquivo_tipo", "Arquivo_fonte", "Cliente",
            "Esp"
          )),
        by = c("Contrato_Ampla", "Empreendimento")
      ) %>%
      mutate(
        Repassado = if_else(Repassado == "Repassado", "Sim", "Não")
      ) %>%
      select(
        Empreendimento, Contrato_Ampla, Repassado, Contrato_CEF,
        Unidade, Cliente, Telefone, everything()
      ) %>%
      distinct()
    r_inad.clientes_t <-
      r_inad.parcelas_t %>%
      group_by(Cliente) %>%
      summarise(
        Total = sum(Total, na.rm = TRUE),
        Atraso_meses = max(Atraso, na.rm = TRUE) / 30,
        Empreendimento = first(Empreendimento),
        Repassado = first(Repassado)
      ) %>%
      ungroup()
    r_inad_l <- list(
      r_inad.parcelas_t = r_inad.parcelas_t,
      r_inad.clientes_t = r_inad.clientes_t
    )
    # ecns_t <-
    #   e_cef_ecns()$Unidades %>%
    #   mutate(CONTRATO_12 = Contrato %>% str_sub(1, -3))
    # eprs_t <-
    #   e_cef_eprs() %>%
    #   rename(
    #     CONTRATO_12 = "CONTRATO",
    #     `Data de Assinatura` = "DT. ASSIN",
    #     `Data de Inclusão` = "DT. INC. CTR",
    #     `Data de Registro` = "DT. INC. REG"
    #   )
    # ecn.epr_t <-
    #   full_join(
    #     ecns_t,
    #     eprs_t,
    #     by = c("CONTRATO_12", "Data de Assinatura", "Data de Inclusão")
    #   ) %>%
    #   rename(Cliente = "NOME MUTARIO")

    # Salvando num xlsx -------------------------------------------------------

    # Definindo o nome do arquivo dinamicamente
    nome.xlsx_c <-
      str_c(
        "Inadimplencia-",
        format(Sys.time(), "%Y_%m_%d-%H_%M_%S"),
        ".xlsx"
      )
    # Criando uma cópia de "Template.xlsx"
    file.copy(
      str_c(c_caminhos_pastas("github"), "/templates/Template-Inadimplencia.xlsx"),
      str_c(c_caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c)
    )
    # Definir a cópia criada como o workbook ativo
    xlsx <-
      loadWorkbook(
        str_c(c_caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c),
      )
    deleteNamedRegion(xlsx, name = "parcelas")
    # Preenchendo os dados da aba "Parcelas"
    writeData(
      xlsx,
      sheet = "Parcelas",
      r_inad.parcelas_t
    )
    # Nomear os dados na aba "Parcelas"
    createNamedRegion(
      xlsx,
      sheet = "Parcelas",
      rows = 1:(nrow(r_inad.parcelas_t) + 1),
      cols = 1:ncol(r_inad.parcelas_t),
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
      rows = 1:(nrow(r_inad.parcelas_t) + 1),
      cols = 1:ncol(r_inad.parcelas_t),
      gridExpand = T
    )
    # Formatar largura das colunas da tabela
    setColWidths(
      xlsx,
      sheet = "Parcelas",
      cols = 1:ncol(r_inad.parcelas_t),
      widths = 18
    )
    # Adicionar filtro à tabela
    addFilter(
      xlsx,
      sheet = "Parcelas",
      rows = 1,
      cols = 1:ncol(r_inad.parcelas_t)
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
      cols = 1:ncol(r_inad.parcelas_t),
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
      rows = 2:(nrow(r_inad.parcelas_t) + 1),
      cols = which(colnames(r_inad.parcelas_t) %in% c("Cliente", "Unidade")),
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
      rows = 2:(nrow(r_inad.parcelas_t) + 1),
      cols = which(colnames(r_inad.parcelas_t) == "Vencto"),
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
      rows = 2:(nrow(r_inad.parcelas_t) + 1),
      cols = which(colnames(r_inad.parcelas_t) == "Data da consulta"),
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
      rows = 1:nrow(r_inad.parcelas_t) + 1,
      cols =
        which(
          colnames(r_inad.parcelas_t) %in%
            c(
              "Principal", "Juros", "Encargos", "Juros de Mora", "Multa",
              "Seguro", "Total"
            )
        ),
      gridExpand = T
    )
    # Congelar a primeira linha
    freezePane(xlsx, sheet = "Parcelas", firstRow = T, firstActiveRow = 2)
    deleteNamedRegion(xlsx, name = "clientes")
    # Preenchendo os dados da aba "Clientes"
    writeData(
      xlsx,
      sheet = "Clientes",
      r_inad.clientes_t
    )
    # Nomear os dados na aba "Clientes"
    createNamedRegion(
      xlsx,
      sheet = "Clientes",
      rows = 1:(nrow(r_inad.clientes_t) + 1),
      cols = 1:ncol(r_inad.clientes_t),
      name = "clientes"
    )
    # Salvar a planilha localmente
    saveWorkbook(
      xlsx,
      str_c(c_caminhos_pastas("temp"), "/", nome.xlsx_c),
      overwrite = TRUE
    )
    # Caminho da planilha na pasta local
    caminho.xlsx_c <-
      str_c(c_caminhos_pastas("temp"), "/", nome.xlsx_c) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
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
      str_c(c_caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c)
    )
    if (nrow(caminhos.inads_t) > 0) {
      meses <- format(caminhos.inads_t$data, "%Y-%m")
      if (length(unique(meses)) == 1) {
        message("\u2705 Os relatórios mais recentes de inadimplência de todos os empreendimentos são do mês ", unique(meses))
      } else {
        msg <- paste0(
          "\u274C Os relatórios mais recentes de inadimplência são de meses diferentes entre os empreendimentos:\n",
          capture.output(print(caminhos.inads_t[, c("caminho", "data")], row.names = FALSE)) %>%
            paste(collapse = "\n")
        )
        message(msg)
      }
    }
    # Filtrar contrs_t para apenas o arquivo mais recente por empreendimento
    caminho.contrs_c <-
      dir_ls(c_caminhos_pastas("cobranca"), recurse = TRUE, type = "file") %>%
      keep(~ str_detect(.x, "(?i)contratos-.*\\.xlsx") & !str_detect(.x, "(?i)consolidado"))
    caminhos.contrs_t <- caminho.contrs_c %>%
      map_dfr(~ {
        data_str <- str_extract(.x, "-\\d{4}_\\d{2}") %>% str_remove("-")
        data <- suppressWarnings(as.Date(paste0(data_str, "_01"), format = "%Y_%m_%d"))
        tibble(caminho = .x, data = data, nome = fs::path_file(.x))
      }) %>%
      arrange(desc(data)) %>%
      distinct(nome, .keep_all = TRUE)
    caminhos.contrs.recentes_c <- caminhos.contrs_t$caminho
    # Mensagem de verificação para contratos
    if (nrow(caminhos.contrs_t) > 0) {
      meses_contrs <- format(caminhos.contrs_t$data, "%Y-%m")
      if (length(unique(meses_contrs)) == 1) {
        message("\u2705 Os contratos mais recentes de todos os empreendimentos são do mês ", unique(meses_contrs))
      } else {
        msg_contrs <- paste0(
          "\u274C Os contratos mais recentes são de meses diferentes entre os empreendimentos:\n",
          capture.output(print(caminhos.contrs_t[, c("caminho", "data")], row.names = FALSE)) %>%
            paste(collapse = "\n")
        )
        message(msg_contrs)
      }
    }
    return(r_inad_l)
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

r_inad <- function() {
  # inad -----------------------------------------------------------------------

  # Consolida os dados dos arquivos do tipo inad
  inads_t <- e_ik_inads(xlsx = FALSE)
  # Tabela com todos os caminhos dos arquivos do tipo inad
  caminhos.inads_t <- e_metadados("inad")
  # Tabela com os caminhos dos arquivos mais recentes do tipo inad
  caminhos.inads.recentes_t <- caminhos.inads_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # contr ----------------------------------------------------------------------

  # Consolida os dados dos arquivos do tipo contr
  contrs_t <- e_ik_contrs()
  # Filtrar contrs_t para apenas o arquivo mais recente por empreendimento
  caminhos.contrs_t <- e_metadados("contr")
  caminhos.contrs.recentes_t <- caminhos.contrs_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # join --------------------------------------------------------------------

  # Cruza inads_t e contrs_t
  r_inad.parcelas_t <-
    inads_t %>%
    dplyr::filter(arquivo %in% caminhos.inads.recentes_t$caminho) %>%
    left_join(
      contrs_t %>%
        dplyr::filter(arquivo %in% caminhos.contrs.recentes_t$caminho) %>%
        select(-c(
          "arquivo.tabela.tipo", "arquivo.tipo", "arquivo.fonte", "cliente",
          "esp"
        )),
      by = c("contrato.ampla", "empreendimento")
    ) %>%
    mutate(
      repassado = if_else(repassado == "repassado", "Sim", "Não")
    ) %>%
    dplyr::select(
      empreendimento, cliente, repassado, contrato.cef,
      unidade, empreendimento, telefone, everything()
    ) %>%
    distinct()
  r_inad.clientes_t <-
    r_inad.parcelas_t %>%
    group_by(cliente) %>%
    summarise(
      total = sum(total, na.rm = TRUE),
      atraso.meses = round(max(atraso, na.rm = TRUE) / 30, 1),
      empreendimento = first(empreendimento),
      repassado = first(repassado)
    ) %>%
    ungroup()
  r_inad_l <- list(
    r_inad.parcelas_t = r_inad.parcelas_t,
    r_inad.clientes_t = r_inad.clientes_t
  )

  # xlsx -----------------------------------------------------------------------

  # Definindo o nome do arquivo dinamicamente
  nome.xlsx_c <-
    str_c(
      "Inadimplencia-",
      format(Sys.time(), "%Y_%m_%d-%H_%M_%S"),
      ".xlsx"
    )
  # Criando uma cópia de "Template.xlsx"
  file.copy(
    str_c(caminhos_pastas("github"), "/templates/Template-Inadimplencia.xlsx"),
    str_c(caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c)
  )
  # Definir a cópia criada como o workbook ativo
  xlsx <-
    loadWorkbook(
      str_c(caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c),
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
  # Formatar largura das colunas "cliente" e "unidade"
  setColWidths(
    xlsx,
    sheet = "Parcelas",
    cols = which(colnames(r_inad.parcelas_t) %in% c("cliente", "unidade")),
    widths = "auto"
  )
  # Formatar largura de colunas específicas
  setColWidths(
    xlsx,
    sheet = "Parcelas",
    cols = which(colnames(r_inad.parcelas_t) %in% c(
      "repassado", "contrato.cef", "contrato.ampla", "esp", "parcela",
      "quantidade.parcelas", "ele", "vencimento", "atraso", "r/f"
    )),
    widths = c(12, 15, 15, 9, 15, 20, 9, 12, 9, 9)
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
        fontSize = 11,
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
  # Formatar as colunas "cliente" e "unidade"
  addStyle(
    xlsx,
    sheet = "Parcelas",
    style =
      createStyle(
        border = "TopBottomLeftRight",
        halign = "left",
        valign = "center",
        wrapText = FALSE
      ),
    rows = 2:(nrow(r_inad.parcelas_t) + 1),
    cols = which(colnames(r_inad.parcelas_t) %in% c("cliente", "unidade")),
    gridExpand = T
  )
  # Formatar a coluna "vencimento" como data
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
    cols = which(colnames(r_inad.parcelas_t) == "vencimento"),
    gridExpand = T
  )
  # Formatar a coluna "data.consulta" como uma data com horário
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
    cols = which(colnames(r_inad.parcelas_t) == "data.consulta"),
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
            "principal", "juros", "encargos", "juros.mora", "multa",
            "seguro", "total"
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

  # salvar ---------------------------------------------------------------------

  caminho_temporario_c <- withr::local_tempfile(fileext = ".xlsx")

  # Salvar a planilha no arquivo temporário
  saveWorkbook(xlsx, caminho_temporario_c, overwrite = TRUE)

  # Normalizar o caminho para o PowerShell
  caminho_temporario_norm_c <- normalizePath(caminho_temporario_c, winslash = "/", mustWork = FALSE)

  # Comando no PowerShell para clicar em "Atualizar tudo" na planilha
  ps_cmd <-
    paste0(
      "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8;",
      "$excel = New-Object -ComObject Excel.Application;",
      "Start-Sleep -Seconds 2;",
      # O caminho do arquivo é passado entre aspas simples
      "$wb = $excel.Workbooks.Open('", caminho_temporario_norm_c, "');",
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

  # Copiando a planilha da pasta temporária para o destino final no OneDrive
  caminho_final_c <- str_c(caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c)
  file.copy(caminho_temporario_c, caminho_final_c, overwrite = TRUE)

  if (nrow(caminhos.inads.recentes_t) > 0) {
    meses <- format(caminhos.inads.recentes_t$data, "%Y-%m")
    if (length(unique(meses)) == 1) {
      message("\u2705 Os relatórios mais recentes de inadimplência de todos os empreendimentos são do mês ", unique(meses))
    } else {
      msg <- paste0(
        "\u274C Os relatórios mais recentes de inadimplência são de meses diferentes entre os empreendimentos:\n",
        capture.output(print(caminhos.inads.recentes_t[, c("caminho", "data")], row.names = FALSE)) %>%
          paste(collapse = "\n")
      )
      message(msg)
    }
  }
  # Mensagem de verificação para contratos
  if (nrow(caminhos.contrs.recentes_t) > 0) {
    meses_contrs <- format(caminhos.contrs.recentes_t$data, "%Y-%m")
    if (length(unique(meses_contrs)) == 1) {
      message("\u2705 Os contratos mais recentes de todos os empreendimentos são do mês ", unique(meses_contrs))
    } else {
      msg_contrs <- paste0(
        "\u274C Os contratos mais recentes são de meses diferentes entre os empreendimentos:\n",
        capture.output(print(caminhos.contrs.recentes_t[, c("caminho", "data")], row.names = FALSE)) %>%
          paste(collapse = "\n")
      )
      message(msg_contrs)
    }
  }
  return(r_inad_l)
}

# Teste -------------------------------------------------------------------

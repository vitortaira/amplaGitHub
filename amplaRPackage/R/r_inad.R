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
  # Tabela com todos os caminhos dos arquivos do tipo contr
  caminhos.contrs_t <- e_metadados("contr")
  # Tabela com os caminhos dos arquivos mais recentes do tipo contr
  caminhos.contrs.recentes_t <- caminhos.contrs_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # join -----------------------------------------------------------------------

  # Cruza inads_t e contrs_t
  r_inad.parcelas_t <-
    inads_t %>%
    dplyr::filter(arquivo %in% caminhos.inads.recentes_t$caminho) %>%
    left_join(
      contrs_t %>%
        dplyr::filter(arquivo %in% caminhos.contrs.recentes_t$caminho) %>%
        select(-"cliente"),
      by = c("contrato.ampla", "empreendimento"),
      suffix = c(".inad", ".contr")
    ) %>%
    mutate(
      repassado = if_else(repassado %in% c(NA, "Não"), "Não", "Sim")
    ) %>%
    dplyr::select(
      empreendimento, cliente, total, repassado, contrato.ampla, contrato.cef,
      unidade, quantidade.parcelas, parcela, atraso, vencimento, ele,
      principal, juros, encargos, juros.mora, multa, seguro, everything()
    ) %>%
    distinct()
  r_inad.clientes_t <-
    r_inad.parcelas_t %>%
    group_by(cliente) %>%
    summarise(
      total = sum(total, na.rm = TRUE),
      atraso.meses = round(max(atraso, na.rm = TRUE) / 30, 0),
      empreendimento = first(empreendimento),
      repassado = first(repassado)
    ) %>%
    ungroup()

  # Lista nomeada com os dataframes e os nomes das abas correspondentes
  dfs_l <- list(
    "Parcelas" = r_inad.parcelas_t,
    "Clientes" = r_inad.clientes_t
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
      str_c(caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c)
    )

  # Edições comuns a todas as abas
  purrr::walk2(
    .x = dfs_l,
    .y = names(dfs_l),
    .f = function(f_df_t, nome_aba) {
      # Deletar a região nomeada antiga, se existir
      nome_regiao <- tolower(nome_aba)
      if (nome_regiao %in% getNamedRegions(xlsx)) {
        deleteNamedRegion(xlsx, name = nome_regiao)
      }

      # Escrever os dados
      writeData(xlsx, sheet = nome_aba, x = f_df_t)

      # Criar nova região nomeada
      createNamedRegion(
        xlsx,
        sheet = nome_aba,
        name = nome_regiao,
        rows = 1:(nrow(f_df_t) + 1),
        cols = 1:ncol(f_df_t)
      )

      # Estilo geral (bordas e alinhamento)
      addStyle(
        xlsx,
        sheet = nome_aba,
        style = createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center"
        ),
        rows = 1:(nrow(f_df_t) + 1),
        cols = 1:ncol(f_df_t),
        gridExpand = TRUE
      )

      # Estilo do cabeçalho
      addStyle(
        xlsx,
        sheet = nome_aba,
        style = createStyle(
          border = "TopBottomLeftRight",
          fontSize = 11,
          halign = "center",
          valign = "center",
          textDecoration = "bold",
          fgFill = "darkgray",
          wrapText = TRUE
        ),
        rows = 1,
        cols = 1:ncol(f_df_t),
        gridExpand = TRUE
      )

      # Adicionar filtro e congelar painel
      addFilter(xlsx, sheet = nome_aba, rows = 1, cols = 1:ncol(f_df_t))
      freezePane(xlsx, sheet = nome_aba, firstRow = TRUE, firstActiveRow = 2)

      # Formatações de largura de coluna (geral e auto)
      setColWidths(xlsx, sheet = nome_aba, cols = 1:ncol(f_df_t), widths = 18)
      setColWidths(
        xlsx,
        sheet = nome_aba,
        cols = which(colnames(f_df_t) %in% c("cliente", "unidade")),
        widths = "auto"
      )

      # --- Formatações específicas da aba "Parcelas" ---
      if (nome_aba == "Parcelas") {
        # Larguras específicas
        setColWidths(
          xlsx,
          sheet = nome_aba,
          cols = which(colnames(f_df_t) %in% c(
            "repassado", "contrato.cef", "contrato.ampla", "esp",
            "parcela", "quantidade.parcelas", "ele", "vencimento",
            "atraso", "r/f"
          )),
          widths = c(12, 15, 15, 9, 15, 20, 9, 12, 9, 9)
        )

        # Estilo para colunas de texto (alinhamento à esquerda)
        addStyle(
          xlsx,
          sheet = nome_aba,
          style = createStyle(halign = "left", wrapText = FALSE),
          rows = 2:(nrow(f_df_t) + 1),
          cols = which(colnames(f_df_t) %in% c("cliente", "unidade")),
          gridExpand = TRUE,
          stack = TRUE
        )

        # Estilo para data
        addStyle(
          xlsx,
          sheet = nome_aba,
          style = createStyle(numFmt = "DD/MM/YYYY"),
          rows = 2:(nrow(f_df_t) + 1),
          cols = which(colnames(f_df_t) == "vencimento"),
          gridExpand = TRUE,
          stack = TRUE
        )

        # Estilo para data e hora
        addStyle(
          xlsx,
          sheet = nome_aba,
          style = createStyle(numFmt = "YYYY-MM-DD HH:MM:SS"),
          rows = 2:(nrow(f_df_t) + 1),
          cols = which(colnames(f_df_t) == "data.consulta"),
          gridExpand = TRUE,
          stack = TRUE
        )

        # Estilo para valores monetários
        addStyle(
          xlsx,
          sheet = nome_aba,
          style = createStyle(numFmt = "#,##0.00"),
          rows = 2:(nrow(f_df_t) + 1),
          cols = which(colnames(f_df_t) %in% c(
            "principal", "juros", "encargos", "juros.mora",
            "multa", "seguro", "total"
          )),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  )

  # salvar ---------------------------------------------------------------------

  caminho_temporario_c <- withr::local_tempfile(fileext = ".xlsx")

  # Salvar a planilha no arquivo temporário
  saveWorkbook(xlsx, caminho_temporario_c, overwrite = TRUE)

  # Normalizar o caminho para o PowerShell
  caminho_temporario_norm_c <- normalizePath(
    caminho_temporario_c,
    winslash = "/",
    mustWork = FALSE
  )

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
      "[System.Runtime.Interopservices.Marshal]::",
      "ReleaseComObject($wb) | Out-Null;",
      "[System.Runtime.Interopservices.Marshal]::",
      "ReleaseComObject($excel) | Out-Null;"
    )

  # Executar o comando do PowerShell pelo R
  system2("powershell", args = c("-Command", ps_cmd))

  # Copiando a planilha da pasta temporária para o destino final no OneDrive
  caminho_final_c <- str_c(
    caminhos_pastas("cobranca"), "/Consolidados/", nome.xlsx_c
  )
  file.copy(caminho_temporario_c, caminho_final_c, overwrite = TRUE)

  if (nrow(caminhos.inads.recentes_t) > 0) {
    meses <- format(caminhos.inads.recentes_t$data, "%Y-%m")
    if (length(unique(meses)) == 1) {
      message(
        "\u2705 Os relatórios mais recentes de inadimplência de todos ",
        "os empreendimentos são do mês ", unique(meses)
      )
    } else {
      msg <- paste0(
        "\u274C Os relatórios mais recentes de inadimplência são de ",
        "meses diferentes entre os empreendimentos:\n",
        capture.output(print(
          caminhos.inads.recentes_t[, c("caminho", "data")],
          row.names = FALSE
        )) %>%
          paste(collapse = "\n")
      )
      message(msg)
    }
  }
  # Mensagem de verificação para contratos
  if (nrow(caminhos.contrs.recentes_t) > 0) {
    meses_contrs <- format(caminhos.contrs.recentes_t$data, "%Y-%m")
    if (length(unique(meses_contrs)) == 1) {
      message(
        "\u2705 Os contratos mais recentes de todos os empreendimentos ",
        "são do mês ", unique(meses_contrs)
      )
    } else {
      msg_contrs <- paste0(
        "\u274C Os contratos mais recentes são de meses diferentes ",
        "entre os empreendimentos:\n",
        capture.output(print(
          caminhos.contrs.recentes_t[, c("caminho", "data")],
          row.names = FALSE
        )) %>%
          paste(collapse = "\n")
      )
      message(msg_contrs)
    }
  }
  return(dfs_l)
}

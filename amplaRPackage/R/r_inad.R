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
  contrs_t <- e_ik_contrs_inad()
  # Tabela com todos os caminhos dos arquivos do tipo contr
  caminhos.contrs_t <- e_metadados("contr")
  # Tabela com os caminhos dos arquivos mais recentes do tipo contr
  caminhos.contrs.recentes_t <- caminhos.contrs_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # join -----------------------------------------------------------------------

  # Aba "Parcelas"
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
      cliente = stringr::str_to_title(cliente),
      repassado = if_else(repassado %in% c(NA, "Não"), "Não", "Sim")
    ) %>%
    dplyr::select(
      empreendimento, cliente, total, repassado,
      contrato.ampla, contrato.cef, unidade, quantidade.parcelas,
      parcela, atraso, vencimento, ele,
      principal, juros, encargos, juros.mora, multa, seguro,
      everything() # now grab all the other cols (incl. arquivo.*)
    ) %>%
    relocate(starts_with("arquivo"), .after = last_col()) %>%
    distinct() %>%
    arrange(desc(total))
  # Aba "Clientes"
  r_inad.clientes_t <-
    r_inad.parcelas_t %>%
    group_by(cliente) %>%
    summarise(
      total.cliente = sum(total, na.rm = TRUE),
      quantidade.parcelas = first(quantidade.parcelas),
      atraso.medio.ponderado = round(
        sum((atraso / 30) * total, na.rm = TRUE) / sum(total, na.rm = TRUE),
        0
      ),
      atraso.maximo = round(max(atraso, na.rm = TRUE) / 30, 0),
      empreendimento = first(empreendimento),
      repassado = first(repassado)
    ) %>%
    ungroup() %>%
    mutate(
      status = NA_character_,
      anotacoes = NA_character_
    ) %>%
    select(
      empreendimento, cliente, total.cliente, quantidade.parcelas,
      atraso.medio.ponderado, atraso.maximo, repassado, status, anotacoes
    ) %>%
    arrange(desc(total.cliente))

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
    .f = function(f_df_t, f_aba_c) {
      # Deletar a região nomeada antiga, se existir
      nome_regiao <- tolower(f_aba_c)
      if (nome_regiao %in% getNamedRegions(xlsx)) {
        deleteNamedRegion(xlsx, name = nome_regiao)
      }

      # Escrever os dados
      writeData(xlsx, sheet = f_aba_c, x = f_df_t)

      # Criar nova região nomeada
      createNamedRegion(
        xlsx,
        sheet = f_aba_c,
        name = nome_regiao,
        rows = 1:(nrow(f_df_t) + 1),
        cols = 1:ncol(f_df_t)
      )

      # Estilo geral (bordas e alinhamento)
      addStyle(
        xlsx,
        sheet = f_aba_c,
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
        sheet = f_aba_c,
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
      addFilter(xlsx, sheet = f_aba_c, rows = 1, cols = 1:ncol(f_df_t))
      freezePane(xlsx, sheet = f_aba_c, firstRow = TRUE, firstActiveRow = 2)

      # Largura das colunas - geral (aplicada primeiro)
      setColWidths(xlsx, sheet = f_aba_c, cols = 1:ncol(f_df_t), widths = 18)

      # Colunas de texto (alinhamento à esquerda, sem quebra de texto)
      colunas_texto <- which(sapply(f_df_t, function(x) is.character(x) | is.factor(x)))
      if (length(colunas_texto) > 0) {
        addStyle(
          xlsx,
          sheet = f_aba_c,
          style = createStyle(halign = "left", wrapText = FALSE),
          rows = 2:(nrow(f_df_t) + 1),
          cols = colunas_texto,
          gridExpand = TRUE,
          stack = TRUE
        )

        # Largura padrão para colunas de texto (será sobrescrita pelas específicas)
        setColWidths(
          xlsx,
          sheet = f_aba_c,
          cols = colunas_texto,
          widths = 16
        )
      }

      # Estilo para valores monetários
      colunas_monetarias <- which(colnames(f_df_t) %in% c(
        "principal", "juros", "encargos", "juros.mora", "multa", "seguro", "total", "total.cliente"
      ))
      if (length(colunas_monetarias) > 0) {
        addStyle(
          xlsx,
          sheet = f_aba_c,
          style = createStyle(numFmt = "#,##0.00"),
          rows = 2:(nrow(f_df_t) + 1),
          cols = colunas_monetarias,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      # Larguras específicas (aplicadas POR ÚLTIMO para garantir precedência)
      colunas_larguras <- c(
        "alterado.por" = 18,
        "anotacoes" = 60,
        "arquivo.tipo.contr" = 18,
        "arquivo.tabela.tipo.contr" = 24,
        "arquivo.fonte.contr" = 20,
        "arquivo.tipo.inad" = 18,
        "arquivo.tabela.tipo.inad" = 24,
        "arquivo.fonte.inad" = 20,
        "atraso" = 6,
        "atraso.medio.ponderado" = 25,
        "autorizado" = 10,
        "cliente" = 35,
        "contrato.alternativo" = 20,
        "contrato.ampla" = 15,
        "contrato.cef" = 15,
        "cotista" = 9,
        "cpf.cnpj" = 15,
        "criado.por" = 18,
        "data.contrato" = 12,
        "ele" = 6,
        "empreendimento" = 16,
        "esp.contr" = 9,
        "esp.inad" = 9,
        "id.cartao" = 9,
        "identificacao.imovel" = 20,
        "atraso.maximo" = 35,
        "moeda" = 9,
        "parcela" = 10,
        "quantidade.parcelas" = 20,
        "r/f" = 6,
        "repassado" = 12,
        "sit" = 6,
        "status" = 30,
        "tipo.contrato" = 15,
        "unidade" = 55,
        "usuario.autorizacao" = 18,
        "vencimento" = 12
      )

      # Aplica larguras específicas uma por uma para garantir que sejam respeitadas
      for (coluna in names(colunas_larguras)) {
        if (coluna %in% colnames(f_df_t)) {
          col_pos <- which(colnames(f_df_t) == coluna)
          setColWidths(
            xlsx,
            sheet = f_aba_c,
            cols = col_pos,
            widths = colunas_larguras[coluna]
          )
        }
      }

      # Estilo para data
      addStyle(
        xlsx,
        sheet = f_aba_c,
        style = createStyle(numFmt = "DD/MM/YYYY"),
        rows = 2:(nrow(f_df_t) + 1),
        cols = which(colnames(f_df_t) %in% c("vencimento", "data.contrato")),
        gridExpand = TRUE,
        stack = TRUE
      )

      # Estilo para data e hora
      addStyle(
        xlsx,
        sheet = f_aba_c,
        style = createStyle(numFmt = "YYYY-MM-DD HH:MM:SS"),
        rows = 2:(nrow(f_df_t) + 1),
        cols = which(colnames(f_df_t) %in%
          c("data.consulta", "criado.em", "alterado.em", "data.autorizacao")),
        gridExpand = TRUE,
        stack = TRUE
      )
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

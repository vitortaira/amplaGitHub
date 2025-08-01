#' Extrai e consolida dados de extratos CEF e ITAÚ
#'
#' @description
#' Função para extrair e consolidar dados de extratos bancários CEF e ITAÚ,
#' combinando-os em um único tibble padronizado. Opcionalmente pode gerar
#' um mapa de combinações únicas e criar uma planilha Excel.
#'
#' @param xlsx Lógico. Se TRUE, cria um arquivo Excel com o mapa de extratos.
#'   Padrão: FALSE.
#'
#' @return Uma lista contendo:
#'   - extratosConsolidados: tibble com dados consolidados de extratos
#'   - mapaExtratos: tibble com combinações únicas de (empresa, banco, conta, descricao)
#' @importFrom dplyr bind_rows mutate select rename case_when if_else arrange all_of
#' @importFrom dplyr distinct group_by summarise n filter
#' @importFrom stringr str_detect str_pad str_sub str_c
#' @importFrom tibble tibble as_tibble
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook addWorksheet addStyle createStyle
#' @importFrom openxlsx setColWidths addFilter freezePane
#' @export
#'
e_extratos <- function(xlsx = FALSE) {
  # Obter dados CEF
  dadosCef <- tryCatch(
    {
      e_cef_xcefs()
    },
    error = function(e) {
      message("Erro ao extrair dados CEF: ", e$message)
      return(tibble::tibble(
        data.lancamento = as.Date(character()),
        data.movimentacao = as.Date(character()),
        documento = character(),
        descricao = character(),
        valor = numeric(),
        saldo = numeric(),
        conta.interno = character(),
        conta = character(),
        agencia = character(),
        produto = character(),
        cnpj = character(),
        empresa = character(),
        periodo.inicio = as.Date(character()),
        periodo.fim = as.Date(character()),
        data.consulta = as.POSIXct(character()),
        arquivo = character(),
        arquivo.subtipo = character(),
        arquivo.tabela.tipo = character(),
        arquivo.tipo = character(),
        arquivo.fonte = character()
      ))
    }
  )

  # Obter dados ITAÚ
  dadosIta <- tryCatch(
    {
      e_ita_xitas()$xita_l
    },
    error = function(e) {
      message("Erro ao extrair dados ITAÚ: ", e$message)
      return(tibble::tibble(
        data = as.Date(character()),
        valor = numeric(),
        descricao = character(),
        empresa = character(),
        cnpj = character(),
        agencia = character(),
        conta = character(),
        periodo.inicio = as.Date(character()),
        periodo.fim = as.Date(character()),
        data.consulta = as.POSIXct(character()),
        arquivo = character(),
        arquivo.subtipo = character(),
        arquivo.tabela.tipo = character(),
        arquivo.tipo = character(),
        arquivo.fonte = character()
      ))
    }
  )

  # Padronizar estrutura dos dados ITAÚ para compatibilidade com CEF
  dadosItaPadronizados <- dadosIta %>%
    dplyr::mutate(
      data.lancamento = .data$data,
      data.movimentacao = .data$data,
      documento = NA_character_,
      saldo = NA_real_,
      conta.interno = .data$conta,
      produto = NA_character_
    ) %>%
    dplyr::select(
      .data$data.lancamento, .data$data.movimentacao, .data$documento,
      .data$descricao, .data$valor, .data$saldo,
      .data$conta.interno, .data$conta, .data$agencia, .data$produto,
      .data$cnpj, .data$empresa,
      .data$periodo.inicio, .data$periodo.fim, .data$data.consulta,
      .data$arquivo, .data$arquivo.subtipo,
      .data$arquivo.tabela.tipo, .data$arquivo.tipo, .data$arquivo.fonte
    )

  # Garantir que as colunas CEF tenham a mesma estrutura
  colunasNecessarias <- c(
    "data.lancamento", "data.movimentacao", "documento", "descricao", "valor",
    "saldo", "conta.interno", "conta", "agencia", "produto", "cnpj", "empresa",
    "periodo.inicio", "periodo.fim", "data.consulta", "arquivo",
    "arquivo.subtipo", "arquivo.tabela.tipo", "arquivo.tipo", "arquivo.fonte"
  )

  # Verificar e criar colunas faltantes nos dados CEF
  for (col in colunasNecessarias) {
    if (!col %in% names(dadosCef)) {
      if (col %in%
        c(
          "data.lancamento", "data.movimentacao", "periodo.inicio",
          "periodo.fim"
        )
      ) {
        dadosCef[[col]] <- as.Date(NA)
      } else if (col == "data.consulta") {
        dadosCef[[col]] <- as.POSIXct(NA)
      } else if (col %in% c("valor", "saldo")) {
        dadosCef[[col]] <- NA_real_
      } else {
        dadosCef[[col]] <- NA_character_
      }
    }
  }

  # Verificar e criar colunas faltantes nos dados ITAÚ padronizados
  for (col in colunasNecessarias) {
    if (!col %in% names(dadosItaPadronizados)) {
      if (col %in%
        c(
          "data.lancamento", "data.movimentacao", "periodo.inicio",
          "periodo.fim"
        )) {
        dadosItaPadronizados[[col]] <- as.Date(NA)
      } else if (col == "data.consulta") {
        dadosItaPadronizados[[col]] <- as.POSIXct(NA)
      } else if (col %in% c("valor", "saldo")) {
        dadosItaPadronizados[[col]] <- NA_real_
      } else {
        dadosItaPadronizados[[col]] <- NA_character_
      }
    }
  }

  # Selecionar apenas as colunas necessárias e na ordem correta
  dadosCefLimpos <- dadosCef %>%
    dplyr::select(all_of(colunasNecessarias))

  dadosItaLimpos <- dadosItaPadronizados %>%
    dplyr::select(all_of(colunasNecessarias))

  # Consolidar os dados
  extratosConsolidados <- dplyr::bind_rows(
    dadosCefLimpos,
    dadosItaLimpos
  ) %>%
    dplyr::mutate(
      # Padronizar identificação da fonte
      banco = dplyr::case_when(
        .data$arquivo.fonte == "cef" ~ "CEF",
        .data$arquivo.fonte == "ita" ~ "Itau",
        TRUE ~ "Desconhecido"
      )
    ) %>%
    dplyr::arrange(.data$data.movimentacao, .data$empresa, .data$valor) %>%
    dplyr::as_tibble()

  message(sprintf("Total de registros CEF: %d", nrow(dadosCefLimpos)))
  message(sprintf("Total de registros ITAÚ: %d", nrow(dadosItaLimpos)))
  message(sprintf("Total de registros consolidados: %d", nrow(extratosConsolidados)))

  if (nrow(extratosConsolidados) > 0) {
    message(sprintf(
      "Período coberto: %s a %s",
      min(extratosConsolidados$data.movimentacao, na.rm = TRUE),
      max(extratosConsolidados$data.movimentacao, na.rm = TRUE)
    ))
    empresasUnicas <- unique(extratosConsolidados$empresa[!is.na(extratosConsolidados$empresa)])
    message(sprintf(
      "Empresas encontradas: %s",
      paste(empresasUnicas, collapse = ", ")
    ))
  }

  # Função auxiliar para criar mapeamento
  .mapeamento <- function(extratos) {
    if (nrow(extratos) == 0) {
      message("Nenhum dado de extrato encontrado para mapeamento.")
      return(tibble::tibble(
        empresa = character(),
        banco = character(),
        conta = character(),
        descricao = character(),
        quantidade.arquivos = integer(),
        quantidade.registros = integer(),
        soma.valor = numeric(),
        soma.valor.abs = numeric(),
        `arquivo(s)` = character()
      ))
    }

    # Criar mapa com combinações únicas da tupla (empresa, banco, conta, descricao)
    mapa <- extratos %>%
      dplyr::filter(!is.na(.data$empresa) & !is.na(.data$banco) &
        !is.na(.data$conta) & !is.na(.data$descricao)) %>%
      dplyr::group_by(.data$empresa, .data$banco, .data$conta, .data$descricao) %>%
      dplyr::summarise(
        `quantidade.arquivos` = length(unique(.data$arquivo)),
        `quantidade.registros` = dplyr::n(),
        `soma.valor` = sum(.data$valor, na.rm = TRUE),
        `soma.valor.abs` = sum(abs(.data$valor), na.rm = TRUE),
        `arquivo(s)` = paste(unique(basename(.data$arquivo)), collapse = "; "),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$empresa, .data$banco, .data$conta, .data$descricao)

    message(sprintf("Total de combinações únicas encontradas: %d", nrow(mapa)))
    return(mapa)
  }

  # Gerar mapa de extratos
  mapaExtratos <- .mapeamento(extratosConsolidados)

  # Criar arquivo Excel se solicitado
  if (xlsx && nrow(mapaExtratos) > 0) {
    # Definindo o nome do arquivo dinamicamente
    nomeArquivo <- stringr::str_c(
      "Extratos-",
      format(Sys.time(), "%Y_%m_%d-%H_%M_%S"),
      ".xlsx"
    )

    # Caminho do template
    caminhoTemplate <- "C:\\Users\\Ampla\\AMPLA INCORPORADORA LTDA\\Controladoria - Documentos\\amplaGitHub\\templates\\Template-DFC.xlsx"

    # Definir larguras específicas das colunas para o resumo
    largurasColunas <- c(
      "empresa" = 20,
      "banco" = 12,
      "conta" = 18,
      "descricao" = 60,
      "quantidade.arquivos" = 18,
      "quantidade.registros" = 20,
      "soma.valor" = 15,
      "soma.valor.abs" = 15,
      "arquivo(s)" = 80
    )

    # Criar caminho de destino completo
    caminhoDestino <- stringr::str_c(caminhos_pastas("extratos"), "/Consolidados")
    caminhoArquivo <- file.path(caminhoDestino, nomeArquivo)

    # Criar diretório se não existir
    dir.create(caminhoDestino, showWarnings = FALSE, recursive = TRUE)

    # Copiar template para destino
    file.copy(caminhoTemplate, caminhoArquivo, overwrite = TRUE)

    # Carregar o workbook copiado
    wb <- openxlsx::loadWorkbook(caminhoArquivo)

    # Adicionar aba "Extratos"
    openxlsx::addWorksheet(wb, "Extratos")
    openxlsx::writeData(wb, sheet = "Extratos", x = extratosConsolidados)

    # Formatação da aba Extratos
    openxlsx::addStyle(
      wb,
      sheet = "Extratos",
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center"
      ),
      rows = 1:(nrow(extratosConsolidados) + 1),
      cols = seq_len(ncol(extratosConsolidados)),
      gridExpand = TRUE
    )

    # Cabeçalho da aba Extratos
    openxlsx::addStyle(
      wb,
      sheet = "Extratos",
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        fontSize = 11,
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "darkgray",
        wrapText = TRUE
      ),
      rows = 1,
      cols = seq_len(ncol(extratosConsolidados)),
      gridExpand = TRUE
    )

    # Adicionar filtro e congelar painel na aba Extratos
    openxlsx::addFilter(wb, sheet = "Extratos", rows = 1, cols = seq_len(ncol(extratosConsolidados)))
    openxlsx::freezePane(wb, sheet = "Extratos", firstRow = TRUE, firstActiveRow = 2)

    # Largura das colunas na aba Extratos
    openxlsx::setColWidths(wb, sheet = "Extratos", cols = seq_len(ncol(extratosConsolidados)), widths = 18)

    # Adicionar aba "Resumo"
    openxlsx::addWorksheet(wb, "Resumo")
    openxlsx::writeData(wb, sheet = "Resumo", x = mapaExtratos)

    # Formatação da aba Resumo
    openxlsx::addStyle(
      wb,
      sheet = "Resumo",
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        halign = "center",
        valign = "center"
      ),
      rows = 1:(nrow(mapaExtratos) + 1),
      cols = seq_len(ncol(mapaExtratos)),
      gridExpand = TRUE
    )

    # Cabeçalho da aba Resumo
    openxlsx::addStyle(
      wb,
      sheet = "Resumo",
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        fontSize = 11,
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "darkgray",
        wrapText = TRUE
      ),
      rows = 1,
      cols = seq_len(ncol(mapaExtratos)),
      gridExpand = TRUE
    )

    # Adicionar filtro e congelar painel na aba Resumo
    openxlsx::addFilter(wb, sheet = "Resumo", rows = 1, cols = seq_len(ncol(mapaExtratos)))
    openxlsx::freezePane(wb, sheet = "Resumo", firstRow = TRUE, firstActiveRow = 2)

    # Larguras específicas das colunas na aba Resumo
    for (nome_coluna in names(largurasColunas)) {
      if (nome_coluna %in% colnames(mapaExtratos)) {
        col_pos <- which(colnames(mapaExtratos) == nome_coluna)
        openxlsx::setColWidths(wb, sheet = "Resumo", cols = col_pos, widths = largurasColunas[nome_coluna])
      }
    }

    # Formatação monetária na aba Resumo
    colunas_monetarias_resumo <- which(colnames(mapaExtratos) %in% c("soma.valor", "soma.valor.abs"))
    if (length(colunas_monetarias_resumo) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = "Resumo",
        style = openxlsx::createStyle(numFmt = "#,##0.00"),
        rows = 2:(nrow(mapaExtratos) + 1),
        cols = colunas_monetarias_resumo,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Formatação de texto com quebra na aba Resumo
    colunas_texto_resumo <- which(colnames(mapaExtratos) %in% c("arquivo(s)"))
    if (length(colunas_texto_resumo) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = "Resumo",
        style = openxlsx::createStyle(
          border = "TopBottomLeftRight",
          halign = "left",
          valign = "top",
          wrapText = TRUE
        ),
        rows = 2:(nrow(mapaExtratos) + 1),
        cols = colunas_texto_resumo,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Obter descrições únicas em ordem alfabética
    descricoesUnicas <- sort(unique(extratosConsolidados$descricao[!is.na(extratosConsolidados$descricao)]))

    # Verificar se a aba "Mapeamento" existe
    if ("Mapeamento" %in% names(wb)) {
      # Escrever as descrições na coluna D a partir da linha 3
      if (length(descricoesUnicas) > 0) {
        openxlsx::writeData(
          wb,
          sheet = "Mapeamento",
          x = descricoesUnicas, # Escrever diretamente o vetor
          startCol = 4, # Coluna D
          startRow = 3,
          colNames = FALSE
        )

        message(sprintf("Adicionadas %d descrições únicas na aba 'Mapeamento'", length(descricoesUnicas)))
      }
    } else {
      message("Aba 'Mapeamento' não encontrada no template")
    }

    # Salvar as alterações
    openxlsx::saveWorkbook(wb, caminhoArquivo, overwrite = TRUE)

    message(sprintf("Arquivo Excel criado com template: %s", caminhoArquivo))
    message("  - Aba 'Extratos': Dados consolidados de extratos CEF e ITAÚ")
    message("  - Aba 'Resumo': Mapa de combinações únicas")
    message("  - Aba 'Mapeamento': Descrições únicas populadas")

    # Mostrar estatísticas
    if (nrow(mapaExtratos) > 0) {
      empresasUnicas <- unique(mapaExtratos$empresa)
      bancosUnicos <- unique(mapaExtratos$banco)
      contasUnicas <- unique(mapaExtratos$conta)

      message(sprintf("Empresas: %s", paste(empresasUnicas, collapse = ", ")))
      message(sprintf("Bancos: %s", paste(bancosUnicos, collapse = ", ")))
      message(sprintf("Total de contas únicas: %d", length(contasUnicas)))
    }
  }

  return(list(
    extratosConsolidados = extratosConsolidados,
    mapaExtratos = mapaExtratos
  ))
}

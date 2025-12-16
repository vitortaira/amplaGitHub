#' Gera planilha Excel personalizada
#'
#' @description
#' Função genérica para criar planilhas Excel com formatação padronizada.
#' Pode receber um tibble/dataframe (para uma aba) ou uma lista de tibbles
#' (para múltiplas abas). A formatação é baseada nos tipos das colunas e
#' argumentos específicos para colunas monetárias e com quebra de texto.
#'
#' @param data Um tibble/dataframe ou uma lista nomeada de tibbles/dataframes
#' @param wb_load Caminho opcional para arquivo de template a ser copiado e carregado.
#'   Se vazio, cria um novo workbook
#' @param tab_names Vetor de strings com nomes das abas. Se NULL, usa nomes da lista
#'   ou "Dados" para tibble único
#' @param tab_colours Vetor nomeado com cores para as abas. Os nomes devem corresponder
#'   aos nomes das abas em `tab_names`. Cores podem ser nomes de cores do Excel
#'   (ex: "red", "blue", "purple") ou códigos hexadecimais. Se NULL, não aplica cores.
#' @param col_width_def Largura padrão das colunas (valor numérico). Padrão: 18
#' @param col_width_spec Vetor nomeado com larguras específicas para colunas por nome
#' @param col_width_auto Vetor com nomes de colunas que devem ter largura ajustada automaticamente ao conteúdo
#' @param col_headers Lista customizando estilos de cabeçalhos por aba e coluna.
#'   Formato: list(aba = list(coluna = list(colour = "cor", font_colour = "cor", font_size = tam, wrapText = TRUE/FALSE))).
#'   Propriedades suportadas: colour (cor de fundo), font_colour (cor da fonte), font_size (tamanho da fonte),
#'   wrapText (quebra de texto, padrão FALSE)
#' @param col_groups Lista definindo grupos de colunas por aba. Formato: list(aba = list(list(cols = c("col1", "col2"), hidden = FALSE, level = 1)))
#' @param tab_freeze Vetor nomeado definindo onde congelar painéis por aba. Formato: c(aba = "nome_coluna"). A coluna especificada será a última congelada.
#' @param col_monetary Vetor com nomes de colunas que devem ser formatadas como valores monetários (com decimais).
#'   Se NULL, infere automaticamente baseado no tipo das colunas (numeric, excluindo integer)
#' @param col_dates Vetor com nomes de colunas que devem ser formatadas como datas.
#'   Se NULL, infere automaticamente baseado no tipo das colunas (Date)
#' @param col_clip Vetor com nomes de colunas de texto que devem ter quebra de texto habilitada
#' @param col_align Lista nomeada especificando alinhamento horizontal por coluna.
#'   Formato: c(col1 = "left", col2 = "center", col3 = "right").
#'   Se NULL, usa alinhamento padrão (center para numéricos, left para texto).
#' @param tab_zoom Vetor nomeado com nível de zoom (em porcentagem) para cada aba.
#'   Formato: c(aba1 = 80, aba2 = 100). Se NULL, usa zoom de 80% como padrão para todas as abas.
#'   Valores devem estar entre 10 e 400.
#' @param table Booleano. Se TRUE, converte os dados em tabelas Excel. Padrão: TRUE.
#' @param save Lista com 2 elementos: (1) nome do arquivo, (2) caminho de destino.
#'   Se NULL, salva automaticamente no diretório Downloads com nome "xlsx-YYYY_MM_DD-HH_MM_SS.xlsx"
#'
#' @return Caminho do arquivo salvo
#' @importFrom dplyr bind_rows mutate select rename case_when if_else arrange all_of
#' @importFrom stringr str_c
#' @importFrom openxlsx createWorkbook loadWorkbook addWorksheet writeData writeDataTable addStyle createStyle
#' @importFrom openxlsx saveWorkbook setColWidths addFilter freezePane createNamedRegion deleteNamedRegion getNamedRegions showGridLines
#' @importFrom openxlsx groupColumns
#' @importFrom purrr walk2
#' @export
#'
gerar_xlsx <- function(data,
                       wb_load = NULL,
                       tab_names = NULL,
                       tab_colours = NULL,
                       col_width_def = 18,
                       col_width_spec = NULL,
                       col_width_auto = NULL,
                       col_headers = NULL,
                       col_groups = NULL,
                       tab_freeze = NULL,
                       col_monetary = NULL,
                       col_dates = NULL,
                       col_clip = NULL,
                       col_align = NULL,
                       tab_zoom = NULL,
                       table = TRUE,
                       save = NULL) {
  # Validação e preparação dos dados
  if (is.data.frame(data)) {
    # Se é um dataframe único, converte para lista
    dados_lista <- list(data)
    if (is.null(tab_names)) {
      tab_names <- "Dados"
    }
  } else if (is.list(data) && all(sapply(data, is.data.frame))) {
    # Se é uma lista de dataframes
    dados_lista <- data
    if (is.null(tab_names)) {
      if (is.null(names(data))) {
        tab_names <- paste0("Aba", seq_along(data))
      } else {
        tab_names <- names(data)
      }
    }
  } else {
    stop("'data' deve ser um tibble/dataframe ou uma lista de tibbles/dataframes")
  }

  # Validar que o número de nomes de abas corresponde aos dados
  if (length(tab_names) != length(dados_lista)) {
    stop("O número de nomes em 'tab_names' deve corresponder ao número de elementos em 'data'")
  }

  # Criar ou carregar workbook
  if (is.null(wb_load)) {
    # Criar novo workbook
    wb <- openxlsx::createWorkbook()
  } else {
    # Carregar template
    if (!file.exists(wb_load)) {
      stop("Arquivo template não encontrado: ", wb_load)
    }

    # Se save foi especificado, copiar template para destino
    if (!is.null(save)) {
      if (length(save) != 2) {
        stop("'save' deve ser uma lista com 2 elementos: nome do arquivo e caminho")
      }

      nome_arquivo <- save[[1]]
      caminho_destino <- save[[2]]

      # Criar nome completo do arquivo de destino
      if (!is.character(nome_arquivo) || length(nome_arquivo) != 1) {
        stop("O nome do arquivo deve ser uma string")
      }

      if (!is.character(caminho_destino) || length(caminho_destino) != 1) {
        stop("O caminho de destino deve ser uma string")
      }

      # Construir caminho completo
      caminho_arquivo_temp <- file.path(caminho_destino, nome_arquivo)

      # Criar diretório se não existir
      dir.create(dirname(caminho_arquivo_temp), showWarnings = FALSE, recursive = TRUE)

      # Copiar template
      file.copy(wb_load, caminho_arquivo_temp, overwrite = TRUE)

      # Carregar a cópia
      wb <- openxlsx::loadWorkbook(caminho_arquivo_temp)
    } else {
      # Carregar template diretamente
      wb <- openxlsx::loadWorkbook(wb_load)
    }
  }

  # Lista nomeada para processamento
  names(dados_lista) <- tab_names

  # Processar cada aba
  for (i in seq_along(dados_lista)) {
    df_dados <- dados_lista[[i]]
    nome_aba <- names(dados_lista)[i]

    # Inferir colunas monetárias (apenas numeric, não integer) se não especificadas
    if (is.null(col_monetary)) {
      col_monetary <- colnames(df_dados)[sapply(df_dados, function(x) is.numeric(x) & !is.integer(x))]
    }

    # Inferir colunas de data se não especificadas
    if (is.null(col_dates)) {
      col_dates <- colnames(df_dados)[sapply(df_dados, function(x) inherits(x, "Date"))]
    }

    # Adicionar worksheet se necessário (para novos workbooks)
    if (is.null(wb_load)) {
      # Verificar se há cor especificada para esta aba
      cor_aba <- if (!is.null(tab_colours) && nome_aba %in% names(tab_colours)) {
        tab_colours[nome_aba]
      } else {
        NULL
      }

      openxlsx::addWorksheet(wb, nome_aba, gridLines = FALSE, tabColour = cor_aba)
    } else {
      # Remover gridlines de worksheet existente
      openxlsx::showGridLines(wb, sheet = nome_aba, showGridLines = FALSE)

      # Remover tabelas existentes na aba (para evitar erro de sobreposição)
      tabelas_existentes <- openxlsx::getTables(wb, sheet = nome_aba)
      if (length(tabelas_existentes) > 0) {
        for (tabela in tabelas_existentes) {
          openxlsx::removeTable(wb, sheet = nome_aba, table = tabela)
        }
      }

      # Remover regiões nomeadas que possam conflitar com a nova tabela
      regioes <- openxlsx::getNamedRegions(wb)
      nome_regiao_lower <- tolower(nome_aba)
      if (nome_regiao_lower %in% regioes) {
        openxlsx::deleteNamedRegion(wb, name = nome_regiao_lower)
      }
      if (nome_aba %in% regioes) {
        openxlsx::deleteNamedRegion(wb, name = nome_aba)
      }
    }

    # Deletar região nomeada antiga, se existir (apenas para novos workbooks)
    if (is.null(wb_load)) {
      nome_regiao_lower <- tolower(nome_aba)
      regioes <- openxlsx::getNamedRegions(wb)
      if (nome_regiao_lower %in% regioes) {
        openxlsx::deleteNamedRegion(wb, name = nome_regiao_lower)
      }
      if (nome_aba %in% regioes) {
        openxlsx::deleteNamedRegion(wb, name = nome_aba)
      }
    }

    # Garantir que os nomes das colunas sejam únicos (case-insensitive para Excel)
    nomes_colunas <- colnames(df_dados)
    nomes_lower <- tolower(nomes_colunas)
    if (anyDuplicated(nomes_lower)) {
      # Encontrar duplicatas case-insensitive e renomear
      nomes_unicos <- make.unique(nomes_lower, sep = "_")
      # Preservar case original onde possível, adicionar sufixo onde necessário
      for (j in seq_along(nomes_colunas)) {
        if (nomes_lower[j] != nomes_unicos[j]) {
          # Extrair o sufixo adicionado por make.unique
          sufixo <- sub(nomes_lower[j], "", nomes_unicos[j])
          nomes_colunas[j] <- paste0(nomes_colunas[j], sufixo)
        }
      }
      colnames(df_dados) <- nomes_colunas
    }

    # Escrever os dados
    if (table) {
      # Nome da tabela não pode conter caracteres especiais (substituir por _)
      nome_tabela <- gsub("[^a-zA-Z0-9]", "_", nome_aba)
      openxlsx::writeDataTable(
        wb,
        sheet = nome_aba,
        x = df_dados,
        tableName = nome_tabela,
        tableStyle = "TableStyleLight1",
        withFilter = TRUE
      )
    } else {
      openxlsx::writeData(wb, sheet = nome_aba, x = df_dados)

      # Criar nova região nomeada (apenas para novos workbooks e sem tabela)
      if (is.null(wb_load)) {
        nome_regiao <- tolower(nome_aba)
        openxlsx::createNamedRegion(
          wb,
          sheet = nome_aba,
          name = nome_regiao,
          rows = 1:(nrow(df_dados) + 1),
          cols = seq_len(ncol(df_dados))
        )
      }
    }

    # Aplicar estilos básicos apenas se não for tabela (tabelas têm seu próprio styling básico)
    if (!table) {
      # Estilo geral (bordas e alinhamento)
      openxlsx::addStyle(
        wb,
        sheet = nome_aba,
        style = openxlsx::createStyle(
          border = "TopBottomLeftRight",
          halign = "center",
          valign = "center",
          wrapText = FALSE # Evitar wrap text por padrão
        ),
        rows = 1:(nrow(df_dados) + 1),
        cols = seq_len(ncol(df_dados)),
        gridExpand = TRUE
      )

      # Estilo do cabeçalho padrão (apenas para colunas sem estilo customizado)
      colunas_com_estilo_custom <- c()
      if (!is.null(col_headers) && nome_aba %in% names(col_headers)) {
        colunas_com_estilo_custom <- intersect(
          names(col_headers[[nome_aba]]),
          colnames(df_dados)
        )
      }

      colunas_padrao <- which(!(colnames(df_dados) %in% colunas_com_estilo_custom))
      if (length(colunas_padrao) > 0) {
        openxlsx::addStyle(
          wb,
          sheet = nome_aba,
          style = openxlsx::createStyle(
            border = "TopBottomLeftRight",
            fontSize = 11,
            halign = "center",
            valign = "center",
            textDecoration = "bold",
            fgFill = "lightgray",
            wrapText = FALSE
          ),
          rows = 1,
          cols = colunas_padrao,
          gridExpand = TRUE
        )
      }

      # Adicionar filtro (não necessário para tabelas)
      openxlsx::addFilter(wb, sheet = nome_aba, rows = 1, cols = seq_len(ncol(df_dados)))
    }

    # Estilos customizados para cabeçalhos específicos (aplicar sempre, mesmo com tabela)
    if (!is.null(col_headers) && nome_aba %in% names(col_headers)) {
      headers_aba <- col_headers[[nome_aba]]
      for (nome_coluna in names(headers_aba)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(colnames(df_dados) == nome_coluna)
          config <- headers_aba[[nome_coluna]]

          # Valores padrão
          cor_fundo <- if (!is.null(config$colour)) config$colour else "lightgray"
          cor_fonte <- if (!is.null(config$font_colour)) config$font_colour else NULL
          tamanho_fonte <- if (!is.null(config$font_size)) config$font_size else 11

          # Determinar wrap text para este cabeçalho
          wrap_padrao <- FALSE
          if (!is.null(config$wrapText)) {
            wrap_padrao <- config$wrapText
          }

          # Criar estilo com ou sem cor de fonte
          estilo_params <- list(
            border = "TopBottomLeftRight",
            fontSize = tamanho_fonte,
            halign = "center",
            valign = "center",
            textDecoration = "bold",
            fgFill = cor_fundo,
            wrapText = wrap_padrao
          )

          if (!is.null(cor_fonte)) {
            estilo_params$fontColour <- cor_fonte
          }

          openxlsx::addStyle(
            wb,
            sheet = nome_aba,
            style = do.call(openxlsx::createStyle, estilo_params),
            rows = 1,
            cols = col_pos,
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
    }

    # Congelar painel (aplicar sempre)
    if (!is.null(tab_freeze) && nome_aba %in% names(tab_freeze)) {
      col_congelar <- tab_freeze[[nome_aba]]
      if (col_congelar %in% colnames(df_dados)) {
        col_pos <- which(colnames(df_dados) == col_congelar)
        openxlsx::freezePane(
          wb,
          sheet = nome_aba,
          firstActiveRow = 2,
          firstActiveCol = col_pos + 1
        )
      } else {
        openxlsx::freezePane(wb, sheet = nome_aba, firstRow = TRUE, firstActiveRow = 2)
      }
    } else {
      openxlsx::freezePane(wb, sheet = nome_aba, firstRow = TRUE, firstActiveRow = 2)
    }

    # Ajustar larguras automaticamente baseado no conteúdo (aplicar sempre)
    larguras_calculadas <- sapply(seq_len(ncol(df_dados)), function(col_idx) {
      col_data <- df_dados[[col_idx]]
      col_name <- names(df_dados)[col_idx]

      max_nome <- nchar(col_name)

      if (is.character(col_data) || is.factor(col_data)) {
        valores_nao_na <- as.character(col_data)[!is.na(col_data)]
        if (length(valores_nao_na) > 0) {
          max_dados <- max(nchar(valores_nao_na), na.rm = TRUE)
        } else {
          max_dados <- 0
        }
      } else {
        valores_nao_na <- col_data[!is.na(col_data)]
        if (length(valores_nao_na) > 0) {
          max_dados <- max(nchar(format(valores_nao_na)), na.rm = TRUE)
        } else {
          max_dados <- 0
        }
      }

      largura_sugerida <- max(max_nome, max_dados, na.rm = TRUE)

      if (is.infinite(largura_sugerida) || largura_sugerida == 0) {
        max_nome + 2
      } else if (largura_sugerida > 50) {
        30
      } else if (largura_sugerida < 8) {
        10
      } else {
        min(largura_sugerida + 2, col_width_def)
      }
    })

    openxlsx::setColWidths(wb, sheet = nome_aba, cols = seq_len(ncol(df_dados)), widths = larguras_calculadas)

    # Colunas de texto (alinhamento à esquerda) - aplicar sempre
    colunas_texto <- which(sapply(df_dados, function(x) is.character(x) | is.factor(x)))
    if (length(colunas_texto) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = nome_aba,
        style = openxlsx::createStyle(halign = "left", wrapText = FALSE),
        rows = 2:(nrow(df_dados) + 1),
        cols = colunas_texto,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Alinhamento customizado por coluna (aplicar sempre)
    if (!is.null(col_align)) {
      for (nome_coluna in names(col_align)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(colnames(df_dados) == nome_coluna)
          align_tipo <- col_align[[nome_coluna]]

          openxlsx::addStyle(
            wb,
            sheet = nome_aba,
            style = openxlsx::createStyle(halign = align_tipo, valign = "center"),
            rows = 2:(nrow(df_dados) + 1),
            cols = col_pos,
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }
    }

    # Larguras específicas (aplicar sempre)
    if (!is.null(col_width_spec)) {
      for (nome_coluna in names(col_width_spec)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(colnames(df_dados) == nome_coluna)
          openxlsx::setColWidths(
            wb,
            sheet = nome_aba,
            cols = col_pos,
            widths = col_width_spec[nome_coluna]
          )
        }
      }
    }

    # Larguras automáticas (aplicar sempre)
    if (!is.null(col_width_auto)) {
      for (nome_coluna in col_width_auto) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(colnames(df_dados) == nome_coluna)
          openxlsx::setColWidths(
            wb,
            sheet = nome_aba,
            cols = col_pos,
            widths = "auto"
          )
        }
      }
    }

    # Estilo para valores monetários (aplicar sempre)
    if (!is.null(col_monetary) && length(col_monetary) > 0) {
      colunas_monetarias <- which(colnames(df_dados) %in% col_monetary)
      if (length(colunas_monetarias) > 0) {
        openxlsx::addStyle(
          wb,
          sheet = nome_aba,
          style = openxlsx::createStyle(numFmt = "#,##0.00"),
          rows = 2:(nrow(df_dados) + 1),
          cols = colunas_monetarias,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    # Estilo para valores inteiros (aplicar sempre)
    colunas_inteiras <- which(sapply(df_dados, is.integer))
    if (length(colunas_inteiras) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = nome_aba,
        style = openxlsx::createStyle(numFmt = "#,##0"),
        rows = 2:(nrow(df_dados) + 1),
        cols = colunas_inteiras,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Estilo para datas (aplicar sempre)
    if (!is.null(col_dates) && length(col_dates) > 0) {
      colunas_data <- which(colnames(df_dados) %in% col_dates)
      if (length(colunas_data) > 0) {
        openxlsx::addStyle(
          wb,
          sheet = nome_aba,
          style = openxlsx::createStyle(numFmt = "DD/MM/YYYY"),
          rows = 2:(nrow(df_dados) + 1),
          cols = colunas_data,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    # Estilo para data e hora (aplicar sempre)
    colunas_datetime <- which(sapply(df_dados, function(x) inherits(x, "POSIXct") | inherits(x, "POSIXt")))
    if (length(colunas_datetime) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = nome_aba,
        style = openxlsx::createStyle(numFmt = "YYYY-MM-DD HH:MM:SS"),
        rows = 2:(nrow(df_dados) + 1),
        cols = colunas_datetime,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Estilo específico para colunas com quebra de texto (aplicar sempre)
    if (!is.null(col_clip)) {
      colunas_clip <- which(colnames(df_dados) %in% col_clip)
      if (length(colunas_clip) > 0) {
        openxlsx::addStyle(
          wb,
          sheet = nome_aba,
          style = openxlsx::createStyle(
            border = "TopBottomLeftRight",
            halign = "left",
            valign = "top",
            wrapText = TRUE
          ),
          rows = 2:(nrow(df_dados) + 1),
          cols = colunas_clip,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    # Agrupar colunas
    if (!is.null(col_groups) && nome_aba %in% names(col_groups)) {
      grupos_aba <- col_groups[[nome_aba]]
      for (grupo in grupos_aba) {
        colunas_nomes <- grupo$cols
        colunas_indices <- which(colnames(df_dados) %in% colunas_nomes)

        if (length(colunas_indices) > 0) {
          col_inicio <- min(colunas_indices)
          col_fim <- max(colunas_indices)

          ocultar <- if (!is.null(grupo$hidden)) grupo$hidden else FALSE
          nivel <- if (!is.null(grupo$level)) grupo$level else 1

          openxlsx::groupColumns(
            wb,
            sheet = nome_aba,
            cols = col_inicio:col_fim,
            hidden = ocultar,
            level = nivel
          )
        }
      }
    }
  }

  # Aplicar zoom às abas
  # Se tab_zoom não for especificado, usar 80% como padrão
  for (i in seq_along(tab_names)) {
    nome_aba <- tab_names[i]

    # Determinar o zoom para esta aba
    zoom_valor <- 80 # Padrão
    if (!is.null(tab_zoom)) {
      if (nome_aba %in% names(tab_zoom)) {
        zoom_valor <- tab_zoom[nome_aba]
      } else if (length(tab_zoom) == 1 && is.null(names(tab_zoom))) {
        # Se tab_zoom é um vetor sem nomes com um único valor, aplicar a todas
        zoom_valor <- tab_zoom[1]
      }
    }

    # Validar zoom (deve estar entre 10 e 400)
    zoom_valor <- max(10, min(400, as.numeric(zoom_valor)))

    # Aplicar zoom à aba modificando o XML sheetView
    ws <- wb$worksheets[[i]]

    if (!is.null(ws) && !is.null(ws$sheetViews)) {
      # sheetViews é uma string XML
      sheetViews_str <- ws$sheetViews

      # Substituir ou adicionar zoomScale
      if (grepl('zoomScale="[0-9]+"', sheetViews_str)) {
        # Substituir valor existente
        sheetViews_str <- sub(
          'zoomScale="[0-9]+"',
          paste0('zoomScale="', zoom_valor, '"'),
          sheetViews_str
        )
      } else {
        # Adicionar zoomScale após workbookViewId
        sheetViews_str <- sub(
          'workbookViewId="[0-9]+"',
          paste0('workbookViewId="0" zoomScale="', zoom_valor, '"'),
          sheetViews_str
        )
      }

      # Atualizar a worksheet
      ws$sheetViews <- sheetViews_str
    }
  }

  # Salvar arquivo
  if (!is.null(save)) {
    # Salvar com parâmetros especificados pelo usuário
    nome_arquivo <- save[[1]]
    caminho_destino <- save[[2]]
    caminho_completo <- file.path(caminho_destino, nome_arquivo)
  } else {
    # Salvar automaticamente no diretório de downloads com timestamp
    timestamp <- format(Sys.time(), "%Y_%m_%d-%H_%M_%S")
    nome_arquivo <- paste0("xlsx-", timestamp, ".xlsx")

    # Determinar diretório de downloads
    if (Sys.info()["sysname"] == "Windows") {
      caminho_destino <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
    } else {
      caminho_destino <- file.path(path.expand("~"), "Downloads")
    }

    caminho_completo <- file.path(caminho_destino, nome_arquivo)
  }

  # Normalizar caminho para consistência de separadores no Windows
  caminho_completo <- normalizePath(caminho_completo, winslash = "\\", mustWork = FALSE)

  # Criar diretório se não existir
  dir.create(dirname(caminho_completo), showWarnings = FALSE, recursive = TRUE)

  # Salvar workbook
  openxlsx::saveWorkbook(wb, caminho_completo, overwrite = TRUE)

  message(sprintf("Planilha salva em: %s", caminho_completo))

  return(caminho_completo)
}

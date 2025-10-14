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
#' @param col_width_def Largura padrão das colunas (valor numérico). Padrão: 18
#' @param col_width_spec Vetor nomeado com larguras específicas para colunas por nome
#' @param col_monetary Vetor com nomes de colunas que devem ser formatadas como valores monetários.
#'   Se NULL, infere automaticamente baseado no tipo das colunas (numeric/integer)
#' @param col_dates Vetor com nomes de colunas que devem ser formatadas como datas.
#'   Se NULL, infere automaticamente baseado no tipo das colunas (Date)
#' @param col_clip Vetor com nomes de colunas de texto que devem ter quebra de texto habilitada
#' @param save Lista com 2 elementos: (1) nome do arquivo, (2) caminho de destino.
#'   Se NULL, salva automaticamente no diretório Downloads com nome "xlsx-YYYY_MM_DD-HH_MM_SS.xlsx"
#'
#' @return Caminho do arquivo salvo
#' @importFrom dplyr bind_rows mutate select rename case_when if_else arrange all_of
#' @importFrom stringr str_c
#' @importFrom openxlsx createWorkbook loadWorkbook addWorksheet writeData addStyle createStyle
#' @importFrom openxlsx saveWorkbook setColWidths addFilter freezePane createNamedRegion deleteNamedRegion getNamedRegions
#' @importFrom purrr walk2
#' @export
#'
gerar_xlsx <- function(data,
                       wb_load = NULL,
                       tab_names = NULL,
                       col_width_def = 18,
                       col_width_spec = NULL,
                       col_monetary = NULL,
                       col_dates = NULL,
                       col_clip = NULL,
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

    # Inferir colunas monetárias se não especificadas
    if (is.null(col_monetary)) {
      col_monetary <- colnames(df_dados)[sapply(df_dados, function(x) is.numeric(x) | is.integer(x))]
    }

    # Inferir colunas de data se não especificadas
    if (is.null(col_dates)) {
      col_dates <- colnames(df_dados)[sapply(df_dados, function(x) inherits(x, "Date"))]
    }

    # Adicionar worksheet se necessário (para novos workbooks)
    if (is.null(wb_load)) {
      openxlsx::addWorksheet(wb, nome_aba)
    }

    # Deletar região nomeada antiga, se existir (apenas para novos workbooks)
    if (is.null(wb_load)) {
      nome_regiao <- tolower(nome_aba)
      if (nome_regiao %in% openxlsx::getNamedRegions(wb)) {
        openxlsx::deleteNamedRegion(wb, name = nome_regiao)
      }
    }

    # Escrever os dados
    openxlsx::writeData(wb, sheet = nome_aba, x = df_dados)

    # Criar nova região nomeada (apenas para novos workbooks)
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

    # Estilo do cabeçalho
    openxlsx::addStyle(
      wb,
      sheet = nome_aba,
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        fontSize = 11,
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        fgFill = "lightgray", # Cor mais suave
        wrapText = TRUE
      ),
      rows = 1,
      cols = seq_len(ncol(df_dados)),
      gridExpand = TRUE
    )

    # Adicionar filtro e congelar painel
    openxlsx::addFilter(wb, sheet = nome_aba, rows = 1, cols = seq_len(ncol(df_dados)))
    openxlsx::freezePane(wb, sheet = nome_aba, firstRow = TRUE, firstActiveRow = 2)

    # Ajustar larguras automaticamente baseado no conteúdo
    # Para colunas de texto muito longas, limitar a largura máxima
    larguras_calculadas <- sapply(seq_len(ncol(df_dados)), function(col_idx) {
      col_data <- df_dados[[col_idx]]
      col_name <- names(df_dados)[col_idx]

      # Calcular largura baseada no nome da coluna e dados
      max_nome <- nchar(col_name)
      max_dados <- if (is.character(col_data) || is.factor(col_data)) {
        max(nchar(as.character(col_data)), na.rm = TRUE)
      } else {
        max(nchar(format(col_data)), na.rm = TRUE)
      }

      largura_sugerida <- max(max_nome, max_dados, na.rm = TRUE)

      # Limitar larguras muito grandes (colunas de texto extenso)
      if (largura_sugerida > 50) {
        30 # Largura máxima para colunas muito longas
      } else if (largura_sugerida < 8) {
        10 # Largura mínima
      } else {
        min(largura_sugerida + 2, col_width_def) # Adicionar padding
      }
    })

    openxlsx::setColWidths(wb, sheet = nome_aba, cols = seq_len(ncol(df_dados)), widths = larguras_calculadas)

    # Colunas de texto (alinhamento à esquerda, sem quebra de texto)
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

    # Larguras específicas (aplicadas por último para garantir precedência)
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

    # Estilo para valores monetários (baseado no argumento 'col_monetary' ou inferido)
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

    # Estilo para datas (baseado no argumento 'col_dates' ou inferido)
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

    # Estilo para data e hora (baseado no tipo da coluna)
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

    # Estilo específico para colunas com quebra de texto (baseado no argumento 'col_clip')
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

  # Criar diretório se não existir
  dir.create(dirname(caminho_completo), showWarnings = FALSE, recursive = TRUE)

  # Salvar workbook
  openxlsx::saveWorkbook(wb, caminho_completo, overwrite = TRUE)

  message(sprintf("Planilha salva em: %s", caminho_completo))

  return(caminho_completo)
}

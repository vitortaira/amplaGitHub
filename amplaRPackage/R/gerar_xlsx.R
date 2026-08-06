#' Gera planilha Excel personalizada
#'
#' @description
#' Função genérica para criar planilhas Excel com formatação padronizada.
#' Pode receber um tibble/dataframe (para uma aba) ou uma lista de tibbles
#' (para múltiplas abas). A formatação é baseada nos tipos das colunas e
#' argumentos específicos para colunas monetárias e com quebra de texto.
#'
#' @param data Um tibble/dataframe ou uma lista nomeada de tibbles/dataframes
#' @param wb_load Caminho opcional para arquivo de template a ser copiado
#'   e carregado. Se vazio, cria um novo workbook
#' @param tab_names Vetor de strings com nomes das abas. Se NULL, usa nomes
#'   da lista ou "Dados" para tibble único
#' @param tab_colours Vetor nomeado com cores para as abas. Os nomes devem
#'   corresponder aos nomes das abas em `tab_names`. Cores podem ser nomes
#'   de cores do Excel (ex: "red", "blue", "purple") ou códigos
#'   hexadecimais. Se NULL, não aplica cores.
#' @param col_width_def Largura padrão das colunas (valor numérico).
#'   Padrão: 18
#' @param col_width_spec Vetor nomeado com larguras específicas para
#'   colunas por nome
#' @param col_width_auto Vetor com nomes de colunas que devem ter largura
#'   ajustada automaticamente ao conteúdo
#' @param col_headers Lista customizando estilos de cabeçalhos por aba e
#'   coluna.
#'   Formato: list(aba = list(coluna = list(colour = "cor",
#'   font_colour = "cor", font_size = tam, wrapText = TRUE/FALSE))).
#'   Propriedades suportadas: colour (cor de fundo),
#'   font_colour (cor da fonte), font_size (tamanho da fonte),
#'   wrapText (quebra de texto, padrão FALSE)
#' @param col_groups Lista definindo grupos de colunas por aba.
#'   Formato: list(aba = list(list(cols = c("col1", "col2"),
#'   hidden = FALSE, level = 1)))
#' @param tab_freeze Vetor nomeado definindo onde congelar painéis por aba.
#'   Formato: c(aba = "nome_coluna"). A coluna especificada será a última
#'   congelada.
#' @param col_monetary Vetor com nomes de colunas que devem ser formatadas
#'   como valores monetários (com decimais). Se NULL, infere
#'   automaticamente baseado no tipo das colunas (numeric, excl. integer)
#' @param col_dates Vetor com nomes de colunas que devem ser formatadas
#'   como datas. Se NULL, infere automaticamente baseado no tipo das
#'   colunas (Date)
#' @param col_clip Vetor com nomes de colunas de texto que devem ter
#'   quebra de texto habilitada
#' @param col_align Lista nomeada especificando alinhamento horizontal por
#'   coluna. Formato: c(col1 = "left", col2 = "center", col3 = "right").
#'   Se NULL, usa alinhamento padrão (center para numéricos, left para
#'   texto).
#' @param tab_zoom Vetor nomeado com nível de zoom (em porcentagem) para
#'   cada aba. Formato: c(aba1 = 80, aba2 = 100). Se NULL, usa zoom de
#'   80% como padrão para todas as abas. Valores entre 10 e 400.
#' @param col_formulas Lista nomeada por aba, contendo listas nomeadas
#'   por coluna com a formula Excel (em notacao inglesa, sem '=').
#'   Formato: list(aba = list(coluna = "VLOOKUP(...)")).
#'   As formulas substituem os valores da coluna no Excel.
#' @param table Booleano. Se TRUE, converte os dados em tabelas Excel.
#'   Padrão: TRUE.
#' @param cell_values Lista nomeada por aba, contendo listas nomeadas por
#'   referencia de celula (ex: "A1") com o valor escalar a gravar.
#'   Formato: list(aba = list(A1 = valor, B2 = valor)). Se a aba nao
#'   existir no workbook, ela e criada. Os valores sao gravados apos o
#'   processamento das abas e antes de salvar. Util para preencher celulas
#'   avulsas (ex: data de fechamento). Se NULL, nada e gravado.
#' @param save Lista com 2 elementos: (1) nome do arquivo, (2) caminho
#'   de destino. Se NULL, salva automaticamente no diretório Downloads
#'   com nome "xlsx-YYYY_MM_DD-HH_MM_SS.xlsx"
#'
#' @return Caminho do arquivo salvo
#' @importFrom dplyr bind_rows mutate select rename case_when if_else
#'   arrange all_of
#' @importFrom stringr str_c
#' @importFrom openxlsx2 wb_workbook wb_load wb_add_worksheet wb_add_data
#' @importFrom openxlsx2 wb_add_data_table wb_add_formula wb_save
#' @importFrom openxlsx2 wb_set_col_widths
#' @importFrom openxlsx2 wb_freeze_pane wb_add_named_region
#' @importFrom openxlsx2 wb_set_grid_lines wb_group_cols
#' @importFrom openxlsx2 wb_get_tables wb_remove_tables
#' @importFrom openxlsx2 wb_remove_worksheet wb_set_order
#' @importFrom openxlsx2 wb_get_sheet_names wb_set_sheetview
#' @importFrom openxlsx2 wb_add_border wb_add_font wb_add_fill
#' @importFrom openxlsx2 wb_add_cell_style wb_add_numfmt wb_color int2col
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
                       col_formulas = NULL,
                       table = TRUE,
                       cell_values = NULL,
                       save = NULL) {
  # Helper: converter cor (nome ou hex) para wb_color
  cor_para_wb <- function(cor) {
    if (startsWith(cor, "#")) {
      openxlsx2::wb_color(hex = paste0("FF", sub("^#", "", cor)))
    } else {
      openxlsx2::wb_color(cor)
    }
  }

  # Helper: construir string de dims "A1:Z99" a partir de rows/cols
  criar_dims <- function(linhas, colunas) {
    paste0(
      openxlsx2::int2col(min(colunas)), min(linhas), ":",
      openxlsx2::int2col(max(colunas)), max(linhas)
    )
  }

  # Validação e preparação dos dados ----
  if (is.data.frame(data)) {
    dados_lista <- list(data)
    if (is.null(tab_names)) {
      tab_names <- "Dados"
    }
  } else if (is.list(data) && all(sapply(data, is.data.frame))) {
    dados_lista <- data
    if (is.null(tab_names)) {
      if (is.null(names(data))) {
        tab_names <- paste0("Aba", seq_along(data))
      } else {
        tab_names <- names(data)
      }
    }
  } else {
    stop(
      "'data' deve ser um tibble/dataframe ou ",
      "uma lista de tibbles/dataframes"
    )
  }

  if (length(tab_names) != length(dados_lista)) {
    stop(
      "O número de nomes em 'tab_names' deve corresponder ",
      "ao número de elementos em 'data'"
    )
  }

  # Criar ou carregar workbook ----
  usar_template <- !is.null(wb_load)

  if (usar_template) {
    if (!file.exists(wb_load)) {
      stop("Arquivo template não encontrado: ", wb_load)
    }

    if (!is.null(save)) {
      if (length(save) != 2) {
        stop(
          "'save' deve ser uma lista com 2 elementos: ",
          "nome do arquivo e caminho"
        )
      }

      nome_arquivo <- save[[1]]
      caminho_destino <- save[[2]]

      if (!is.character(nome_arquivo) ||
        length(nome_arquivo) != 1) {
        stop("O nome do arquivo deve ser uma string")
      }
      if (!is.character(caminho_destino) ||
        length(caminho_destino) != 1) {
        stop("O caminho de destino deve ser uma string")
      }

      caminho_arquivo_temp <- file.path(
        caminho_destino, nome_arquivo
      )
      dir.create(
        dirname(caminho_arquivo_temp),
        showWarnings = FALSE, recursive = TRUE
      )
      file.copy(
        wb_load, caminho_arquivo_temp,
        overwrite = TRUE
      )
      wb <- openxlsx2::wb_load(caminho_arquivo_temp)
    } else {
      wb <- openxlsx2::wb_load(wb_load)
    }
    ordem_template <- unname(
      openxlsx2::wb_get_sheet_names(wb)
    )
  } else {
    wb <- openxlsx2::wb_workbook()
    ordem_template <- NULL
  }

  names(dados_lista) <- tab_names
  message(sprintf(
    "gerar_xlsx: Processando %d abas...", length(dados_lista)
  ))

  # Fontes de tabelas dinamicas do template ----
  # Templates com tabelas dinamicas guardam a fonte em
  # pivotCacheDefinition como <worksheetSource name="X"/>, onde X
  # e o nome de uma tabela Excel (ou intervalo nomeado). Se a
  # tabela criada nao tiver exatamente esse nome, a pivot nao
  # resolve a fonte e o Excel abre o arquivo em modo de reparo.
  # Detectamos esses nomes para renomear as tabelas de forma
  # que casem com a fonte esperada.
  nomes_fonte_pivot <- character(0)
  if (usar_template) {
    nomes_fonte_pivot <- tryCatch(
      {
        arquivos_zip <- utils::unzip(
          wb_load,
          list = TRUE
        )$Name
        pc_arquivos <- grep(
          "^xl/pivotCache/pivotCacheDefinition\\d+\\.xml$",
          arquivos_zip,
          value = TRUE
        )
        fontes <- character(0)
        if (length(pc_arquivos) > 0) {
          tmpdir_pc <- tempfile("pivotcache")
          dir.create(tmpdir_pc)
          on.exit(
            unlink(tmpdir_pc, recursive = TRUE),
            add = TRUE
          )
          utils::unzip(
            wb_load,
            files = pc_arquivos, exdir = tmpdir_pc
          )
          for (pcf in pc_arquivos) {
            conteudo <- paste(
              readLines(
                file.path(tmpdir_pc, pcf),
                warn = FALSE
              ),
              collapse = ""
            )
            ocorrencias <- regmatches(
              conteudo,
              gregexpr(
                'worksheetSource[^>]*name="([^"]+)"',
                conteudo,
                perl = TRUE
              )
            )[[1]]
            nomes <- sub(
              '.*name="([^"]+)".*', "\\1", ocorrencias
            )
            fontes <- c(fontes, nomes)
          }
        }
        unique(fontes)
      },
      error = function(e) character(0)
    )
  }

  # Colunas calculadas para pos-processamento
  formulas_calculadas <- list()

  # Processar cada aba ----
  for (i in seq_along(dados_lista)) {
    df_dados <- dados_lista[[i]]
    nome_aba <- names(dados_lista)[i]

    # Remover caracteres de controle ilegais no XML (OOXML) ----
    # Caracteres 0x00-0x08, 0x0B, 0x0C, 0x0E-0x1F (exceto tab,
    # LF e CR) sao invalidos no XML e causam "xml import
    # unsuccessful" ao openxlsx2 reprocessar a planilha. Podem
    # vir de textos extraidos de PDFs/extratos bancarios.
    colunas_texto <- which(
      vapply(df_dados, is.character, logical(1))
    )
    for (j in colunas_texto) {
      df_dados[[j]] <- gsub(
        "[\u0001-\u0008\u000B\u000C\u000E-\u001F]",
        "", df_dados[[j]],
        perl = TRUE
      )
    }

    # Inferir colunas monetárias (numeric não-integer)
    # Variáveis locais por aba para evitar acúmulo entre abas
    colunas_numericas <- colnames(df_dados)[
      sapply(
        df_dados,
        function(x) is.numeric(x) & !is.integer(x)
      )
    ]
    col_monetary_aba <- unique(c(
      col_monetary,
      setdiff(colunas_numericas, c(col_dates, col_clip))
    ))

    # Inferir colunas de data (local por aba)
    colunas_data <- colnames(df_dados)[
      sapply(df_dados, inherits, "Date")
    ]
    col_dates_aba <- unique(c(
      col_dates,
      setdiff(colunas_data, c(col_monetary_aba, col_clip))
    ))

    # Controle de abas e tabelas existentes ----
    nome_tabela_original <- NULL
    aba_tem_tabela_existente <- FALSE
    tabelas_existentes <- character(0)

    if (usar_template) {
      abas_existentes <- unname(
        openxlsx2::wb_get_sheet_names(wb)
      )
      nome_aba_normalizado <- stringr::str_remove_all(
        tolower(nome_aba), "[._]"
      )
      abas_normalizadas <- stringr::str_remove_all(
        tolower(abas_existentes), "[._]"
      )
      indice_match <- match(
        nome_aba_normalizado, abas_normalizadas
      )

      if (!is.na(indice_match)) {
        nome_aba_template <- abas_existentes[indice_match]
        if (nome_aba != nome_aba_template) {
          nome_aba <- nome_aba_template
        }

        # Capturar tabelas existentes
        tabelas_info <- tryCatch(
          openxlsx2::wb_get_tables(
            wb,
            sheet = nome_aba
          ),
          error = function(e) NULL
        )
        if (is.data.frame(tabelas_info) &&
          nrow(tabelas_info) > 0) {
          nome_tabela_original <- tabelas_info$tab_name[1]
          tabelas_existentes <- tabelas_info$tab_name
          aba_tem_tabela_existente <- TRUE
        }

        if (table && aba_tem_tabela_existente) {
          # Remover tabelas existentes sem recriar a aba
          # (evita corrupcao de XML no workbook)
          for (tabela in tabelas_existentes) {
            wb <- openxlsx2::wb_remove_tables(
              wb,
              sheet = nome_aba, table = tabela
            )
          }
          # Limpar named regions desta aba para evitar
          # conflito com a nova tabela Excel
          tryCatch(
            {
              nrs <- openxlsx2::wb_get_named_regions(wb)
              if (is.data.frame(nrs) && nrow(nrs) > 0) {
                nrs_aba <- character(0)
                if ("sheet" %in% names(nrs)) {
                  nrs_aba <- nrs$name[
                    !is.na(nrs$sheet) &
                      nrs$sheet == nome_aba
                  ]
                }
                for (nr in nrs_aba) {
                  wb <- openxlsx2::wb_remove_named_region(
                    wb,
                    name = nr
                  )
                }
              }
            },
            error = function(e) NULL
          )
          wb <- openxlsx2::wb_set_grid_lines(
            wb,
            sheet = nome_aba, show = FALSE
          )
        } else if (table && !aba_tem_tabela_existente) {
          # Aba sem tabelas (pode ter named regions do
          # template antigo): limpar named regions para
          # evitar conflito com a nova tabela Excel,
          # sem remover a aba (evita corrupção de XML)
          tryCatch(
            {
              nrs <- openxlsx2::wb_get_named_regions(wb)
              if (is.data.frame(nrs) && nrow(nrs) > 0) {
                # Filtrar named regions desta aba
                nrs_aba <- character(0)
                if ("sheet" %in% names(nrs)) {
                  nrs_aba <- nrs$name[
                    !is.na(nrs$sheet) &
                      nrs$sheet == nome_aba
                  ]
                }
                for (nr in nrs_aba) {
                  wb <- openxlsx2::wb_remove_named_region(
                    wb,
                    name = nr
                  )
                }
              }
            },
            error = function(e) NULL
          )
          wb <- openxlsx2::wb_set_grid_lines(
            wb,
            sheet = nome_aba, show = FALSE
          )
        } else if (!table && aba_tem_tabela_existente) {
          # Remover tabelas mas manter aba
          wb <- openxlsx2::wb_set_grid_lines(
            wb,
            sheet = nome_aba, show = FALSE
          )
          for (tabela in tabelas_existentes) {
            wb <- openxlsx2::wb_remove_tables(
              wb,
              sheet = nome_aba, table = tabela
            )
          }
        } else {
          wb <- openxlsx2::wb_set_grid_lines(
            wb,
            sheet = nome_aba, show = FALSE
          )
        }
      } else {
        # Aba não existe no template - criar nova
        cor_aba <- if (!is.null(tab_colours) &&
          nome_aba %in% names(tab_colours)) {
          tab_colours[nome_aba]
        } else {
          NULL
        }
        wb <- openxlsx2::wb_add_worksheet(
          wb,
          sheet = nome_aba,
          grid_lines = FALSE, tab_color = cor_aba
        )
      }
    } else {
      # Novo workbook - criar aba
      cor_aba <- if (!is.null(tab_colours) &&
        nome_aba %in% names(tab_colours)) {
        tab_colours[nome_aba]
      } else {
        NULL
      }
      wb <- openxlsx2::wb_add_worksheet(
        wb,
        sheet = nome_aba,
        grid_lines = FALSE, tab_color = cor_aba
      )
    }

    # Nomes de colunas únicos (case-insensitive) ----
    nomes_colunas <- colnames(df_dados)
    nomes_lower <- tolower(nomes_colunas)
    if (anyDuplicated(nomes_lower)) {
      nomes_unicos <- make.unique(nomes_lower, sep = "_")
      for (j in seq_along(nomes_colunas)) {
        if (nomes_lower[j] != nomes_unicos[j]) {
          sufixo <- sub(nomes_lower[j], "", nomes_unicos[j])
          nomes_colunas[j] <- paste0(
            nomes_colunas[j], sufixo
          )
        }
      }
      colnames(df_dados) <- nomes_colunas
    }

    # Sanitizar datas < 1900-01-01 (Excel nao suporta)
    data_limite <- as.Date("1900-01-01")
    for (j in seq_along(df_dados)) {
      col_val <- df_dados[[j]]
      if (inherits(col_val, "Date")) {
        invalidas <- !is.na(col_val) & col_val < data_limite
        if (any(invalidas)) {
          df_dados[[j]][invalidas] <- NA
        }
      } else if (inherits(col_val, "POSIXct")) {
        invalidas <- !is.na(col_val) &
          as.Date(col_val) < data_limite
        if (any(invalidas)) {
          df_dados[[j]][invalidas] <- NA
        }
      }
    }

    # Guardar info de formulas para aplicar APOS escrever dados ----
    # (injetar formulas inline causa t="str" no XML, que
    #  Excel considera corrupto e dispara reparo)
    formulas_pendentes <- list()
    if (!is.null(col_formulas) &&
      nome_aba %in% names(col_formulas)) {
      formulas_aba <- col_formulas[[nome_aba]]
      for (nome_coluna in names(formulas_aba)) {
        if (nome_coluna %in% colnames(df_dados) &&
          nrow(df_dados) > 0) {
          col_pos <- which(
            colnames(df_dados) == nome_coluna
          )
          formulas_pendentes[[nome_coluna]] <- list(
            col = col_pos,
            formula = formulas_aba[[nome_coluna]]
          )
          # Limpar coluna para evitar conflito no XML
          df_dados[[nome_coluna]] <- NA_character_
        }
      }
    }

    n_linhas <- nrow(df_dados)
    n_colunas <- ncol(df_dados)
    message(sprintf(
      "  [%d/%d] %s (%d linhas x %d colunas)...",
      i, length(dados_lista), nome_aba, n_linhas, n_colunas
    ))
    t_aba_inicio <- Sys.time()

    # Escrever dados ----
    if (table) {
      if (!is.null(nome_tabela_original) &&
        aba_tem_tabela_existente) {
        nome_tabela <- nome_tabela_original
      } else {
        # Se houver uma fonte de tabela dinamica cujo nome
        # corresponda a esta aba (ignorando maiusculas e os
        # separadores "._"), usar esse nome na tabela Excel
        # para que a pivot resolva a fonte e o Excel nao abra
        # o arquivo em modo de reparo.
        nome_aba_norm_pivot <- stringr::str_remove_all(
          tolower(nome_aba), "[._]"
        )
        fonte_correspondente <- nomes_fonte_pivot[
          stringr::str_remove_all(
            tolower(nomes_fonte_pivot), "[._]"
          ) == nome_aba_norm_pivot
        ]
        if (length(fonte_correspondente) > 0) {
          nome_tabela <- fonte_correspondente[1]
        } else {
          nome_tabela <- paste0("t_", nome_aba)
        }
      }
      wb <- openxlsx2::wb_add_data_table(
        wb,
        sheet = nome_aba,
        x = df_dados,
        table_name = nome_tabela,
        table_style = "TableStyleLight1",
        with_filter = TRUE,
        first_column = FALSE,
        banded_rows = TRUE,
        na.strings = ""
      )
    } else {
      wb <- openxlsx2::wb_add_data(
        wb,
        sheet = nome_aba, x = df_dados,
        na.strings = ""
      )

      # Região nomeada (só para novos workbooks)
      if (!usar_template) {
        nome_regiao <- tolower(nome_aba)
        dims_regiao <- criar_dims(
          1:(n_linhas + 1), seq_len(n_colunas)
        )
        wb <- openxlsx2::wb_add_named_region(
          wb,
          sheet = nome_aba,
          dims = dims_regiao, name = nome_regiao
        )
      }
    }
    # Formulas de colunas calculadas ----
    # Estrategia: (1) wb_add_formula() para escrever <f> nas
    # celulas, (2) remover t="str" do XML interno (openxlsx2
    # sempre gera t="str" em formulas, mas sem <v> isso e
    # invalido e dispara reparo no Excel), (3) pos-save:
    # injetar <calculatedColumnFormula> no table XML.
    if (table && length(formulas_pendentes) > 0 &&
      n_linhas > 0) {
      sheet_idx <- which(
        wb$get_sheet_names() == nome_aba
      )
      for (nome_coluna in names(formulas_pendentes)) {
        info <- formulas_pendentes[[nome_coluna]]
        col_letra <- openxlsx2::int2col(info$col)
        # Converter [@[col]] -> tabela[[#This Row],[col]]
        # (o atalho @ nao e valido no XML do OOXML, apenas
        # na interface do Excel)
        formula_xml <- gsub(
          "\\[@",
          paste0(nome_tabela, "[[#This Row],"),
          info$formula,
          perl = TRUE
        )
        # Registrar para calculatedColumnFormula (pos-save)
        formulas_calculadas <- c(
          formulas_calculadas,
          list(list(
            tabela = nome_tabela,
            coluna = nome_coluna,
            formula = info$formula
          ))
        )
        # Aplicar formula nas celulas
        dims_formula <- sprintf(
          "%s2:%s%d",
          col_letra, col_letra, n_linhas + 1
        )
        wb <- openxlsx2::wb_add_formula(
          wb,
          sheet = nome_aba,
          x = rep(formula_xml, n_linhas),
          dims = dims_formula
        )
      }
      # Corrigir t="str" gerado por wb_add_formula
      # (celula com <f> sem <v> nao deve ter t="str")
      cc <- wb$worksheets[[sheet_idx]]$sheet_data$cc
      idx_fix <- which(
        !is.na(cc$f) & nchar(cc$f) > 0 &
          (is.na(cc$v) | nchar(cc$v) == 0)
      )
      if (length(idx_fix) > 0) {
        cc$c_t[idx_fix] <- ""
        wb$worksheets[[sheet_idx]]$sheet_data$cc <- cc
      }
    }
    # Estilos básicos quando table=FALSE ----
    if (!table) {
      dims_todos <- criar_dims(
        1:(n_linhas + 1), seq_len(n_colunas)
      )

      # Bordas em todas as células
      wb <- openxlsx2::wb_add_border(
        wb,
        sheet = nome_aba, dims = dims_todos
      )

      # Alinhamento centralizado
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = nome_aba, dims = dims_todos,
        horizontal = "center", vertical = "center",
        wrap_text = "0"
      )

      # Cabeçalho padrão (colunas sem estilo customizado)
      colunas_com_estilo_custom <- c()
      if (!is.null(col_headers) &&
        nome_aba %in% names(col_headers)) {
        colunas_com_estilo_custom <- intersect(
          names(col_headers[[nome_aba]]),
          colnames(df_dados)
        )
      }

      colunas_padrao <- which(
        !(colnames(df_dados) %in% colunas_com_estilo_custom)
      )
      if (length(colunas_padrao) > 0) {
        for (col_p in colunas_padrao) {
          dims_hdr <- paste0(
            openxlsx2::int2col(col_p), "1"
          )
          wb <- openxlsx2::wb_add_border(
            wb,
            sheet = nome_aba, dims = dims_hdr
          )
          wb <- openxlsx2::wb_add_font(
            wb,
            sheet = nome_aba, dims = dims_hdr,
            bold = TRUE, size = "11"
          )
          wb <- openxlsx2::wb_add_fill(
            wb,
            sheet = nome_aba, dims = dims_hdr,
            color = openxlsx2::wb_color("lightgray")
          )
          wb <- openxlsx2::wb_add_cell_style(
            wb,
            sheet = nome_aba, dims = dims_hdr,
            horizontal = "center", vertical = "center",
            wrap_text = "0"
          )
        }
      }
    }

    # Cabeçalhos customizados (sempre) ----
    if (!is.null(col_headers) &&
      nome_aba %in% names(col_headers)) {
      headers_aba <- col_headers[[nome_aba]]
      for (nome_coluna in names(headers_aba)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(
            colnames(df_dados) == nome_coluna
          )
          config <- headers_aba[[nome_coluna]]

          cor_fundo <- if (!is.null(config$colour)) {
            config$colour
          } else {
            "lightgray"
          }
          cor_fonte <- config$font_colour
          tamanho_fonte <- if (!is.null(config$font_size)) {
            config$font_size
          } else {
            11
          }
          wrap_padrao <- if (!is.null(config$wrapText)) {
            config$wrapText
          } else {
            FALSE
          }

          dims_hdr <- paste0(
            openxlsx2::int2col(col_pos), "1"
          )

          wb <- openxlsx2::wb_add_border(
            wb,
            sheet = nome_aba, dims = dims_hdr
          )

          if (!is.null(cor_fonte)) {
            wb <- openxlsx2::wb_add_font(
              wb,
              sheet = nome_aba, dims = dims_hdr,
              bold = TRUE,
              size = as.character(tamanho_fonte),
              color = cor_para_wb(cor_fonte)
            )
          } else {
            wb <- openxlsx2::wb_add_font(
              wb,
              sheet = nome_aba, dims = dims_hdr,
              bold = TRUE,
              size = as.character(tamanho_fonte)
            )
          }

          wb <- openxlsx2::wb_add_fill(
            wb,
            sheet = nome_aba, dims = dims_hdr,
            color = cor_para_wb(cor_fundo)
          )

          wb <- openxlsx2::wb_add_cell_style(
            wb,
            sheet = nome_aba, dims = dims_hdr,
            horizontal = "center", vertical = "center",
            wrap_text = if (wrap_padrao) "1" else "0"
          )
        }
      }
    }

    # Congelar painel (sempre) ----
    if (!is.null(tab_freeze) &&
      nome_aba %in% names(tab_freeze)) {
      col_congelar <- tab_freeze[[nome_aba]]
      if (col_congelar %in% colnames(df_dados)) {
        col_pos <- which(
          colnames(df_dados) == col_congelar
        )
        wb <- openxlsx2::wb_freeze_pane(
          wb,
          sheet = nome_aba,
          first_active_row = 2,
          first_active_col = col_pos + 1
        )
      } else {
        wb <- openxlsx2::wb_freeze_pane(
          wb,
          sheet = nome_aba, first_row = TRUE
        )
      }
    } else {
      wb <- openxlsx2::wb_freeze_pane(
        wb,
        sheet = nome_aba, first_row = TRUE
      )
    }

    # Larguras de colunas (sempre) ----
    larguras_calculadas <- sapply(
      seq_len(n_colunas),
      function(col_idx) {
        col_data <- df_dados[[col_idx]]
        col_name <- names(df_dados)[col_idx]
        max_nome <- nchar(col_name)

        # col_width_auto: permitir largura maior sem cap
        eh_auto <- !is.null(col_width_auto) &&
          col_name %in% col_width_auto
        largura_max <- if (eh_auto) 60 else col_width_def

        if (is.character(col_data) || is.factor(col_data)) {
          valores <- as.character(col_data)[
            !is.na(col_data)
          ]
          # Amostrar para colunas grandes
          if (length(valores) > 500) {
            valores <- c(
              head(valores, 250), tail(valores, 250)
            )
          }
          max_dados <- if (length(valores) > 0) {
            max(nchar(valores), na.rm = TRUE)
          } else {
            0
          }
        } else {
          valores <- col_data[!is.na(col_data)]
          # Amostrar para colunas grandes
          if (length(valores) > 500) {
            valores <- c(
              head(valores, 250), tail(valores, 250)
            )
          }
          max_dados <- if (length(valores) > 0) {
            max(nchar(format(valores)), na.rm = TRUE)
          } else {
            0
          }
        }

        largura <- max(max_nome, max_dados, na.rm = TRUE)

        if (is.infinite(largura) || largura == 0) {
          max_nome + 2
        } else if (largura > 50) {
          30
        } else if (largura < 8) {
          10
        } else {
          min(largura + 2, largura_max)
        }
      }
    )

    wb <- openxlsx2::wb_set_col_widths(
      wb,
      sheet = nome_aba,
      cols = seq_len(n_colunas),
      widths = larguras_calculadas
    )

    # Colunas de texto: alinhamento à esquerda (sempre) ----
    colunas_texto <- which(
      sapply(
        df_dados,
        function(x) is.character(x) | is.factor(x)
      )
    )
    if (length(colunas_texto) > 0 && n_linhas > 0) {
      dims_texto <- paste0(
        openxlsx2::int2col(colunas_texto), "2:",
        openxlsx2::int2col(colunas_texto), n_linhas + 1
      )
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = nome_aba,
        dims = paste(dims_texto, collapse = ","),
        horizontal = "left", wrap_text = "0"
      )
    }

    # Alinhamento customizado por coluna (sempre) ----
    if (!is.null(col_align) && n_linhas > 0) {
      for (nome_coluna in names(col_align)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(
            colnames(df_dados) == nome_coluna
          )
          align_tipo <- col_align[[nome_coluna]]
          dims_col <- paste0(
            openxlsx2::int2col(col_pos), "2:",
            openxlsx2::int2col(col_pos), n_linhas + 1
          )
          wb <- openxlsx2::wb_add_cell_style(
            wb,
            sheet = nome_aba, dims = dims_col,
            horizontal = align_tipo, vertical = "center"
          )
        }
      }
    }

    # Larguras específicas (sempre) ----
    if (!is.null(col_width_spec)) {
      for (nome_coluna in names(col_width_spec)) {
        if (nome_coluna %in% colnames(df_dados)) {
          col_pos <- which(
            colnames(df_dados) == nome_coluna
          )
          wb <- openxlsx2::wb_set_col_widths(
            wb,
            sheet = nome_aba,
            cols = col_pos,
            widths = col_width_spec[nome_coluna]
          )
        }
      }
    }

    # Larguras automáticas já calculadas em larguras_calculadas

    # Valores monetários (sempre) ----
    if (length(col_monetary_aba) > 0 && n_linhas > 0) {
      colunas_monetarias <- which(
        colnames(df_dados) %in% col_monetary_aba
      )
      if (length(colunas_monetarias) > 0) {
        dims_monetario <- paste0(
          openxlsx2::int2col(colunas_monetarias), "2:",
          openxlsx2::int2col(colunas_monetarias),
          n_linhas + 1
        )
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          sheet = nome_aba,
          dims = paste(dims_monetario, collapse = ","),
          numfmt = "#,##0.00"
        )
      }
    }

    # Valores inteiros (sempre) ----
    colunas_inteiras <- which(sapply(df_dados, is.integer))
    if (length(colunas_inteiras) > 0 && n_linhas > 0) {
      dims_inteiro <- paste0(
        openxlsx2::int2col(colunas_inteiras), "2:",
        openxlsx2::int2col(colunas_inteiras),
        n_linhas + 1
      )
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        sheet = nome_aba,
        dims = paste(dims_inteiro, collapse = ","),
        numfmt = "#,##0"
      )
    }

    # Datas (sempre) ----
    if (length(col_dates_aba) > 0 && n_linhas > 0) {
      colunas_data_idx <- which(
        colnames(df_dados) %in% col_dates_aba
      )
      if (length(colunas_data_idx) > 0) {
        dims_data <- paste0(
          openxlsx2::int2col(colunas_data_idx), "2:",
          openxlsx2::int2col(colunas_data_idx),
          n_linhas + 1
        )
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          sheet = nome_aba,
          dims = paste(dims_data, collapse = ","),
          numfmt = "DD/MM/YYYY"
        )
      }
    }

    # Data e hora (sempre) ----
    colunas_datetime <- which(sapply(
      df_dados,
      function(x) inherits(x, "POSIXct") | inherits(x, "POSIXt")
    ))
    if (length(colunas_datetime) > 0 && n_linhas > 0) {
      dims_datetime <- paste0(
        openxlsx2::int2col(colunas_datetime), "2:",
        openxlsx2::int2col(colunas_datetime),
        n_linhas + 1
      )
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        sheet = nome_aba,
        dims = paste(dims_datetime, collapse = ","),
        numfmt = "YYYY-MM-DD HH:MM:SS"
      )
    }

    # Quebra de texto - col_clip (sempre) ----
    if (!is.null(col_clip) && n_linhas > 0) {
      colunas_clip <- which(
        colnames(df_dados) %in% col_clip
      )
      if (length(colunas_clip) > 0) {
        dims_clip <- paste0(
          openxlsx2::int2col(colunas_clip), "2:",
          openxlsx2::int2col(colunas_clip), n_linhas + 1
        )
        dims_clip_str <- paste(dims_clip, collapse = ",")
        wb <- openxlsx2::wb_add_border(
          wb,
          sheet = nome_aba, dims = dims_clip_str
        )
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          sheet = nome_aba, dims = dims_clip_str,
          horizontal = "left", vertical = "top",
          wrap_text = "1"
        )
      }
    }

    # Agrupar colunas ----
    if (!is.null(col_groups) &&
      nome_aba %in% names(col_groups)) {
      grupos_aba <- col_groups[[nome_aba]]
      for (grupo in grupos_aba) {
        colunas_nomes <- grupo$cols
        colunas_indices <- which(
          colnames(df_dados) %in% colunas_nomes
        )

        if (length(colunas_indices) > 0) {
          col_inicio <- min(colunas_indices)
          col_fim <- max(colunas_indices)

          ocultar <- if (!is.null(grupo$hidden)) {
            grupo$hidden
          } else {
            FALSE
          }
          nivel <- if (!is.null(grupo$level)) {
            grupo$level
          } else {
            NULL
          }

          wb <- openxlsx2::wb_group_cols(
            wb,
            sheet = nome_aba,
            cols = col_inicio:col_fim,
            collapsed = ocultar,
            levels = nivel
          )
        }
      }
    }
    message(sprintf(
      "    [%d/%d] %s concluida (%.1fs)",
      i, length(dados_lista), nome_aba,
      as.numeric(Sys.time() - t_aba_inicio, units = "secs")
    ))
  }

  # Zoom (pós-processamento) ----
  for (i in seq_along(tab_names)) {
    nome_aba <- tab_names[i]

    zoom_valor <- 80 # Padrão
    if (!is.null(tab_zoom)) {
      if (nome_aba %in% names(tab_zoom)) {
        zoom_valor <- tab_zoom[nome_aba]
      } else if (length(tab_zoom) == 1 &&
        is.null(names(tab_zoom))) {
        zoom_valor <- tab_zoom[1]
      }
    }

    zoom_valor <- max(
      10, min(400, as.numeric(zoom_valor))
    )

    wb <- openxlsx2::wb_set_sheetview(
      wb,
      sheet = nome_aba,
      zoom_scale = zoom_valor
    )
  }

  # Reordenar abas (preservar ordem do template) ----
  if (!is.null(ordem_template)) {
    abas_atuais <- unname(
      openxlsx2::wb_get_sheet_names(wb)
    )
    normalizar <- function(x) {
      stringr::str_remove_all(tolower(x), "[._]")
    }
    template_norm <- normalizar(ordem_template)
    atuais_norm <- normalizar(abas_atuais)

    ordem_final <- c(
      abas_atuais[
        match(template_norm, atuais_norm, nomatch = 0)
      ],
      abas_atuais[!atuais_norm %in% template_norm]
    )
    ordem_final <- ordem_final[
      ordem_final != 0 & !is.na(ordem_final)
    ]
    if (length(ordem_final) == length(abas_atuais)) {
      wb <- openxlsx2::wb_set_order(
        wb, match(ordem_final, abas_atuais)
      )
    }
  }

  # Gravar valores em celulas avulsas ----
  if (!is.null(cell_values)) {
    abas_wb <- unname(openxlsx2::wb_get_sheet_names(wb))
    normalizar_cv <- function(x) {
      stringr::str_remove_all(tolower(x), "[._]")
    }
    for (nome_aba_cv in names(cell_values)) {
      idx_cv <- match(
        normalizar_cv(nome_aba_cv), normalizar_cv(abas_wb)
      )
      if (is.na(idx_cv)) {
        wb <- openxlsx2::wb_add_worksheet(
          wb,
          sheet = nome_aba_cv, grid_lines = FALSE
        )
        abas_wb <- unname(openxlsx2::wb_get_sheet_names(wb))
        aba_alvo_cv <- nome_aba_cv
      } else {
        aba_alvo_cv <- abas_wb[idx_cv]
      }
      celulas_cv <- cell_values[[nome_aba_cv]]
      for (ref_celula_cv in names(celulas_cv)) {
        wb <- openxlsx2::wb_add_data(
          wb,
          sheet = aba_alvo_cv,
          x = celulas_cv[[ref_celula_cv]],
          dims = ref_celula_cv
        )
      }
    }
  }

  # Salvar arquivo ----
  if (!is.null(save)) {
    nome_arquivo <- save[[1]]
    caminho_destino <- save[[2]]
    caminho_completo <- file.path(
      caminho_destino, nome_arquivo
    )
  } else {
    timestamp <- format(Sys.time(), "%Y_%m_%d-%H_%M_%S")
    nome_arquivo <- paste0("xlsx-", timestamp, ".xlsx")

    if (Sys.info()["sysname"] == "Windows") {
      caminho_destino <- file.path(
        Sys.getenv("USERPROFILE"), "Downloads"
      )
    } else {
      caminho_destino <- file.path(
        path.expand("~"), "Downloads"
      )
    }

    caminho_completo <- file.path(
      caminho_destino, nome_arquivo
    )
  }

  caminho_completo <- normalizePath(
    caminho_completo,
    winslash = "\\", mustWork = FALSE
  )

  dir.create(
    dirname(caminho_completo),
    showWarnings = FALSE, recursive = TRUE
  )

  # Forcar recalculo completo ao abrir (necessario para formulas injetadas)
  wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'

  message("Salvando arquivo...")
  suppressWarnings(
    openxlsx2::wb_save(wb, caminho_completo, overwrite = TRUE)
  )

  # Restaurar customXml do template (openxlsx2 nao preserva)
  if (usar_template) {
    tryCatch(
      {
        arquivos_zip <- utils::unzip(
          wb_load,
          list = TRUE
        )$Name
        tem_custom <- any(
          grepl("^customXml/", arquivos_zip)
        )

        if (tem_custom) {
          # Usar .NET ZipFile via PowerShell para injetar
          # os arquivos customXml do template no xlsx salvo
          ps_file <- tempfile(fileext = ".ps1")
          on.exit(unlink(ps_file), add = TRUE)

          template_path <- normalizePath(
            wb_load,
            winslash = "\\"
          )
          output_path <- normalizePath(
            caminho_completo,
            winslash = "\\"
          )

          writeLines(c(
            "Add-Type -AssemblyName System.IO.Compression.FileSystem",
            sprintf(
              "$src = [System.IO.Compression.ZipFile]::OpenRead(\"%s\")",
              template_path
            ),
            sprintf(
              "$dst = [System.IO.Compression.ZipFile]::Open(\"%s\", 'Update')",
              output_path
            ),
            # Copiar arquivos customXml do template
            "foreach ($e in $src.Entries) {",
            "  if ($e.FullName -like 'customXml/*' -and $e.Length -gt 0) {",
            "    $x = $dst.GetEntry($e.FullName)",
            "    if ($x) { $x.Delete() }",
            "    $n = $dst.CreateEntry($e.FullName)",
            "    $r = $e.Open(); $w = $n.Open()",
            "    $r.CopyTo($w); $w.Close(); $r.Close()",
            "  }",
            "}",
            # Atualizar [Content_Types].xml com entradas customXml
            "$ctS = $src.GetEntry('[Content_Types].xml')",
            "$ctD = $dst.GetEntry('[Content_Types].xml')",
            "if ($ctS -and $ctD) {",
            "  $r1 = New-Object System.IO.StreamReader($ctS.Open()); [xml]$xS = $r1.ReadToEnd(); $r1.Close()",
            "  $r2 = New-Object System.IO.StreamReader($ctD.Open()); [xml]$xD = $r2.ReadToEnd(); $r2.Close()",
            "  $ns = $xD.DocumentElement.NamespaceURI; $mod = $false",
            "  foreach ($nd in $xS.DocumentElement.ChildNodes) {",
            "    if ($nd.LocalName -eq 'Override' -and $nd.GetAttribute('PartName') -like '/customXml/*') {",
            "      $pn = $nd.GetAttribute('PartName'); $dup = $false",
            "      foreach ($ex in $xD.DocumentElement.ChildNodes) {",
            "        if ($ex.LocalName -eq 'Override' -and $ex.GetAttribute('PartName') -eq $pn) { $dup = $true; break }",
            "      }",
            "      if (-not $dup) {",
            "        $el = $xD.CreateElement('Override', $ns)",
            "        $el.SetAttribute('PartName', $pn)",
            "        $el.SetAttribute('ContentType', $nd.GetAttribute('ContentType'))",
            "        $xD.DocumentElement.AppendChild($el) | Out-Null; $mod = $true",
            "      }",
            "    }",
            "  }",
            "  if ($mod) {",
            "    $ctD.Delete(); $ctN = $dst.CreateEntry('[Content_Types].xml')",
            "    $enc = New-Object System.Text.UTF8Encoding($false)",
            "    $wr = New-Object System.IO.StreamWriter($ctN.Open(), $enc)",
            "    $xD.Save($wr); $wr.Close()",
            "  }",
            "}",
            "$src.Dispose(); $dst.Dispose()"
          ), ps_file)

          system2(
            "powershell",
            args = c(
              "-NoProfile", "-ExecutionPolicy", "Bypass",
              "-File", ps_file
            ),
            stdout = FALSE, stderr = FALSE
          )
          message("  customXml restaurado do template.")
        }
      },
      error = function(e) {
        warning(
          "Nao foi possivel restaurar customXml: ",
          conditionMessage(e)
        )
      }
    )
  }

  # Pos-processamento: corrigir tabelas dinamicas do template ----
  # Dois problemas comuns em templates com tabelas dinamicas
  # fazem o Excel abrir o arquivo em modo de reparo:
  # (1) definedName com referencia quebrada (#REF!), tipicamente
  #     "_xlnm._FilterDatabase" herdado do template;
  # (2) pivotCache com dados defasados apontando para a fonte
  #     antiga. Ao renomear a tabela para casar com a fonte
  #     (feito acima) e marcar o cache com refreshOnLoad="1", o
  #     Excel reconstroi a tabela dinamica a partir dos novos
  #     dados ao abrir, sem disparar reparo.
  if (usar_template) {
    tryCatch(
      {
        ps_file_pv <- tempfile(fileext = ".ps1")
        on.exit(unlink(ps_file_pv), add = TRUE)

        output_path_pv <- normalizePath(
          caminho_completo,
          winslash = "\\"
        )

        writeLines(c(
          "Add-Type -AssemblyName System.IO.Compression.FileSystem",
          sprintf(
            "$zip = [System.IO.Compression.ZipFile]::Open('%s', 'Update')",
            output_path_pv
          ),
          "$enc = New-Object System.Text.UTF8Encoding($false)",
          # (1) Remover definedName com #REF! do workbook.xml
          "$e = $zip.GetEntry('xl/workbook.xml')",
          "if ($e) {",
          "  $sr = New-Object System.IO.StreamReader($e.Open())",
          "  $xml = $sr.ReadToEnd(); $sr.Close()",
          paste0(
            "  $novo = [regex]::Replace($xml, ",
            "'<definedName\\b[^>]*>[^<]*#REF![^<]*</definedName>', '')"
          ),
          "  $novo = $novo -replace '<definedNames>\\s*</definedNames>', ''",
          "  if ($novo -ne $xml) {",
          "    $e.Delete(); $ne = $zip.CreateEntry('xl/workbook.xml')",
          "    $w = New-Object System.IO.StreamWriter($ne.Open(), $enc)",
          "    $w.Write($novo); $w.Close()",
          "  }",
          "}",
          # (2) refreshOnLoad nas pivotCacheDefinition
          "$pcNames = @()",
          "foreach ($en in $zip.Entries) {",
          paste0(
            "  if ($en.FullName -match ",
            "'^xl/pivotCache/pivotCacheDefinition\\d+\\.xml$') ",
            "{ $pcNames += $en.FullName }"
          ),
          "}",
          "foreach ($fn in $pcNames) {",
          "  $en = $zip.GetEntry($fn)",
          "  $sr = New-Object System.IO.StreamReader($en.Open())",
          "  $xml = $sr.ReadToEnd(); $sr.Close()",
          "  if ($xml -notmatch 'refreshOnLoad=') {",
          paste0(
            "    $novo = $xml -replace ",
            "'(<pivotCacheDefinition\\b)', '$1 refreshOnLoad=\"1\"'"
          ),
          "    $en.Delete(); $ne = $zip.CreateEntry($fn)",
          "    $w = New-Object System.IO.StreamWriter($ne.Open(), $enc)",
          "    $w.Write($novo); $w.Close()",
          "  }",
          "}",
          "$zip.Dispose()"
        ), ps_file_pv)

        system2(
          "powershell",
          args = c(
            "-NoProfile", "-ExecutionPolicy", "Bypass",
            "-File", ps_file_pv
          ),
          stdout = FALSE, stderr = FALSE
        )
        message(
          "  Tabelas dinamicas corrigidas ",
          "(#REF removido, refreshOnLoad)."
        )
      },
      error = function(e) {
        warning(
          "Nao foi possivel corrigir tabelas dinamicas: ",
          conditionMessage(e)
        )
      }
    )
  }

  # Pos-processamento: injetar calculatedColumnFormula ----
  # openxlsx2 nao suporta colunas calculadas em tabelas Excel.
  # As celulas ja tem <f> correto (sem t="str") graças ao
  # wb_add_formula + correcao de c_t. Falta apenas adicionar
  # <calculatedColumnFormula> na definicao da coluna no
  # table XML para que Excel reconheca como coluna calculada.
  if (length(formulas_calculadas) > 0) {
    tryCatch(
      {
        ps_file_f <- tempfile(fileext = ".ps1")
        on.exit(unlink(ps_file_f), add = TRUE)

        output_path_f <- normalizePath(
          caminho_completo,
          winslash = "\\"
        )

        ps_lines <- c(
          "Add-Type -AssemblyName System.IO.Compression.FileSystem",
          sprintf(
            "$zip = [System.IO.Compression.ZipFile]::Open('%s', 'Update')",
            output_path_f
          ),
          ""
        )

        for (fix in formulas_calculadas) {
          tbl <- fix$tabela
          col <- fix$coluna
          frm <- fix$formula
          # Converter [@[col]] -> tabela[[#This Row],[col]]
          frm <- gsub(
            "\\[@",
            paste0(tbl, "[[#This Row],"),
            frm,
            perl = TRUE
          )
          # Escapar XML especial na formula
          frm_xml <- gsub("&", "&amp;", frm, fixed = TRUE)
          frm_xml <- gsub("<", "&lt;", frm_xml, fixed = TRUE)
          frm_xml <- gsub(">", "&gt;", frm_xml, fixed = TRUE)

          ps_lines <- c(
            ps_lines,
            sprintf(
              "# --- Tabela: %s, Coluna: %s ---", tbl, col
            ),
            "foreach ($e in @($zip.Entries)) {",
            paste0(
              "  if ($e.FullName -match ",
              "'^xl/tables/table\\d+\\.xml$') {"
            ),
            "    $sr = New-Object System.IO.StreamReader(",
            "      $e.Open())",
            "    $xml = $sr.ReadToEnd(); $sr.Close()",
            sprintf(
              "    if ($xml -match 'displayName=\"%s\"') {",
              tbl
            ),
            sprintf(
              paste0(
                "      $newXml = $xml -replace ",
                "'(<tableColumn[^>]*name=\"%s\"[^/]*)\\s*/>',",
                " ('$1><calculatedColumnFormula>%s",
                "</calculatedColumnFormula></tableColumn>')"
              ),
              col, frm_xml
            ),
            "      if ($newXml -ne $xml) {",
            "        $fn = $e.FullName; $e.Delete()",
            "        $ne = $zip.CreateEntry($fn)",
            paste0(
              "        $enc = New-Object ",
              "System.Text.UTF8Encoding($false)"
            ),
            paste0(
              "        $w = New-Object ",
              "System.IO.StreamWriter($ne.Open(), $enc)"
            ),
            "        $w.Write($newXml); $w.Close()",
            "      }",
            "      break",
            "    }",
            "  }",
            "}",
            ""
          )
        }

        ps_lines <- c(ps_lines, "$zip.Dispose()")
        writeLines(ps_lines, ps_file_f)

        system2(
          "powershell",
          args = c(
            "-NoProfile", "-ExecutionPolicy", "Bypass",
            "-File", ps_file_f
          ),
          stdout = FALSE, stderr = FALSE
        )
        message(
          "  calculatedColumnFormula injetada no table XML."
        )
      },
      error = function(e) {
        warning(
          "Nao foi possivel injetar formulas nas tabelas: ",
          conditionMessage(e)
        )
      }
    )
  }

  message(sprintf("Planilha salva em: %s", caminho_completo))

  return(caminho_completo)
}

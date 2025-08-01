#' Cria mapa de extratos únicos em planilha Excel
#'
#' @description
#' Função que cria uma planilha Excel simples com todas as combinações únicas
#' da tupla (empresa, banco, conta, descricao) encontradas nos extratos
#' consolidados CEF e ITAÚ.
#'
#' @return Caminho do arquivo Excel criado
#' @importFrom dplyr distinct select arrange filter group_by summarise n
#' @importFrom stringr str_c
#' @importFrom openxlsx createWorkbook addWorksheet writeData addStyle createStyle
#' @importFrom openxlsx saveWorkbook setColWidths addFilter freezePane
#' @export
#'
e_extratos_mapa <- function() {
  # Obter dados consolidados de extratos
  extratos <- e_extratos()

  if (nrow(extratos) == 0) {
    message("Nenhum dado de extrato encontrado.")
    return(NULL)
  }

  # Criar mapa com combinações únicas da tupla (empresa, banco, conta, descricao)
  # e incluir os arquivos que contêm cada combinação
  mapaExtratos <- extratos %>%
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

  message(sprintf("Total de combinações únicas encontradas: %d", nrow(mapaExtratos)))

  # Definindo o nome do arquivo dinamicamente
  nomeArquivo <- stringr::str_c(
    "MapaExtratos-",
    format(Sys.time(), "%Y_%m_%d-%H_%M_%S"),
    ".xlsx"
  )

  # Definir caminho do arquivo
  caminhoArquivo <- stringr::str_c(
    caminhos_pastas("extratos"),
    "/Consolidados/",
    nomeArquivo
  )

  # Criar diretório se não existir
  dir.create(
    dirname(caminhoArquivo),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Criar workbook
  wb <- openxlsx::createWorkbook()

  # Adicionar worksheet
  openxlsx::addWorksheet(wb, "MapaExtratos")

  # Escrever os dados
  openxlsx::writeData(wb, sheet = "MapaExtratos", x = mapaExtratos)

  # Estilo do cabeçalho
  openxlsx::addStyle(
    wb,
    sheet = "MapaExtratos",
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

  # Estilo geral (bordas e alinhamento)
  openxlsx::addStyle(
    wb,
    sheet = "MapaExtratos",
    style = openxlsx::createStyle(
      border = "TopBottomLeftRight",
      halign = "left",
      valign = "center"
    ),
    rows = 2:(nrow(mapaExtratos) + 1),
    cols = seq_len(ncol(mapaExtratos)),
    gridExpand = TRUE
  )

  # Definir larguras das colunas
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

  for (i in seq_along(largurasColunas)) {
    openxlsx::setColWidths(
      wb,
      sheet = "MapaExtratos",
      cols = i,
      widths = largurasColunas[i]
    )
  }

  # Adicionar filtro e congelar painel
  openxlsx::addFilter(wb, sheet = "MapaExtratos", rows = 1, cols = seq_len(ncol(mapaExtratos)))
  openxlsx::freezePane(wb, sheet = "MapaExtratos", firstRow = TRUE, firstActiveRow = 2)

  # Estilo específico para a coluna arquivo(s) - texto quebrado
  colunaArquivos <- which(colnames(mapaExtratos) == "arquivo(s)")
  if (length(colunaArquivos) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = "MapaExtratos",
      style = openxlsx::createStyle(
        border = "TopBottomLeftRight",
        halign = "left",
        valign = "top",
        wrapText = TRUE
      ),
      rows = 2:(nrow(mapaExtratos) + 1),
      cols = colunaArquivos,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Estilo para valores monetários
  colunasMonetarias <- which(colnames(mapaExtratos) %in% c("soma.valor", "soma.valor.abs"))
  if (length(colunasMonetarias) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = "MapaExtratos",
      style = openxlsx::createStyle(numFmt = "#,##0.00"),
      rows = 2:(nrow(mapaExtratos) + 1),
      cols = colunasMonetarias,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Salvar planilha
  openxlsx::saveWorkbook(wb, caminhoArquivo, overwrite = TRUE)

  message(sprintf("Mapa de extratos salvo em: %s", caminhoArquivo))

  # Mostrar estatísticas
  if (nrow(mapaExtratos) > 0) {
    empresasUnicas <- unique(mapaExtratos$empresa)
    bancosUnicos <- unique(mapaExtratos$banco)
    contasUnicas <- unique(mapaExtratos$conta)

    message(sprintf("Empresas: %s", paste(empresasUnicas, collapse = ", ")))
    message(sprintf("Bancos: %s", paste(bancosUnicos, collapse = ", ")))
    message(sprintf("Total de contas únicas: %d", length(contasUnicas)))
  }

  return(caminhoArquivo)
}

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

  # Definir larguras específicas das colunas
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

  # Usar gerar_xlsx() para criar a planilha
  caminhoArquivo <- gerar_xlsx(
    data = mapaExtratos,
    tab_names = "MapaExtratos",
    col_width_def = 18,
    col_width_spec = largurasColunas,
    save = list(nomeArquivo, stringr::str_c(caminhos_pastas("extratos"), "/Consolidados"))
  )

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

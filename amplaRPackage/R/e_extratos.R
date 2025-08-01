#' Extrai e consolida dados de extratos CEF e ITAÚ
#'
#' @description
#' Função para extrair e consolidar dados de extratos bancários CEF e ITAÚ,
#' combinando-os em um único tibble padronizado.
#'
#' @return Um tibble com dados consolidados de extratos CEF e ITAÚ, contendo
#'   colunas padronizadas para análise temporal e financeira.
#' @importFrom dplyr bind_rows mutate select rename case_when if_else arrange all_of
#' @importFrom stringr str_detect str_pad str_sub
#' @importFrom tibble tibble as_tibble
#' @export
#'
e_extratos <- function() {
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

  return(extratosConsolidados)
}

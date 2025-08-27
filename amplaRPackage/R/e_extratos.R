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
#'   - fluxosContaMes: tibble com análise de entradas e saídas por conta e mês
#'   - DFC: tibble com dados para Demonstração do Fluxo de Caixa
#' @importFrom dplyr bind_rows mutate select rename case_when if_else arrange all_of
#' @importFrom dplyr distinct group_by summarise n filter
#' @importFrom stringr str_detect str_pad str_sub str_c
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate floor_date
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

  # Criar análise de entradas e saídas por conta e mês
  fluxosContaMes <- tryCatch(
    {
      message("Criando análise de fluxos por conta e mês...")
      message(sprintf("Dados consolidados: %d registros", nrow(extratosConsolidados)))

      # Verificar se há dados válidos
      dados_validos <- extratosConsolidados %>%
        dplyr::filter(!is.na(.data$valor), !is.na(.data$data.movimentacao))

      message(sprintf("Dados válidos (com valor e data): %d registros", nrow(dados_validos)))

      if (nrow(dados_validos) == 0) {
        message("Nenhum dado válido encontrado para análise de fluxos")
        tibble::tibble(
          mes = as.Date(character()),
          identificacao.conta = character(),
          empresa = character(),
          banco = character(),
          conta = character(),
          entradas = numeric(),
          saidas = numeric(),
          saldo.liquido = numeric(),
          qtd.transacoes = integer()
        )
      } else {
        # Criar análise de fluxos
        resultado <- dados_validos %>%
          dplyr::mutate(
            mes = lubridate::floor_date(.data$data.movimentacao, "month"),
            identificacao.conta = paste(.data$empresa, .data$banco, .data$conta, sep = " - ")
          ) %>%
          dplyr::group_by(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta) %>%
          dplyr::summarise(
            entradas = sum(pmax(.data$valor, 0), na.rm = TRUE),
            saidas = sum(pmin(.data$valor, 0), na.rm = TRUE),
            saldo.liquido = sum(.data$valor, na.rm = TRUE),
            qtd.transacoes = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data$mes, .data$empresa, .data$banco, .data$conta)

        message(sprintf("Análise de fluxos criada: %d registros", nrow(resultado)))
        resultado
      }
    },
    error = function(e) {
      message("Erro ao criar análise de fluxos por conta e mês: ", e$message)
      return(tibble::tibble(
        mes = as.Date(character()),
        identificacao.conta = character(),
        empresa = character(),
        banco = character(),
        conta = character(),
        entradas = numeric(),
        saidas = numeric(),
        saldo.liquido = numeric(),
        qtd.transacoes = integer()
      ))
    }
  )

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

    # Criar caminho de destino completo
    caminhoDestino <- stringr::str_c(caminhos_pastas("extratos"), "/Consolidados")

    # Preparar dados para gerar_xlsx
    dadosAbas <- list(
      "Extratos" = extratosConsolidados,
      "Resumo" = mapaExtratos,
      "Fluxos por Conta" = fluxosContaMes
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

    # Definir colunas monetárias
    colunasMonetarias <- c("valor", "soma.valor", "soma.valor.abs", "entradas", "saidas", "saldo.liquido")

    # Definir colunas com quebra de texto
    colunasTexto <- c("arquivo(s)", "descricao")

    # Usar gerar_xlsx para criar o arquivo (sem template para criar abas automaticamente)
    caminhoArquivo <- gerar_xlsx(
      data = dadosAbas,
      wb_load = NULL, # Não usar template aqui para permitir criação automática das abas
      tab_names = names(dadosAbas),
      col_width_def = 18,
      col_width_spec = largurasColunas,
      col_monetary = colunasMonetarias,
      col_clip = colunasTexto,
      save = list(nomeArquivo, caminhoDestino)
    )

    # Carregar o arquivo criado e adicionar abas do template
    wb <- openxlsx::loadWorkbook(caminhoArquivo)
    wbTemplate <- openxlsx::loadWorkbook(caminhoTemplate)

    # Copiar abas do template (DFC, Mapeamento, Configurações)
    for (abaTemplate in names(wbTemplate)) {
      if (!abaTemplate %in% names(wb)) {
        # Criar a aba
        openxlsx::addWorksheet(wb, abaTemplate)

        # Copiar dados da aba do template se existirem
        tryCatch(
          {
            dadosTemplate <- openxlsx::readWorkbook(wbTemplate, sheet = abaTemplate, colNames = FALSE)
            if (nrow(dadosTemplate) > 0) {
              openxlsx::writeData(wb, sheet = abaTemplate, x = dadosTemplate, colNames = FALSE)
            }
          },
          error = function(e) {
            message(sprintf("Não foi possível copiar dados da aba '%s': %s", abaTemplate, e$message))
          }
        )
      }
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
    message("  - Aba 'Fluxos por Conta': Análise de entradas e saídas por conta e mês")
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

  # Criar DFC a partir dos dados CEF (repasse e pj separadamente)
  DFC <- tryCatch(
    {
      # Criar dataframe base com todas as combinações de mes e empresa
      df_base <- dadosCef %>%
        dplyr::mutate(
          mes = lubridate::floor_date(.data$data.movimentacao, "month")
        ) %>%
        dplyr::select(.data$mes, .data$empresa) %>%
        dplyr::distinct()

      # Calcular repasses
      df_repasse <- dadosCef %>%
        dplyr::filter(.data$repasse == TRUE) %>%
        dplyr::group_by(
          mes = lubridate::floor_date(.data$data.movimentacao, "month"),
          .data$empresa
        ) %>%
        dplyr::summarise(
          repasse = sum(.data$valor, na.rm = TRUE),
          .groups = "drop"
        )

      # Calcular PJ
      df_pj <- dadosCef %>%
        dplyr::filter(.data$pj == TRUE) %>%
        dplyr::group_by(
          mes = lubridate::floor_date(.data$data.movimentacao, "month"),
          .data$empresa
        ) %>%
        dplyr::summarise(
          pj = sum(.data$valor, na.rm = TRUE),
          .groups = "drop"
        )

      # Juntar tudo
      df_base %>%
        dplyr::left_join(df_repasse, by = c("mes", "empresa")) %>%
        dplyr::left_join(df_pj, by = c("mes", "empresa")) %>%
        dplyr::mutate(
          repasse = dplyr::coalesce(.data$repasse, 0),
          pj = dplyr::coalesce(.data$pj, 0)
        ) %>%
        dplyr::arrange(.data$mes, .data$empresa)
    },
    error = function(e) {
      message("Erro ao criar DFC: ", e$message)
      return(tibble::tibble(
        mes = as.Date(character()),
        empresa = character(),
        valor = numeric()
      ))
    }
  )

  # Retornar lista com os objetos criados
  return(list(
    extratosConsolidados = extratosConsolidados,
    mapaExtratos = mapaExtratos,
    fluxosContaMes = fluxosContaMes,
    DFC = DFC
  ))
}

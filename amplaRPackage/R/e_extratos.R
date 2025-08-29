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
#'   - extratosIk: tibble com dados CMF extraídos do Informakon
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

  # Obter dados CMF (Informakon)
  dadosIk <- tryCatch(
    {
      if (exists("e_ik_cmf", mode = "function")) {
        e_ik_cmf()
      } else {
        message("Função e_ik_cmf não encontrada. Dados CMF não serão incluídos.")
        tibble::tibble()
      }
    },
    error = function(e) {
      message("Erro ao extrair dados CMF (Informakon): ", e$message)
      return(tibble::tibble())
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
  message(sprintf("Total de registros CMF (Informakon): %d", nrow(dadosIk)))
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
      message("Criando análise de fluxos por conta e mês (comparando extratos consolidados e IK)...")
      message(sprintf("Dados consolidados: %d registros", nrow(extratosConsolidados)))
      message(sprintf("Dados IK: %d registros", nrow(dadosIk)))

      # Verificar se há dados válidos nos extratos consolidados
      dados_validos <- extratosConsolidados %>%
        dplyr::filter(!is.na(.data$valor), !is.na(.data$data.movimentacao))

      # Verificar se há dados válidos nos dados IK
      # Identificar as colunas corretas de valor e data nos dados IK
      coluna_valor_ik <- if ("valor" %in% names(dadosIk)) {
        "valor"
      } else if ("Valor" %in% names(dadosIk)) {
        "Valor"
      } else {
        NULL
      }

      coluna_data_ik <- if ("data.movimento" %in% names(dadosIk)) {
        "data.movimento"
      } else if ("Data.Movimento" %in% names(dadosIk)) {
        "Data.Movimento"
      } else if ("Data" %in% names(dadosIk)) {
        "Data"
      } else {
        NULL
      }

      message(sprintf("Coluna valor IK identificada: %s", if (is.null(coluna_valor_ik)) "NENHUMA" else coluna_valor_ik))
      message(sprintf("Coluna data IK identificada: %s", if (is.null(coluna_data_ik)) "NENHUMA" else coluna_data_ik))

      dados_ik_validos <- if (!is.null(coluna_valor_ik) && !is.null(coluna_data_ik)) {
        dadosIk %>%
          dplyr::filter(!is.na(.data[[coluna_valor_ik]]), !is.na(.data[[coluna_data_ik]]))
      } else {
        tibble::tibble()
      }

      message(sprintf("Dados consolidados válidos (com valor e data): %d registros", nrow(dados_validos)))
      message(sprintf("Dados IK válidos (com valor e data): %d registros", nrow(dados_ik_validos)))

      # Criar análise de fluxos para extratos consolidados
      fluxos_consolidados <- if (nrow(dados_validos) > 0) {
        dados_validos %>%
          dplyr::mutate(
            mes = lubridate::floor_date(.data$data.movimentacao, "month"),
            identificacao.conta = paste(.data$empresa, .data$banco, .data$conta.interno, sep = " - ")
          ) %>%
          dplyr::group_by(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta, .data$conta.interno) %>%
          dplyr::summarise(
            entradas = sum(pmax(.data$valor, 0), na.rm = TRUE),
            saidas = sum(pmin(.data$valor, 0), na.rm = TRUE),
            saldo.liquido = sum(.data$valor, na.rm = TRUE),
            qtd.transacoes = dplyr::n(),
            .groups = "drop"
          )
      } else {
        tibble::tibble(
          mes = as.Date(character()),
          identificacao.conta = character(),
          empresa = character(),
          banco = character(),
          conta = character(),
          conta.interno = character(),
          entradas = numeric(),
          saidas = numeric(),
          saldo.liquido = numeric(),
          qtd.transacoes = integer()
        )
      }

      # Criar análise de fluxos para dados IK
      fluxos_ik <- if (nrow(dados_ik_validos) > 0 && !is.null(coluna_valor_ik) && !is.null(coluna_data_ik)) {
        # Identificar coluna Conta.N nos dados IK para conta.ik
        coluna_conta_n <- if ("Conta.N" %in% names(dados_ik_validos)) {
          "Conta.N"
        } else if ("conta.n" %in% names(dados_ik_validos)) {
          "conta.n"
        } else if ("Conta.Numero" %in% names(dados_ik_validos)) {
          "Conta.Numero"
        } else if ("conta" %in% names(dados_ik_validos)) {
          "conta" # fallback
        } else {
          NULL
        }

        if (!is.null(coluna_conta_n)) {
          dados_ik_validos %>%
            dplyr::mutate(
              mes = lubridate::floor_date(.data[[coluna_data_ik]], "month"),
              # Criar conta.ik a partir de Conta.N
              conta.ik = as.character(.data[[coluna_conta_n]]),
              # Recalcular conta.interno a partir de conta.ik (últimos 4 dígitos numéricos)
              conta.interno = ifelse(
                is.na(.data[["conta.ik"]]) | .data[["conta.ik"]] == "",
                .data$conta.interno, # manter o existente se conta.ik estiver vazio
                {
                  # Extrair apenas números de conta.ik
                  conta_numeros <- stringr::str_extract_all(as.character(.data[["conta.ik"]]), "\\d") %>%
                    sapply(function(x) paste(x, collapse = ""))

                  # Se não houver dígitos ou for string vazia, usar conta.interno existente
                  ifelse(nchar(conta_numeros) == 0 | conta_numeros == "",
                    .data$conta.interno,
                    stringr::str_pad(stringr::str_sub(conta_numeros, -4, -1), 4, side = "left", pad = "0")
                  )
                }
              ),
              identificacao.conta = paste(.data$empresa, .data$banco, .data$conta.interno, sep = " - ")
            ) %>%
            dplyr::group_by(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta.interno) %>%
            dplyr::summarise(
              conta.ik = first(.data$conta.ik),
              entradas.ik = sum(pmax(.data[[coluna_valor_ik]], 0), na.rm = TRUE),
              saidas.ik = sum(pmin(.data[[coluna_valor_ik]], 0), na.rm = TRUE),
              saldo.liquido.ik = sum(.data[[coluna_valor_ik]], na.rm = TRUE),
              qtd.transacoes.ik = dplyr::n(),
              .groups = "drop"
            )
        } else {
          # Se não tiver Conta.N, criar usando conta.interno existente
          dados_ik_validos %>%
            dplyr::mutate(
              mes = lubridate::floor_date(.data[[coluna_data_ik]], "month"),
              conta.ik = .data$conta.interno, # usar conta.interno como conta.ik
              identificacao.conta = paste(.data$empresa, .data$banco, .data$conta.interno, sep = " - ")
            ) %>%
            dplyr::group_by(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta.ik, .data$conta.interno) %>%
            dplyr::summarise(
              entradas.ik = sum(pmax(.data[[coluna_valor_ik]], 0), na.rm = TRUE),
              saidas.ik = sum(pmin(.data[[coluna_valor_ik]], 0), na.rm = TRUE),
              saldo.liquido.ik = sum(.data[[coluna_valor_ik]], na.rm = TRUE),
              qtd.transacoes.ik = dplyr::n(),
              .groups = "drop"
            )
        }
      } else {
        tibble::tibble(
          mes = as.Date(character()),
          identificacao.conta = character(),
          empresa = character(),
          banco = character(),
          conta.ik = character(),
          conta.interno = character(),
          entradas.ik = numeric(),
          saidas.ik = numeric(),
          saldo.liquido.ik = numeric(),
          qtd.transacoes.ik = integer()
        )
      }

      # Criar conjunto completo de combinações mes/identificacao.conta
      todas_combinacoes <- bind_rows(
        fluxos_consolidados %>% select(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta.interno),
        fluxos_ik %>% select(.data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta.interno)
      ) %>%
        distinct()

      # Fazer left join para combinar ambos os datasets
      resultado <- todas_combinacoes %>%
        dplyr::left_join(
          fluxos_consolidados %>% select(.data$mes, .data$identificacao.conta, .data$conta, .data$entradas, .data$saidas, .data$saldo.liquido, .data$qtd.transacoes),
          by = c("mes", "identificacao.conta")
        ) %>%
        dplyr::left_join(
          fluxos_ik %>% select(.data$mes, .data$identificacao.conta, .data$conta.ik, .data$entradas.ik, .data$saidas.ik, .data$saldo.liquido.ik, .data$qtd.transacoes.ik),
          by = c("mes", "identificacao.conta")
        ) %>%
        dplyr::mutate(
          # Preencher valores NA com 0
          entradas = dplyr::coalesce(.data$entradas, 0),
          entradas.ik = dplyr::coalesce(.data$entradas.ik, 0),
          saidas = dplyr::coalesce(.data$saidas, 0),
          saidas.ik = dplyr::coalesce(.data$saidas.ik, 0),
          saldo.liquido = dplyr::coalesce(.data$saldo.liquido, 0),
          saldo.liquido.ik = dplyr::coalesce(.data$saldo.liquido.ik, 0),
          qtd.transacoes = dplyr::coalesce(.data$qtd.transacoes, 0L),
          qtd.transacoes.ik = dplyr::coalesce(.data$qtd.transacoes.ik, 0L)
          # Não alterar a coluna 'conta' - ela deve preservar os valores originais dos extratos consolidados
        ) %>%
        dplyr::select(
          .data$mes, .data$identificacao.conta, .data$empresa, .data$banco, .data$conta, .data$conta.ik, .data$conta.interno,
          .data$entradas, .data$entradas.ik, .data$saidas, .data$saidas.ik,
          .data$saldo.liquido, .data$saldo.liquido.ik, .data$qtd.transacoes, .data$qtd.transacoes.ik
        ) %>%
        dplyr::arrange(.data$mes, .data$empresa, .data$banco, .data$conta.interno)

      message(sprintf("Análise de fluxos comparativa criada: %d registros", nrow(resultado)))
      resultado
    },
    error = function(e) {
      message("Erro ao criar análise de fluxos por conta e mês: ", e$message)
      return(tibble::tibble(
        mes = as.Date(character()),
        identificacao.conta = character(),
        empresa = character(),
        banco = character(),
        conta = character(),
        conta.ik = character(),
        conta.interno = character(),
        entradas = numeric(),
        entradas.ik = numeric(),
        saidas = numeric(),
        saidas.ik = numeric(),
        saldo.liquido = numeric(),
        saldo.liquido.ik = numeric(),
        qtd.transacoes = integer(),
        qtd.transacoes.ik = integer()
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
      "Resumo - Lançamentos" = mapaExtratos,
      "Extratos - Ik" = dadosIk,
      "Contas mensal" = fluxosContaMes
    )

    # Definir larguras específicas das colunas por aba
    largurasColunas <- list(
      # Aba Extratos
      "Extratos" = c(
        "data.lancamento" = 15,
        "data.movimentacao" = 15,
        "documento" = 18,
        "descricao" = 60,
        "valor" = 15,
        "saldo" = 15,
        "conta.interno" = 12,
        "conta" = 18,
        "agencia" = 12,
        "produto" = 25,
        "cnpj" = 20,
        "empresa" = 12,
        "periodo.inicio" = 15,
        "periodo.fim" = 15,
        "data.consulta" = 20,
        "arquivo" = 40,
        "banco" = 12
      ),
      # Aba Resumo - Lançamentos
      "Resumo - Lançamentos" = c(
        "empresa" = 12,
        "banco" = 12,
        "conta" = 18,
        "descricao" = 60,
        "quantidade.arquivos" = 18,
        "quantidade.registros" = 20,
        "soma.valor" = 15,
        "soma.valor.abs" = 15,
        "arquivo(s)" = 80
      ),
      # Aba Extratos - Ik (adaptar baseado nas colunas reais dos dados IK)
      "Extratos - Ik" = if (nrow(dadosIk) > 0) {
        # Criar larguras dinâmicas baseadas nas colunas reais dos dados IK
        colunas_ik <- names(dadosIk)
        larguras_ik <- rep(15, length(colunas_ik))
        names(larguras_ik) <- colunas_ik

        # Ajustar larguras específicas para colunas conhecidas
        larguras_especiais <- c(
          "Data" = 15, "data.movimento" = 15, "Data.Movimento" = 15,
          "Valor" = 15, "valor" = 15,
          "empresa" = 12, "banco" = 12,
          "conta.interno" = 12, "conta" = 18,
          "mes" = 15,
          "Descricao" = 50, "descricao" = 50,
          "Historico" = 50, "historico" = 50
        )

        for (col_nome in names(larguras_especiais)) {
          if (col_nome %in% colunas_ik) {
            larguras_ik[col_nome] <- larguras_especiais[col_nome]
          }
        }

        larguras_ik
      } else {
        c(
          "Data" = 15, "Valor" = 15, "empresa" = 12, "banco" = 12,
          "conta.interno" = 12, "conta" = 18, "mes" = 15
        )
      },
      # Aba Contas mensal
      "Contas mensal" = c(
        "mes" = 15,
        "identificacao.conta" = 40,
        "empresa" = 12,
        "banco" = 12,
        "conta" = 18,
        "conta.ik" = 18,
        "conta.interno" = 12,
        "entradas" = 15,
        "entradas.ik" = 15,
        "saidas" = 15,
        "saidas.ik" = 15,
        "saldo.liquido" = 15,
        "saldo.liquido.ik" = 15,
        "qtd.transacoes" = 15,
        "qtd.transacoes.ik" = 15
      )
    )

    # Definir colunas monetárias por aba
    colunasMonetarias <- list(
      "Extratos" = c("valor", "saldo"),
      "Resumo - Lançamentos" = c("soma.valor", "soma.valor.abs"),
      "Extratos - Ik" = if (nrow(dadosIk) > 0) {
        # Identificar colunas monetárias dinamicamente nos dados IK
        colunas_valor_ik <- c(
          "Valor", "valor", "Total.Pago", "total.pago",
          "Valor.Titulo", "valor.titulo"
        )
        colunas_valor_ik[colunas_valor_ik %in% names(dadosIk)]
      } else {
        c("Valor")
      },
      "Contas mensal" = c(
        "entradas", "entradas.ik", "saidas", "saidas.ik",
        "saldo.liquido", "saldo.liquido.ik"
      )
    )

    # Definir colunas com quebra de texto por aba
    colunasTexto <- list(
      "Extratos" = c("descricao"),
      "Resumo - Lançamentos" = c("arquivo(s)", "descricao"),
      "Extratos - Ik" = character(0),
      "Contas mensal" = c("identificacao.conta")
    )

    # Definir colunas de data por aba
    colunasData <- list(
      "Extratos" = c("data.lancamento", "data.movimentacao", "periodo.inicio", "periodo.fim"),
      "Resumo - Lançamentos" = character(0),
      "Extratos - Ik" = if (nrow(dadosIk) > 0) {
        # Identificar colunas de data dinamicamente nos dados IK
        colunas_data_ik <- c(
          "Data", "data.movimento", "Data.Movimento",
          "mes", "Data.Lancamento", "data.lancamento"
        )
        colunas_data_ik[colunas_data_ik %in% names(dadosIk)]
      } else {
        c("Data", "mes")
      },
      "Contas mensal" = c("mes")
    )

    # Usar gerar_xlsx para carregar o template e popular as abas existentes
    caminhoArquivo <- gerar_xlsx(
      data = dadosAbas,
      wb_load = caminhoTemplate, # Carregar o template diretamente
      tab_names = names(dadosAbas),
      col_width_def = 18,
      col_width_spec = largurasColunas[["Extratos"]], # Usar primeira aba como base
      col_monetary = colunasMonetarias[["Extratos"]], # Usar primeira aba como base
      col_clip = colunasTexto[["Extratos"]], # Usar primeira aba como base
      save = list(nomeArquivo, caminhoDestino)
    )

    # Carregar o arquivo criado para formatação adicional específica por aba
    wb <- openxlsx::loadWorkbook(caminhoArquivo)

    # Aplicar formatação específica para cada aba
    for (nome_aba in names(dadosAbas)) {
      if (nome_aba %in% names(wb) && nrow(dadosAbas[[nome_aba]]) > 0) {
        dados_aba <- dadosAbas[[nome_aba]]

        # Aplicar larguras específicas da aba
        if (nome_aba %in% names(largurasColunas)) {
          larguras_aba <- largurasColunas[[nome_aba]]
          for (nome_coluna in names(larguras_aba)) {
            if (nome_coluna %in% colnames(dados_aba)) {
              col_pos <- which(colnames(dados_aba) == nome_coluna)
              openxlsx::setColWidths(
                wb,
                sheet = nome_aba,
                cols = col_pos,
                widths = larguras_aba[nome_coluna]
              )
            }
          }
        }

        # Aplicar formatação monetária específica da aba
        if (nome_aba %in% names(colunasMonetarias)) {
          colunas_monetarias_aba <- colunasMonetarias[[nome_aba]]
          if (length(colunas_monetarias_aba) > 0) {
            colunas_monetarias_pos <- which(colnames(dados_aba) %in% colunas_monetarias_aba)
            if (length(colunas_monetarias_pos) > 0) {
              openxlsx::addStyle(
                wb,
                sheet = nome_aba,
                style = openxlsx::createStyle(numFmt = "#,##0.00"),
                rows = 2:(nrow(dados_aba) + 1),
                cols = colunas_monetarias_pos,
                gridExpand = TRUE,
                stack = TRUE
              )
            }
          }
        }

        # Aplicar formatação de data específica da aba
        if (nome_aba %in% names(colunasData)) {
          colunas_data_aba <- colunasData[[nome_aba]]
          if (length(colunas_data_aba) > 0) {
            colunas_data_pos <- which(colnames(dados_aba) %in% colunas_data_aba)
            if (length(colunas_data_pos) > 0) {
              openxlsx::addStyle(
                wb,
                sheet = nome_aba,
                style = openxlsx::createStyle(numFmt = "DD/MM/YYYY"),
                rows = 2:(nrow(dados_aba) + 1),
                cols = colunas_data_pos,
                gridExpand = TRUE,
                stack = TRUE
              )
            }
          }
        }

        # Aplicar formatação de texto com quebra específica da aba
        if (nome_aba %in% names(colunasTexto)) {
          colunas_texto_aba <- colunasTexto[[nome_aba]]
          if (length(colunas_texto_aba) > 0) {
            colunas_texto_pos <- which(colnames(dados_aba) %in% colunas_texto_aba)
            if (length(colunas_texto_pos) > 0) {
              openxlsx::addStyle(
                wb,
                sheet = nome_aba,
                style = openxlsx::createStyle(halign = "left", wrapText = TRUE),
                rows = 2:(nrow(dados_aba) + 1),
                cols = colunas_texto_pos,
                gridExpand = TRUE,
                stack = TRUE
              )
            }
          }
        }
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

    message(sprintf("Arquivo Excel criado baseado no template: %s", caminhoArquivo))
    message("Abas populadas com dados extraídos:")
    message("  - Aba 'Extratos': Dados consolidados de extratos CEF e ITAÚ")
    message("  - Aba 'Resumo - Lançamentos': Mapa de combinações únicas")
    message("  - Aba 'Extratos - Ik': Dados CMF extraídos do Informakon")
    message("  - Aba 'Contas mensal': Análise de entradas e saídas por conta e mês")
    message("  - Aba 'Mapeamento': Descrições únicas adicionadas")
    message("Abas do template preservadas: Empresa mensal, DFC, Configurações")

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
    DFC = DFC,
    extratosIk = dadosIk
  ))
}

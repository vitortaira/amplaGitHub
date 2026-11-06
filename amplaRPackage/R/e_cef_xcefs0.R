#' Consolida extratos CEF com diagnostico por arquivo
#'
#' @description
#' Versao experimental de e_cef_xcefs(), sem substituir a funcao original.
#' Usa o mesmo extrator individual e as mesmas regras de classificacao.
#' Erros, avisos e mensagens sao registrados sem interromper outros arquivos.
#'
#' @param f_caminho.pasta.extratos_c Pasta dos extratos. Quando fornecida,
#'   restringe os caminhos selecionados por c_extratos() a essa pasta.
#' @param arquivo.subtipo Selecao de arquivos: "melhores" ou "todos".
#' @param caminhosArquivos Vetor opcional de caminhos explicitos. Ignora a
#'   selecao automatica e o filtro de pasta; caminhos repetidos sao removidos.
#' @param contratosPj Vetor opcional dos ultimos seis digitos dos contratos
#'   PJ. NULL carrega e_cef_nplpjs() uma unica vez. Falhas interrompem a
#'   execucao para evitar classificacao incorreta de entradas PJ.
#' @param mapaContas Tibble de continuidade com id.antigo, id.atual e
#'   id.continuo. O padrao usa o mapeamento do pacote; NULL desativa o filtro.
#' @param detalhar Se TRUE, imprime o resultado de cada arquivo. Caso
#'   contrario, lotes de pelo menos 20 arquivos mostram progresso por quartos.
#'
#' @return Tibble com as mesmas colunas de e_cef_xcefs(), inclusive quando
#'   vazio. O atributo "diagnostico" apresenta primeiro erros, arquivos com
#'   avisos e resultados vazios; inclui arquivo, estado, registros, erro,
#'   avisos, caminho completo e mensagens. O atributo "mensagensSelecao"
#'   preserva as mensagens do catalogo, sem imprimi-las no console.
#'   Advertencias do catalogo continuam visiveis. "vazio" significa zero linhas
#'   retornadas pelo parser, nao comprova ausencia de movimentacao.
#'   Uma advertencia resumida sinaliza erros ou avisos do parser.
#' @examples
#' \dontrun{
#' extratos <- e_cef_xcefs0()
#' attr(extratos, "diagnostico")
#' }
#' @importFrom magrittr %>%
#' @export
e_cef_xcefs0 <- function(
  f_caminho.pasta.extratos_c = caminhos_pastas("extratos"),
  arquivo.subtipo = "melhores",
  caminhosArquivos = NULL,
  contratosPj = NULL,
  mapaContas = contasBancarias,
  detalhar = FALSE
) {
  inicio <- proc.time()[["elapsed"]]
  mensagensSelecao <- character()
  arquivo.subtipo <- match.arg(arquivo.subtipo, c("todos", "melhores"))
  if (!is.logical(detalhar) || length(detalhar) != 1L || is.na(detalhar)) {
    stop("'detalhar' deve ser TRUE ou FALSE.")
  }
  colunasMapa <- c("id.antigo", "id.atual", "id.continuo")
  if (!is.null(mapaContas) &&
    (!is.data.frame(mapaContas) ||
      !all(colunasMapa %in% names(mapaContas)))) {
    stop("Mapeamento de contas sem id.antigo, id.atual ou id.continuo.")
  }
  if (is.null(caminhosArquivos)) {
    message("CEF | Selecionando extratos (", arquivo.subtipo, ")...")
    caminhosArquivos <- withCallingHandlers(
      c_extratos(
        arquivo.tipo = "xcef", arquivo.subtipo = arquivo.subtipo
      )$caminho,
      message = function(mensagem) {
        mensagensSelecao <<- c(mensagensSelecao,
          trimws(conditionMessage(mensagem)))
        invokeRestart("muffleMessage")
      }
    )
    if (!missing(f_caminho.pasta.extratos_c)) {
      if (!fs::dir_exists(f_caminho.pasta.extratos_c)) {
        stop("Pasta de extratos nao encontrada: ",
          f_caminho.pasta.extratos_c)
      }
      pastaExtratos <- fs::path_abs(f_caminho.pasta.extratos_c)
      caminhosArquivos <- caminhosArquivos[
        fs::path_has_parent(fs::path_abs(caminhosArquivos), pastaExtratos)
      ]
    }
  }
  if (!is.character(caminhosArquivos) || anyNA(caminhosArquivos) ||
    any(!nzchar(caminhosArquivos))) {
    stop("Os caminhos devem ser textos nao vazios e sem NA.")
  }
  caminhosArquivos <- unique(as.character(fs::path_abs(caminhosArquivos)))
  modeloExtratos <- tibble::tibble(
    data.lancamento = as.Date(character()),
    data.movimentacao = as.Date(character()),
    documento = character(), descricao = character(),
    valor = double(), saldo = double(), natureza = character(),
    conta.interno = character(), conta = character(), agencia = character(),
    produto = character(), cnpj = character(), empresa = character(),
    periodo.inicio = as.Date(character()),
    periodo.fim = as.Date(character()),
    data.consulta = as.POSIXct(character(), tz = "UTC"),
    contrato.6 = character(), contrato.5 = character(),
    arquivo = character(), arquivo.subtipo = character(),
    arquivo.tabela.tipo = character(), arquivo.tipo = character(),
    arquivo.fonte = character(), cpf.cnpj = character(),
    nome.razao = character()
  )
  modeloDiagnostico <- tibble::tibble(
    arquivo = character(), estado = character(), registros = integer(),
    erro = character(), avisos = character(), caminho = character(),
    mensagens = character()
  )
  if (length(caminhosArquivos) == 0L) {
    attr(modeloExtratos, "diagnostico") <- modeloDiagnostico
    attr(modeloExtratos, "mensagensSelecao") <- mensagensSelecao
    message("CEF | Nenhum arquivo selecionado; nenhuma extracao realizada.")
    return(modeloExtratos)
  }
  totalArquivos <- length(caminhosArquivos)
  message(sprintf("CEF | %d arquivos selecionados. Preparando contratos PJ...",
    totalArquivos))
  if (is.null(contratosPj)) {
    dadosContratos <- tryCatch(
      e_cef_nplpjs(),
      error = function(erro) {
        stop("Falha ao carregar contratos PJ: ", conditionMessage(erro),
          call. = FALSE)
      }
    )
    colunasContratos <- c("contrato.6.ultimo", "contrato.6.penultimo")
    if (!all(colunasContratos %in% names(dadosContratos))) {
      stop("Dados de contratos PJ sem as colunas esperadas.")
    }
    contratosPj <- unlist(dadosContratos[colunasContratos], use.names = FALSE)
  }
  contratosPj <- unique(as.character(contratosPj[!is.na(contratosPj)]))
  colunasObrigatorias <- setdiff(names(modeloExtratos), c(
    "natureza", "contrato.6", "contrato.5", "arquivo.tabela.tipo",
    "arquivo.tipo", "arquivo.fonte", "cpf.cnpj", "nome.razao"
  ))
  message("CEF | Extraindo arquivos...")
  marcosProgresso <- unique(ceiling(totalArquivos * c(0.25, 0.5, 0.75, 1)))
  resultados <- lapply(seq_along(caminhosArquivos), function(indiceArquivo) {
    caminhoArquivo <- caminhosArquivos[[indiceArquivo]]
    avisos <- character()
    mensagens <- character()
    erroExtracao <- NA_character_
    dados <- tryCatch(
      withCallingHandlers({
        if (!fs::file_exists(caminhoArquivo)) {
          stop("Arquivo nao encontrado.")
        }
        extrato <- e_cef_xcef(caminhoArquivo)
        if (!is.data.frame(extrato)) {
          stop("O extrator nao retornou um data.frame.")
        }
        if (nrow(extrato) > 0L) {
          faltantes <- setdiff(colunasObrigatorias, names(extrato))
          if (length(faltantes) > 0L) {
            stop("Colunas obrigatorias ausentes: ",
              paste(faltantes, collapse = ", "))
          }
          opcionais <- c("cpf.cnpj", "nome.razao")
          ausentes <- setdiff(opcionais, names(extrato))
          extrato[ausentes] <- rep(list(NA_character_), length(ausentes))
          colunasEntrada <- c(colunasObrigatorias, opcionais)
          extrato <- vctrs::vec_cast(
            tibble::as_tibble(extrato[colunasEntrada]),
            modeloExtratos[colunasEntrada]
          ) %>%
            dplyr::mutate(arquivo = caminhoArquivo)
        }
        extrato
      }, warning = function(aviso) {
        avisos <<- c(avisos, conditionMessage(aviso))
        invokeRestart("muffleWarning")
      }, message = function(mensagem) {
        mensagens <<- c(mensagens, trimws(conditionMessage(mensagem)))
        invokeRestart("muffleMessage")
      }),
      error = function(erro) {
        erroExtracao <<- conditionMessage(erro)
        NULL
      }
    )
    registros <- if (is.null(dados)) 0L else nrow(dados)
    estado <- if (!is.na(erroExtracao)) "erro" else if (registros == 0L) {
      "vazio"
    } else {
      "sucesso"
    }
    if (detalhar) {
      message(sprintf("CEF | [%d/%d] ", indiceArquivo, totalArquivos),
        fs::path_file(caminhoArquivo), ": ", estado,
        " (", registros, " registros)",
        if (!is.na(erroExtracao)) paste0(" - ", erroExtracao) else "")
    } else if (totalArquivos >= 20L &&
      indiceArquivo %in% marcosProgresso) {
      message(sprintf("CEF | Extraidos %d/%d arquivos (%.0f%%).",
        indiceArquivo, totalArquivos, 100 * indiceArquivo / totalArquivos))
    }
    list(
      dados = if (registros > 0L) dados else NULL,
      diagnostico = tibble::tibble(
        arquivo = fs::path_file(caminhoArquivo), estado = estado,
        registros = registros,
        erro = erroExtracao, avisos = paste(unique(avisos), collapse = "\n"),
        caminho = caminhoArquivo,
        mensagens = paste(unique(mensagens), collapse = "\n")
      )
    )
  })
  diagnostico <- dplyr::bind_rows(lapply(resultados, `[[`, "diagnostico"))
  extratos <- dplyr::bind_rows(
    c(list(modeloExtratos), lapply(resultados, `[[`, "dados"))
  )
  extratos <- extratos %>%
    dplyr::mutate(
      empresa = dplyr::case_when(
        stringr::str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
        stringr::str_detect(empresa, "(?i)metro\\s?vila\\s?sonia") ~ "AVS",
        stringr::str_detect(empresa, "(?i)grauca") ~ "GRA",
        stringr::str_detect(empresa, "(?i)incorflora") ~ "INC",
        stringr::str_detect(empresa, "(?i)sao\\s?l") ~ "LUC",
        stringr::str_detect(empresa, "(?i)pompeia") ~ "POM",
        stringr::str_detect(empresa, "(?i)up\\s?s\\.") ~ "SAU",
        stringr::str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
        stringr::str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
        TRUE ~ empresa
      ),
      natureza = dplyr::case_when(
        (descricao %in% c("CR DESBLOQ", "CRE D IMOB") |
          stringr::str_detect(descricao, "(?i)cred.*hab\\s?desbloq")) &
          documento %in% contratosPj ~ "entrada.pj",
        (descricao %in% c("CR DESBLOQ", "CRE D IMOB") |
          stringr::str_detect(descricao, "(?i)cred.*hab\\s?desbloq")) &
          !(documento %in% contratosPj) |
          descricao %in% c("DESB CR CX", "DESBL.SALD") ~ "repasse.cef",
        descricao %in% c(
          "AMORTIZACAO SALDO DEVEDOR",
          "AMORTIZA\u00c7\u00c3O SALDO DEVEDOR",
          "DEB AMORT SD DEVEDOR FIN", "DEB AMT PJ", "DEBITO AUTORIZADO"
        ) ~ "amortizacao.pj",
        TRUE ~ NA_character_
      ),
      contrato.6 = stringr::str_pad(documento, 6, side = "left", pad = "0"),
      contrato.5 = dplyr::if_else(
        arquivo.subtipo == "xcef6", stringr::str_sub(contrato.6, 1, 5),
        stringr::str_sub(contrato.6, 2, 6)
      ),
      arquivo.tabela.tipo = "xcef", arquivo.tipo = "xcef",
      arquivo.fonte = "cef"
    )
  if (!is.null(mapaContas) && nrow(mapaContas) > 0L) {
    extratos <- extratos %>%
      dplyr::mutate(
        idCorrente = stringr::str_c(empresa, "-CEF_", conta.interno),
        origemConta = dplyr::case_when(
          idCorrente %in% mapaContas$id.antigo ~ "antiga",
          idCorrente %in% mapaContas$id.atual ~ "atual",
          TRUE ~ "unica"
        ),
        idContinuo = dplyr::case_when(
          origemConta == "antiga" ~ mapaContas$id.continuo[
            match(idCorrente, mapaContas$id.antigo)],
          origemConta == "atual" ~ mapaContas$id.continuo[
            match(idCorrente, mapaContas$id.atual)],
          TRUE ~ idCorrente
        ),
        mesMovimento = lubridate::floor_date(data.movimentacao, "month")
      ) %>%
      dplyr::group_by(idContinuo, mesMovimento) %>%
      dplyr::filter(
        is.na(mesMovimento) | !(origemConta == "antiga" &
          any(origemConta == "atual"))
      ) %>%
      dplyr::ungroup()
  }
  extratos <- dplyr::select(extratos, dplyr::all_of(names(modeloExtratos)))
  prioridade <- ifelse(diagnostico$estado == "erro", 1L,
    ifelse(nzchar(diagnostico$avisos), 2L,
      ifelse(diagnostico$estado == "vazio", 3L, 4L)))
  diagnostico <- diagnostico[order(prioridade, diagnostico$arquivo), ]
  attr(extratos, "diagnostico") <- diagnostico
  attr(extratos, "mensagensSelecao") <- mensagensSelecao
  totalErros <- sum(diagnostico$estado == "erro")
  totalVazios <- sum(diagnostico$estado == "vazio")
  totalAvisos <- sum(nzchar(diagnostico$avisos))
  registrosExtraidos <- sum(diagnostico$registros)
  message(sprintf(
    paste0("CEF | Concluido em %.1fs: %d arquivos; ",
      "%d com dados; %d sem linhas; %d com erro."),
    proc.time()[["elapsed"]] - inicio,
    nrow(diagnostico), sum(diagnostico$estado == "sucesso"),
    totalVazios, totalErros
  ))
  message(sprintf(
    "CEF | Registros: %d extraidos; %d removidos por continuidade; %d finais.",
    registrosExtraidos, registrosExtraidos - nrow(extratos), nrow(extratos)
  ))
  invisible(lapply(c("erro", "avisos"), function(tipoProblema) {
    problemas <- diagnostico[[tipoProblema]]
    presentes <- !is.na(problemas) & nzchar(problemas)
    if (!any(presentes)) return(NULL)
    causas <- stringr::str_squish(problemas[presentes])
    frequencias <- sort(table(causas), decreasing = TRUE)
    message("CEF | ", if (tipoProblema == "erro") "Falhas" else "Avisos",
      " por causa (", sum(presentes), " arquivos):")
    invisible(lapply(utils::head(names(frequencias), 3L), function(causa) {
      exemplos <- diagnostico$arquivo[presentes][causas == causa]
      message("  ", length(exemplos), " arquivo(s): ", causa,
        "\n    ", paste(utils::head(exemplos, 3L), collapse = ", "),
        if (length(exemplos) > 3L) {
          paste0("; +", length(exemplos) - 3L, " no diagnostico")
        } else "")
    }))
    if (length(frequencias) > 3L) {
      message("  Mais ", length(frequencias) - 3L,
        " causa(s) no diagnostico.")
    }
    NULL
  }))
  if (totalVazios > 0L) {
    exemplosVazios <- diagnostico$arquivo[diagnostico$estado == "vazio"]
    message("CEF | Sem linhas retornadas: ",
      paste(utils::head(exemplosVazios, 3L), collapse = ", "),
      if (totalVazios > 3L) paste0("; +", totalVazios - 3L) else "", ".",
      "\n  Resultado vazio nao comprova ausencia de movimentacao.")
  }
  if (totalErros > 0L || totalAvisos > 0L) {
    warning(
      sprintf(paste0("Extracao CEF requer revisao: %d arquivo(s) com erro; ",
        "%d com avisos; %d sem linhas. "),
        totalErros, totalAvisos, totalVazios),
      "Consulte o atributo 'diagnostico' do objeto retornado.", call. = FALSE
    )
  }
  extratos
}
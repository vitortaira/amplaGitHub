#' @title Extração de um relatório CMF (Conta Movimento Financeiro) Informakon
#'
#' @description
#' A função e_ik_cmf() extrai os dados de um único arquivo de Conta Movimento
#' Financeiro (CMF) do Informakon, informado pelo caminho, e os retorna em um
#' \code{tibble} padronizado.
#'
#' @details
#' O relatório CMF organiza os lançamentos em blocos precedidos por linhas de
#' agrupamento \code{"Agente Conta: <agente> - <código> - <conta>"}, cujos
#' valores são propagados para baixo (fill-down) e separados nas colunas
#' \code{agente}, \code{agente.codigo} e \code{n.conta}. As datas vêm no
#' formato serial do Excel e são convertidas para \code{Date}; o campo
#' \code{valor} recebe sinal negativo quando \code{d.c == "D"}.
#'
#' @param caminho.cmf_c String com o caminho do arquivo CMF (.xlsx).
#'
#' @return \code{tibble} com as colunas: \code{agente}, \code{agente.codigo},
#'   \code{n.conta}, \code{n.mov}, \code{registro}, \code{data.razao},
#'   \code{conciliacao}, \code{c}, \code{natureza.mov}, \code{origem},
#'   \code{valor}, \code{d.c}, \code{saldo.razao}, \code{historico},
#'   \code{arquivo}, \code{arquivo.tipo} (\code{"cmf"}) e \code{arquivo.fonte}
#'   (\code{"ik"}).
#'
#' @examples
#' \dontrun{
#' cmf_t <- e_ik_cmf("caminho/para/cmf-2026_08.xlsx")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter select if_else
#' @importFrom tidyr fill
#' @importFrom stringr str_detect str_remove str_match str_trim
#' @export
e_ik_cmf <- function(caminho.cmf_c) {
  if (!file.exists(caminho.cmf_c)) {
    stop("Arquivo CMF não encontrado: ", caminho.cmf_c)
  }

  # Agente Conta: <agente> - <código> - <conta>
  padraoAgente_c <- "^(\\S+)\\s*-\\s*(\\S+)\\s*-\\s*(.+)$"

  cmf_bruto <- suppressMessages(readxl::read_excel(
    caminho.cmf_c,
    sheet = 1,
    col_names = FALSE,
    col_types = "text"
  ))

  # Layout alternativo: tabela plana com linha de cabecalho
  # (N Mov, Data, C, Origem, Historico, AgenteFinanceiro, Conta N, Valor, D/C...)
  linhaCabecalho_i <- which(
    str_detect(coalesce(cmf_bruto[[1]], ""), "^(?i)n.{0,2}\\s*mov")
  )
  if (length(linhaCabecalho_i) > 0) {
    return(extrairCmfPlano(cmf_bruto, linhaCabecalho_i[1], caminho.cmf_c))
  }

  cmf_t <- tibble::tibble(
    c1 = cmf_bruto[[1]],
    n.mov = cmf_bruto[[2]],
    registro = cmf_bruto[[3]],
    data.razao = cmf_bruto[[4]],
    conciliacao = cmf_bruto[[5]],
    c = cmf_bruto[[6]],
    natureza.mov = cmf_bruto[[7]],
    origem = cmf_bruto[[8]],
    valor = cmf_bruto[[9]],
    d.c = cmf_bruto[[10]],
    saldo.razao = cmf_bruto[[11]],
    historico = cmf_bruto[[12]]
  ) %>%
    mutate(
      agente.conta = if_else(
        str_detect(c1, "^(?i)agente conta:"),
        str_remove(c1, "^[^:]*:\\s*"),
        NA_character_
      )
    ) %>%
    tidyr::fill(agente.conta, .direction = "down") %>%
    # Mantém apenas linhas de dados (descarta cabeçalho e linhas de agrupamento)
    dplyr::filter(!is.na(n.mov) & !str_detect(n.mov, "(?i)movimento")) %>%
    mutate(
      agente = str_match(agente.conta, padraoAgente_c)[, 2],
      agente.codigo = str_match(agente.conta, padraoAgente_c)[, 3],
      n.conta = str_trim(str_match(agente.conta, padraoAgente_c)[, 4]),
      registro = as.Date(as.integer(registro), origin = "1899-12-30"),
      data.razao = as.Date(as.integer(data.razao), origin = "1899-12-30"),
      conciliacao = as.Date(as.integer(conciliacao), origin = "1899-12-30"),
      valor = if_else(d.c == "D", as.numeric(valor) * -1, as.numeric(valor)),
      saldo.razao = as.numeric(saldo.razao),
      arquivo = caminho.cmf_c,
      arquivo.tipo = "cmf",
      arquivo.fonte = "ik"
    ) %>%
    dplyr::select(
      agente, agente.codigo, n.conta, n.mov, registro, data.razao,
      conciliacao, c, natureza.mov, origem, valor, d.c, saldo.razao,
      historico, arquivo, arquivo.tipo, arquivo.fonte
    )

  return(cmf_t)
}

# Extrai o layout plano do CMF (cabecalho na linha indicada) mapeando as
# colunas pelo nome, ja que a ordem difere do layout em blocos.
#' @importFrom dplyr coalesce
#' @importFrom stringr str_to_lower str_replace_all
#' @noRd
extrairCmfPlano <- function(cmf_bruto, linhaCabecalho_i, caminho.cmf_c) {
  normalizar <- function(x_c) {
    x_c %>%
      iconv(to = "ASCII//TRANSLIT", sub = "") %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]", "")
  }
  cabecalho_c <- normalizar(coalesce(
    as.character(cmf_bruto[linhaCabecalho_i, ]), ""
  ))
  dados_t <- cmf_bruto[-seq_len(linhaCabecalho_i), ]

  coluna <- function(chave_c) {
    idx_i <- which(str_detect(cabecalho_c, chave_c))[1]
    if (is.na(idx_i)) {
      return(rep(NA_character_, nrow(dados_t)))
    }
    dados_t[[idx_i]]
  }

  dataSerial <- function(x_c) {
    as.Date(suppressWarnings(as.integer(x_c)), origin = "1899-12-30")
  }

  tibble::tibble(
    agente = coluna("^agentefinanceiro"),
    agente.codigo = NA_character_,
    n.conta = coluna("^contan"),
    n.mov = coluna("^n.{0,2}mov"),
    registro = dataSerial(coluna("^data$")),
    data.razao = dataSerial(coluna("^data$")),
    conciliacao = dataSerial(coluna("^conciliacao")),
    c = coluna("^c$"),
    natureza.mov = coluna("^naturezadomovimento"),
    origem = coluna("^origem"),
    valor = as.numeric(coluna("^valor$")),
    d.c = coluna("^dc$"),
    saldo.razao = NA_real_,
    historico = coluna("^historico"),
    arquivo = caminho.cmf_c,
    arquivo.tipo = "cmf",
    arquivo.fonte = "ik"
  ) %>%
    dplyr::filter(!is.na(n.mov)) %>%
    mutate(valor = if_else(d.c == "D", valor * -1, valor))
}

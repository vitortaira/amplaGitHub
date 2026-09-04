#' @title Extração de um relatório FC (Fechamento de Caixa) Informakon
#'
#' @description
#' A função e_ik_fc() extrai os dados de um único arquivo de Fechamento de
#' Caixa (FC) do Informakon, informado pelo caminho, e os retorna em um
#' \code{tibble} padronizado.
#'
#' @details
#' O relatório FC não possui uma linha de cabeçalho utilizável: os dados são
#' organizados em blocos precedidos por linhas de agrupamento
#' \code{"Empresa: ..."} e \code{"Centro de Negócio: ..."}, cujos valores são
#' propagados para baixo (fill-down). As linhas de dados são identificadas pela
#' data em \code{dd/mm/aaaa} na primeira coluna; linhas de subtotal
#' (\code{"Total: ..."}) e metadados são descartadas.
#'
#' A maioria das linhas segue o layout padrão, mas um pequeno conjunto de
#' lançamentos vem deslocado em uma coluna à direita; ambos os layouts são
#' tratados via detecção do deslocamento. Os valores monetários estão no
#' formato brasileiro (\code{1.234,56}) e são convertidos para numérico. O
#' campo \code{valor} é calculado como \code{entrada - saida}.
#'
#' @param caminho.fc_c String com o caminho do arquivo FC (.xlsx).
#'
#' @return \code{tibble} com as colunas: \code{empresa},
#'   \code{centro.negocio}, \code{data}, \code{n.mov}, \code{historico},
#'   \code{conta}, \code{n.conta}, \code{entrada}, \code{saida}, \code{valor},
#'   \code{arquivo}, \code{arquivo.tipo} (\code{"fc"}) e \code{arquivo.fonte}
#'   (\code{"ik"}).
#'
#' @examples
#' \dontrun{
#' fc_t <- e_ik_fc("caminho/para/fc-2026_08.xlsx")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter select if_else
#' @importFrom tidyr fill
#' @importFrom stringr str_detect str_extract str_remove str_remove_all
#'   str_replace
#' @export
e_ik_fc <- function(caminho.fc_c) {
  if (!file.exists(caminho.fc_c)) {
    stop("Arquivo FC não encontrado: ", caminho.fc_c)
  }

  # Converte texto no formato monetário brasileiro (1.234,56) para numérico
  numeroBr <- function(x_c) {
    x_c %>%
      str_remove_all("\\.") %>%
      str_replace(",", ".") %>%
      as.numeric()
  }

  fc_bruto <- suppressMessages(readxl::read_excel(
    caminho.fc_c,
    sheet = 1,
    col_names = FALSE,
    col_types = "text"
  ))

  fc_t <- tibble::tibble(
    c1 = fc_bruto[[1]],
    c2 = fc_bruto[[2]],
    c3 = fc_bruto[[3]],
    c11 = fc_bruto[[11]],
    c12 = fc_bruto[[12]],
    c13 = fc_bruto[[13]],
    c14 = fc_bruto[[14]],
    c17 = fc_bruto[[17]],
    c18 = fc_bruto[[18]],
    c19 = fc_bruto[[19]]
  ) %>%
    mutate(
      empresa = if_else(
        str_detect(c1, "^(?i)empresa:"),
        str_extract(str_remove(c1, "^[^:]*:\\s*"), "^\\S+"),
        NA_character_
      ),
      centro.negocio = if_else(
        str_detect(c1, "^(?i)centro de neg"),
        str_extract(str_remove(c1, "^[^:]*:\\s*"), "^\\S+"),
        NA_character_
      )
    ) %>%
    tidyr::fill(empresa, .direction = "down") %>%
    tidyr::fill(centro.negocio, .direction = "down") %>%
    # Mantém apenas linhas de dados (data em dd/mm/aaaa na primeira coluna)
    dplyr::filter(str_detect(c1, "^\\d{2}/\\d{2}/\\d{4}$")) %>%
    mutate(
      # Alguns lançamentos vêm deslocados uma coluna à direita
      deslocado = is.na(c11) & !is.na(c12),
      data = as.Date(c1, format = "%d/%m/%Y"),
      n.mov = c2,
      historico = c3,
      conta = if_else(deslocado, c12, c11),
      n.conta = if_else(deslocado, c14, c13),
      entrada = numeroBr(if_else(deslocado, c18, c17)),
      saida = numeroBr(if_else(deslocado, c19, c18)),
      valor = round(entrada - saida, 2),
      arquivo = caminho.fc_c,
      arquivo.tipo = "fc",
      arquivo.fonte = "ik"
    ) %>%
    dplyr::select(
      empresa, centro.negocio, data, n.mov, historico, conta, n.conta,
      entrada, saida, valor, arquivo, arquivo.tipo, arquivo.fonte
    )

  return(fc_t)
}

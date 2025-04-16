# Descrição ---------------------------------------------------------------

#' @title Consolida\u00e7\u00e3o dos dados dos extratos da CEF
#'
#' @description
#' Consolida e processa dados de m\u00faltiplos extratos em PDF da CEF,
#' combinando-os em um \u00fanico data frame.
#'
#' @param f_caminho.pasta.extratos_c Caminho completo para a pasta que
#'   cont\u00e9m os arquivos PDF dos extratos.
#'
#' @details
#' A fun\u00e7\u00e3o percorre a pasta especificada buscando arquivos PDF que
#' contenham os c\u00f3digos 2429, 2419 ou 2245, ignorando aqueles que contenham
#' a palavra \u0022fundo\u0022. Para cada arquivo encontrado, chama a fun\u00e7\u00e3o
#' \code{e_cef_extrato} para realizar a extra\u00e7\u00e3o dos dados e, posteriormente,
#' consolida os resultados em um \u00fanico tibble.
#'
#' @return
#' Retorna um tibble com as seguintes colunas:
#'   - Data de lan\u00e7amento: Date.
#'   - Data de movimento: Date.
#'   - Documento: Character.
#'   - Hist\u00f3rico: Character.
#'   - Valor: Numeric.
#'   - Saldo: Numeric.
#'   - Conta_interno: Character.
#'   - Conta: Character.
#'   - Ag\u00eAncia: Character.
#'   - Produto: Character.
#'   - CNPJ: Character.
#'   - Cliente: Character.
#'   - Per\u00edodo_in\u00edcio: Date.
#'   - Per\u00edodo_fim: Date.
#'   - Data_consulta: POSIXct.
#'
#' @examples
#' \dontrun{
#' extratos <- e_cef_extratos(
#'   f_caminho.pasta.extratos_c = "caminho/para/a/pasta/dos/extratos"
#' )
#' print(extratos)
#' }
#'
#' @export

e_cef_extratos <-
  function(f_caminho.pasta.extratos_c =
             here("Relatórios - Documentos", "Relatorios - Extratos")) {
    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    caminhos.extratos.cef_c <-
      list.files(
        f_caminho.pasta.extratos_c,
        full.names = TRUE, recursive = T
      ) %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "2429|2419|2245") &
          !str_detect(.x, "(?i)fundo")
      )
    extratos_l <- list()
    extratos_t <- data.frame()
    for (
      i_caminho.extrato.cef_c in caminhos.extratos.cef_c
    ) {
      extratos_l[[i_caminho.extrato.cef_c]] <-
        e_cef_extrato(i_caminho.extrato.cef_c)
      extratos_t <-
        bind_rows(extratos_t, extratos_l[[i_caminho.extrato.cef_c]])
    }
    extratos_t %<>%
      mutate(
        Contrato_6 =
          Documento %>% str_pad(width = 6, side = "left", pad = "0")
      ) %>%
      as_tibble()
    return(extratos_t)
  }

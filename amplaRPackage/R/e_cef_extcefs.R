# Descrição ---------------------------------------------------------------

#' @title Consolidação dos dados dos extratos da CEF
#'
#' @description
#' Consolida e processa dados de múltiplos extratos em PDF da CEF,
#' combinando-os em um único data frame.
#'
#' @param f_caminho.pasta.extratos_c Caminho completo para a pasta que
#'   contém os arquivos PDF dos extratos.
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos PDF que
#' contenham os códigos 2429, 2419 ou 2245, ignorando aqueles que contenham
#' a palavra "fundo". Para cada arquivo encontrado, chama a função
#' \code{e_cef_extcef} para realizar a extração dos dados e, posteriormente,
#' consolida os resultados em um único tibble.
#'
#' @return
#' Retorna um tibble com as seguintes colunas:
#'   - Data de lançamento: Date.
#'   - Data de movimento: Date.
#'   - documento: Character.
#'   - Histórico: Character.
#'   - Valor: Numeric.
#'   - Saldo: Numeric.
#'   - conta.interno: Character.
#'   - Conta: Character.
#'   - Agência: Character.
#'   - produto: Character.
#'   - CNPJ: Character.
#'   - Cliente: Character.
#'   - periodo.inicio: Date.
#'   - periodo.fim: Date.
#'   - data.consulta: POSIXct.
#'
#' @examples
#' \dontrun{
#' extratos <- e_cef_extcefs(
#'   f_caminho.pasta.extratos_c = "caminho/para/a/pasta/dos/extratos"
#' )
#' print(extratos)
#' }
#'
#' @export

e_cef_extcefs <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    # Mensagem informando o número de extratos identificados
    # n_extratos <- length(caminhos.extratos.cef_c)
    # message(sprintf(
    #   "%d extratos da CEF foram identificados na rede.",
    #   n_extratos
    # ))

    extratos_l <- list()
    extratos_t <- data.frame()
    for (
      i_caminho.extrato.cef_c in e_metadados("extcef")$caminho
    ) {
      extrato <- tryCatch(
        e_cef_extcef(i_caminho.extrato.cef_c),
        error = function(e) {
          message(sprintf("Falha ao extrair: %s", basename(i_caminho.extrato.cef_c)))
          return(NULL)
        }
      )
      if (!is.null(extrato) && nrow(extrato) > 0) {
        message(sprintf("arquivo extraído com sucesso: %s", basename(i_caminho.extrato.cef_c)))
        extratos_l[[i_caminho.extrato.cef_c]] <- extrato
        extratos_t <- bind_rows(extratos_t, extrato)
      } else {
        message(sprintf("arquivo vazio ou não extraído: %s", basename(i_caminho.extrato.cef_c)))
      }
    }
    extratos_t %<>%
      mutate(
        empresa = case_when(
          str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
          str_detect(empresa, "(?i)metro\\s?vila\\s?sonia") ~ "AVS",
          str_detect(empresa, "(?i)grauca") ~ "GRA",
          str_detect(empresa, "(?i)incorflora") ~ "INC",
          str_detect(empresa, "(?i)sao\\s?l") ~ "LUC",
          str_detect(empresa, "(?i)pompeia") ~ "POM",
          str_detect(empresa, "(?i)up\\s?s\\.") ~ "SAU",
          str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
          str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
          TRUE ~ empresa
          # TRUE ~ NA_character_
        ),
        repasse = if_else(
          (descricao == "CR DESBLOQ") &
            !(documento %in% contratos.pj.6.ultimos_c),
          TRUE,
          FALSE
        ),
        pj = if_else(
          (descricao == "CR DESBLOQ") &
            (documento %in% contratos.pj.6.ultimos_c),
          TRUE,
          FALSE
        ),
        contrato.6 =
          documento %>% str_pad(width = 6, side = "left", pad = "0"),
        arquivo.tabela.tipo = "extcef",
        arquivo.tipo = "extcef",
        arquivo.fonte = "cef"
      ) %>%
      as_tibble() %>%
      select(
        data.lancamento, data.movimentacao, documento, descricao, valor, saldo, repasse, pj, conta.interno, conta, agencia, produto, cnpj, empresa,
        periodo.inicio, periodo.fim, data.consulta, contrato.6, arquivo,
        arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte
      )
    return(extratos_t)
  }

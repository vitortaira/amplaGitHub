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
  function(f_caminho.pasta.extratos_c = c_caminhos_pastas("extratos")) {
    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    caminhos.extratos.cef_c <-
      dir_ls(f_caminho.pasta.extratos_c, recurse = TRUE, type = "file") %>%
      keep(
        ~ str_ends(.x, ".pdf") &
          str_detect(.x, "2429|2419|2245") &
          !str_detect(.x, "(?i)fundo")
      )
    # Mensagem informando o número de extratos identificados
    n_extratos <- length(caminhos.extratos.cef_c)
    message(sprintf(
      "%d extratos da CEF foram identificados na rede.",
      n_extratos
    ))
    # Número do contrato do empréstimp PJ

    contratos.pj.6.ultimos_c <- unique(
      e_cef_ecns()$Emprestimo$`Número`
    ) %>%
      str_sub(-6, -1)
    extratos_l <- list()
    extratos_t <- data.frame()
    for (
      i_caminho.extrato.cef_c in caminhos.extratos.cef_c
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
        arquivo.tipo.tabela = "extcef",
        arquivo.tipo = "extcef",
        arquivo.fonte = "cef"
      ) %>%
      as_tibble() %>%
      select(
        data.lancamento, data.movimento, documento, descricao, valor, saldo, repasse, pj, conta.interno, conta, agencia, produto, cnpj, cliente,
        periodo.inicio, periodo.fim, data.consulta, contrato.6, arquivo,
        arquivo.tipo.tabela, arquivo.tipo, arquivo.fonte
      ) %>%
      rename(empreendimento = cliente)
    return(extratos_t)
  }

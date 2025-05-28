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
#' \code{e_cef_extrato} para realizar a extração dos dados e, posteriormente,
#' consolida os resultados em um único tibble.
#'
#' @return
#' Retorna um tibble com as seguintes colunas:
#'   - Data de lançamento: Date.
#'   - Data de movimento: Date.
#'   - Documento: Character.
#'   - Histórico: Character.
#'   - Valor: Numeric.
#'   - Saldo: Numeric.
#'   - Conta_interno: Character.
#'   - Conta: Character.
#'   - Agência: Character.
#'   - Produto: Character.
#'   - CNPJ: Character.
#'   - Cliente: Character.
#'   - Período_início: Date.
#'   - Período_fim: Date.
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
      extratos_l[[i_caminho.extrato.cef_c]] <-
        e_cef_extrato(i_caminho.extrato.cef_c)
      extratos_t <-
        bind_rows(extratos_t, extratos_l[[i_caminho.extrato.cef_c]])
    }
    extratos_t %<>%
      mutate(
        Repasse = if_else(
          (`Histórico` == "CR DESBLOQ") &
            !(Documento %in% contratos.pj.6.ultimos_c),
          TRUE,
          FALSE
        ),
        PJ = if_else(
          (`Histórico` == "CR DESBLOQ") &
            (Documento %in% contratos.pj.6.ultimos_c),
          TRUE,
          FALSE
        ),
        Contrato_6 =
          Documento %>% str_pad(width = 6, side = "left", pad = "0"),
        Arquivo_tipo_tabela = "extcef",
        Arquivo_tipo = "extcef",
        Arquivo_fonte = "cef"
      ) %>%
      as_tibble() %>%
      select(
        `Data de lançamento`, `Data de movimento`, Documento, `Histórico`,
        Valor, Saldo, Repasse, PJ, Conta_interno, Conta, `Agência`, Produto, CNPJ, Cliente,
        `Período_início`, `Período_fim`, `Data_consulta`, Contrato_6, Arquivo,
        Arquivo_tipo_tabela, Arquivo_tipo, Arquivo_fonte
      ) %>%
      rename(Empreendimento = Cliente)
    return(extratos_t)
  }

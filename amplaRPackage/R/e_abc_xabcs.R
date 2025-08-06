#' @title Consolidação dos dados dos extratos do Banco ABC
#'
#' @description
#' Consolida e processa dados de múltiplos extratos em Excel do Banco ABC,
#' combinando-os em um único tibble.
#'
#' @param f_caminho.pasta.extratos_c Caminho completo para a pasta que
#'   contém os arquivos Excel dos extratos do Banco ABC.
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos Excel (.xlsx)
#' que contenham extratos do Banco ABC. Para cada arquivo encontrado, chama
#' a função \code{e_abc_xabc} para realizar a extração dos dados e,
#' posteriormente, consolida os resultados em um único tibble.
#'
#' @return
#' Retorna uma lista com um elemento:
#' - \code{xabc_l}: Tibble consolidado com dados de todos os extratos ABC
#'   processados, contendo as seguintes colunas:
#'   - data: Date da transação
#'   - valor: Numeric do valor da transação
#'   - saldo: Numeric do saldo após a transação
#'   - descricao: Character com descrição da transação
#'   - empresa: Character com nome do cliente
#'   - cnpj: Character com CNPJ do cliente
#'   - agencia: Character com número da agência
#'   - conta: Character com número da conta
#'   - periodo.inicio: Date de início do período do extrato
#'   - periodo.fim: Date de fim do período do extrato
#'   - data.consulta: POSIXct de quando o extrato foi gerado
#'   - arquivo: Character com caminho do arquivo original
#'   - banco: Character com nome do banco
#'   - documento: Character com número do documento da transação
#'   - operacao: Character com código da operação
#'   - tipo.valor: Character com tipo do valor
#'   - complemento: Character com informações complementares
#'   - arquivo.tabela.tipo: Character sempre "xabc_l"
#'   - arquivo.tipo: Character sempre "xabc"
#'   - arquivo.fonte: Character sempre "abc"
#'
#' @examples
#' \dontrun{
#' extratos <- e_abc_xabcs(
#'   f_caminho.pasta.extratos_c = "caminho/para/a/pasta/dos/extratos"
#' )
#' print(extratos$xabc_l)
#' }
#'
#' @importFrom dplyr bind_rows distinct mutate as_tibble
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
e_abc_xabcs <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    extratos_l <- list()
    extratos_t <- data.frame()

    for (i_caminho.extrato.abc_c in e_metadados("xabc")$caminho) {
      extrato <- tryCatch(
        {
          e_abc_xabc(i_caminho.extrato.abc_c)
        },
        error = function(e) {
          message(sprintf(
            "Falha ao extrair: %s - %s",
            basename(i_caminho.extrato.abc_c), e$message
          ))
          return(NULL)
        }
      )

      if (!is.null(extrato) && nrow(extrato) > 0) {
        message(sprintf(
          "Arquivo extraído com sucesso: %s",
          basename(i_caminho.extrato.abc_c)
        ))
        extratos_l[[i_caminho.extrato.abc_c]] <- extrato
        extratos_t <- bind_rows(extratos_t, extrato)
      } else {
        message(sprintf(
          "Arquivo vazio ou não extraído: %s",
          basename(i_caminho.extrato.abc_c)
        ))
      }
    }

    # Verificar se há dados antes de processar
    if (nrow(extratos_t) == 0) {
      message("Nenhum extrato do Banco ABC foi extraído com sucesso.")
      return(list(xabc_l = tibble()))
    }

    xabc.l_t <- extratos_t %>%
      distinct() %>%
      mutate(
        arquivo.tabela.tipo = "xabc_l",
        arquivo.tipo = "xabc",
        arquivo.fonte = "abc"
      ) %>%
      as_tibble()

    list(
      xabc_l = xabc.l_t
    )
  }

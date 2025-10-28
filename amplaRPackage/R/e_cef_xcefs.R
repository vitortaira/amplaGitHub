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
#' \code{e_cef_xcef} para realizar a extração dos dados e, posteriormente,
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
#' extratos <- e_cef_xcef
#'   f_caminho.pasta.extratos_c = "caminho/para/a/pasta/dos/extratos"
#' )
#' print(extratos)
#' }
#'
#' @export

e_cef_xcefs<-
  function(
    f_caminho.pasta.extratos_c = caminhos_pastas("extratos"),
    arquivo.subtipo = "melhores"
  ) {
    # Validando parâmetros
    arquivo.subtipo <- match.arg(arquivo.subtipo, c("todos", "melhores"))
    # Caminhos dos arquivos a serem extraídos
    caminhos <- if (arquivo.subtipo == "melhores") {
      c_extratos(arquivo.tipo = "xcef", arquivo.subtipo = "melhores")$caminho
    } else {
      c_extratos(arquivo.tipo = "xcef", arquivo.subtipo = "todos")$caminho
    }
    # Obter lista atualizada de contratos PJ
    contratos_pj <- c(
      e_cef_nplpjs()$contrato.6.ultimo, e_cef_nplpjs()$contrato.6.penultimo
    )

    # Mensagem informando o número de extratos identificados
    # n_extratos <- length(caminhos.extratos.cef_c)
    # message(sprintf(
    #   "%d extratos da CEF foram identificados na rede.",
    #   n_extratos
    # ))

    extratos_l <- list()
    extratos_t <- data.frame()
    for (
      i_caminho.extrato.cef_c in caminhos
    ) {
      extrato <- tryCatch(
        e_cef_xcef(i_caminho.extrato.cef_c),
        error = function(e) {
          message(sprintf("Falha ao extrair: %s", basename(i_caminho.extrato.cef_c)))
          return(NULL)
        }
      )
      if (!is.null(extrato) && nrow(extrato) > 0) {
        message(sprintf("Arquivo extraído com sucesso: %s (subtipo: %s)",
                       basename(i_caminho.extrato.cef_c),
                       unique(extrato$arquivo.subtipo)[1]))
        extratos_l[[i_caminho.extrato.cef_c]] <- extrato
        extratos_t <- bind_rows(extratos_t, extrato)
      } else {
        message(sprintf("Arquivo vazio ou não extraído: %s", basename(i_caminho.extrato.cef_c)))
      }
    }

    # Verificar se há dados antes de processar
    if (nrow(extratos_t) == 0) {
      message("Nenhum extrato foi extraído com sucesso.")
      return(tibble())
    }

    # Garantir que colunas xcef2 existam mesmo se nenhum arquivo xcef2 foi processado
    if (!"cpf.cnpj" %in% names(extratos_t)) {
      extratos_t$cpf.cnpj <- NA_character_
    }
    if (!"nome.razao" %in% names(extratos_t)) {
      extratos_t$nome.razao <- NA_character_
    }

    # Mostrar resumo dos subtipos processados
    subtipos_encontrados <- table(extratos_t$arquivo.subtipo)
    message("Subtipos processados:")
    for(subtipo in names(subtipos_encontrados)) {
      message(sprintf("  %s: %d registros", subtipo, subtipos_encontrados[subtipo]))
    }

    # Verificar se há colunas xcef2 específicas
    tem_cpf_cnpj <- "cpf.cnpj" %in% names(extratos_t)
    tem_nome_razao <- "nome.razao" %in% names(extratos_t)
    message(sprintf("Colunas xcef2 presentes: cpf.cnpj=%s, nome.razao=%s",
                   tem_cpf_cnpj, tem_nome_razao))

    extratos_t %<>%
      mutate(
        empresa = case_when(
          !is.na(empresa) & str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
          !is.na(empresa) & str_detect(empresa, "(?i)metro\\s?vila\\s?sonia") ~ "AVS",
          !is.na(empresa) & str_detect(empresa, "(?i)grauca") ~ "GRA",
          !is.na(empresa) & str_detect(empresa, "(?i)incorflora") ~ "INC",
          !is.na(empresa) & str_detect(empresa, "(?i)sao\\s?l") ~ "LUC",
          !is.na(empresa) & str_detect(empresa, "(?i)pompeia") ~ "POM",
          !is.na(empresa) & str_detect(empresa, "(?i)up\\s?s\\.") ~ "SAU",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
          TRUE ~ empresa
          # TRUE ~ NA_character_
        ),
        natureza = case_when(
          ((descricao == "CR DESBLOQ") | (descricao == "CRE D IMOB")) &
            (documento %in% contratos_pj) ~ "pj",
          (((descricao == "CR DESBLOQ") | (descricao == "CRE D IMOB")) &
            !(documento %in% contratos_pj)) |
            (descricao == "DESB CR CX") |
            (descricao == "DESBL.SALD") ~ "repasse",
          TRUE ~ NA_character_
        ),
        contrato.6 =
          documento %>% str_pad(width = 6, side = "left", pad = "0"),
        contrato.5 = if_else(
          arquivo.subtipo == "xcef6",
          str_sub(contrato.6, start = 1, end = 5),
          str_sub(contrato.6, start = 2, end = 6),
        ),
        arquivo.tabela.tipo = "xcef",
        arquivo.tipo = "xcef",
        arquivo.fonte = "cef"
      ) %>%
      as_tibble() %>%
      select(
        data.lancamento, data.movimentacao, documento, descricao, valor, saldo,
        natureza, conta.interno, conta, agencia, produto, cnpj, empresa,
        periodo.inicio, periodo.fim, data.consulta, contrato.6, arquivo,
        arquivo.subtipo, arquivo.tabela.tipo, arquivo.tipo, arquivo.fonte,
        cpf.cnpj, nome.razao
      )
    return(extratos_t)
  }

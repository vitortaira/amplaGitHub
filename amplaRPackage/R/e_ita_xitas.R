#' @title Consolidação dos dados dos extratos bancários do Itaú
#'
#' @description
#' A função **e_ita_xitas** extrai e consolida os dados de todos os extratos
#' bancários do Itaú disponíveis na pasta de extratos. Para cada arquivo de
#' extrato encontrado, a função chama `e_ita_xita()` para realizar a extração
#' dos dados e, posteriormente, consolida os resultados em uma única tabela.
#'
#' @param f_caminho.pasta.extratos_c Caminho para a pasta contendo os extratos.
#'   Por padrão, utiliza o caminho relativo baseado na estrutura do projeto
#'   retornado por `caminhos_pastas("extratos")`.
#'
#' @details
#' A função realiza as seguintes operações:
#' \enumerate{
#'   \item Busca todos os arquivos de extratos do Itaú através de `e_metadados("xita")`
#'   \item Para cada arquivo encontrado, tenta extrair os dados usando `e_ita_xita()`
#'   \item Consolida todos os dados extraídos em uma única tabela
#'   \item Padroniza os códigos das empresas usando mapeamento interno
#'   \item Remove entradas relacionadas a saldos
#'   \item Adiciona metadados sobre o tipo de arquivo e fonte
#' }
#'
#' O mapeamento de empresas inclui:
#' \itemize{
#'   \item AMP: Ampla Incorporadora
#'   \item AVS: Metro VS e I
#'   \item CBL: Campo Belo
#'   \item ENC: Nova Civil
#'   \item GRA: Grauca
#'   \item INC: Incorflora
#'   \item JSP: Jardim São Paulo
#'   \item POM: Pompeia
#'   \item SAU: Saúde
#'   \item SN2: Sonia II
#'   \item SN4: Sonia IV
#'   \item USL: Sale
#'   \item LUC: São Lucas
#'   \item SOC: Socorro
#' }
#'
#' @return
#' Retorna uma lista com um elemento:
#'   \item{xita_l}{Tibble consolidado com os dados de todos os extratos do Itaú,
#'     incluindo colunas padronizadas para empresa, descrição, valores, datas e
#'     metadados sobre o tipo de arquivo.}
#'
#' @examples
#' \dontrun{
#' # Extrair e consolidar todos os extratos do Itaú
#' extratos_ita <- e_ita_xitas()
#' print(extratos_ita$xita_l)
#'
#' # Usar caminho personalizado
#' extratos_ita <- e_ita_xitas(
#'   f_caminho.pasta.extratos_c = "caminho/para/extratos"
#' )
#' }
#'
#' @seealso
#' \code{\link{e_ita_xita}} para extração de um único extrato,
#' \code{\link{e_metadados}} para obtenção dos metadados dos arquivos,
#' \code{\link{caminhos_pastas}} para definição dos caminhos das pastas.
#'
#' @importFrom dplyr mutate case_when filter bind_rows
#' @importFrom stringr str_detect str_starts
#' @importFrom tibble tibble
#'
#' @export

e_ita_xitas <-
  function(f_caminho.pasta.extratos_c = caminhos_pastas("extratos")) {
    extratos_l <- list()
    extratos_t <- tibble()

    for (i_caminho.extrato.ita_c in e_metadados("xita")$caminho) {
      extrato <- tryCatch(
        {
          e_ita_xita(i_caminho.extrato.ita_c)$xita_l
        },
        error = function(e) {
          message(sprintf("Falha ao extrair: %s - %s", basename(i_caminho.extrato.ita_c), e$message))
          return(NULL)
        }
      )

      if (!is.null(extrato) && nrow(extrato) > 0) {
        message(sprintf("Arquivo extraído com sucesso: %s", basename(i_caminho.extrato.ita_c)))
        extratos_l[[i_caminho.extrato.ita_c]] <- extrato
        extratos_t <- bind_rows(extratos_t, extrato)
      } else {
        message(sprintf("Arquivo vazio ou não extraído: %s", basename(i_caminho.extrato.ita_c)))
      }
    }

    # Verificar se há dados antes de processar
    if (nrow(extratos_t) == 0) {
      message("Nenhum extrato ITAÚ foi extraído com sucesso.")
      return(list(xita_l = tibble()))
    }

    # Verificar se a coluna empresa existe, se não, criar com NA
    if (!"empresa" %in% names(extratos_t)) {
      extratos_t$empresa <- NA_character_
    }

    xita.l_t <- extratos_t %>%
      mutate(
        empresa = case_when(
          !is.na(empresa) & str_detect(empresa, "(?i)ampla\\s?incorporadora") ~ "AMP",
          !is.na(empresa) & str_detect(empresa, "(?i)metro\\s?v\\s?s\\s?e\\s?i") ~ "AVS",
          !is.na(empresa) & str_detect(empresa, "(?i)campo\\s?belo") ~ "CBL",
          !is.na(empresa) & str_detect(empresa, "(?i)nova\\s?civil") ~ "ENC",
          !is.na(empresa) & str_detect(empresa, "(?i)grauca") ~ "GRA",
          !is.na(empresa) & str_detect(empresa, "(?i)incorflora") ~ "INC",
          !is.na(empresa) & str_detect(empresa, "(?i)jd\\s?sao\\s?paulo") ~ "JSP",
          !is.na(empresa) & str_detect(empresa, "(?i)pompeia") ~ "POM",
          !is.na(empresa) & str_detect(empresa, "(?i)saude") ~ "SAU",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?ii") ~ "SN2",
          !is.na(empresa) & str_detect(empresa, "(?i)sonia\\s?iv") ~ "SN4",
          !is.na(empresa) & str_detect(empresa, "(?i)sale") ~ "USL",
          !is.na(empresa) & str_detect(empresa, "(?i)sao\\s?lucas") ~ "LUC",
          !is.na(empresa) & str_detect(empresa, "(?i)socorro") ~ "SOC",
          TRUE ~ empresa
        ),
        mutuo = if_else(
          str_detect(descricao, "(?i)ampla|buti[aá]|grau[cç][aá]"),
          TRUE,
          FALSE
        ),
        arquivo.tabela.tipo = "xita_l",
        arquivo.tipo = "xita",
        arquivo.fonte = "ita"
      ) %>%
      dplyr::filter(
        !str_starts(descricao, "(?i)saldo") &
          !str_detect(descricao, "(?i)saldo\\s?a\\s?liberar")
      )
    list(
      xita_l = xita.l_t
    )
  }

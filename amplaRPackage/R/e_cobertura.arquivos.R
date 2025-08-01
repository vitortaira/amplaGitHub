#' Extrai cobertura de arquivos CEF e ITAÚ
#'
#' @description
#' Função para extrair e consolidar informações de cobertura temporal de arquivos
#' CEF (extratos bancários) e ITAÚ, retornando um tibble com dados combinados.
#'
#' @return Um tibble com colunas: arquivo, arquivo.subtipo, empresa, conta,
#'   periodo.inicio, periodo.fim, arquivo.tipo
#' @importFrom dplyr left_join bind_rows distinct filter mutate select rename
#' @importFrom stringr str_remove str_sub
#' @importFrom tibble tibble
#' @importFrom fs path_file
#' @export
#'
e_cobertura.arquivos <- function() {
  # Obter dados CEF
  dadosCef <- tryCatch(
    {
      e_cef_xcefs()
    },
    error = function(e) {
      message("Erro ao extrair dados CEF: ", e$message)
      return(tibble::tibble(
        arquivo = character(), arquivo.subtipo = character(),
        conta = character(),
        periodo.inicio = as.Date(character()), periodo.fim = as.Date(character())
      ))
    }
  )

  # Obter dados ITAÚ
  dadosIta <- tryCatch(
    {
      e_ita_xitas()$xita_l
    },
    error = function(e) {
      message("Erro ao extrair dados ITAÚ: ", e$message)
      return(tibble::tibble(
        arquivo = character(), arquivo.subtipo = character(),
        conta = character(),
        periodo.inicio = as.Date(character()), periodo.fim = as.Date(character())
      ))
    }
  )

  # Garantir que as colunas necessárias existam (excluindo empresa que vem dos metadados)
  colunasNecessarias <- c("arquivo", "arquivo.subtipo", "conta", "periodo.inicio", "periodo.fim")

  for (col in colunasNecessarias) {
    if (!col %in% names(dadosCef)) {
      if (col %in% c("periodo.inicio", "periodo.fim")) {
        dadosCef[[col]] <- as.Date(NA)
      } else {
        dadosCef[[col]] <- NA_character_
      }
    }
    if (!col %in% names(dadosIta)) {
      if (col %in% c("periodo.inicio", "periodo.fim")) {
        dadosIta[[col]] <- as.Date(NA)
      } else {
        dadosIta[[col]] <- NA_character_
      }
    }
  }

  # Selecionar apenas colunas necessárias dos dados extraídos, excluindo empresa para evitar duplicatas
  dadosCefLimpos <- dadosCef %>%
    dplyr::select(arquivo, arquivo.subtipo, conta, periodo.inicio, periodo.fim)

  dadosItaLimpos <- dadosIta %>%
    dplyr::select(arquivo, arquivo.subtipo, conta, periodo.inicio, periodo.fim)

  xcef_t <- dplyr::left_join(
    e_metadados("xcef") %>%
      dplyr::rename(arquivo = "caminho"),
    dadosCefLimpos,
    by = "arquivo"
  )

  extita_t <- dplyr::left_join(
    e_metadados("xita") %>%
      dplyr::rename(arquivo = "caminho"),
    dadosItaLimpos,
    by = "arquivo"
  )

  coberturaCompleta <- dplyr::bind_rows(
    xcef_t,
    extita_t
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      banco = stringr::str_remove(fs::path_file(arquivo), "^[^-]*-") %>%
        stringr::str_remove("_.*"),
      id.corrente = stringr::str_c(
        empresa,
        "-",
        banco,
        "_",
        stringr::str_remove_all(conta, "-") %>%
          stringr::str_sub(-4, -1)
      ),
      id = {
        # Safe ID assignment with proper error handling
        if (is.null(contasBancarias) || nrow(contasBancarias) == 0) {
          message("contasBancarias não está disponível. IDs serão definidos como NA.")
          rep(NA_character_, length(id.corrente))
        } else if (!all(c("id.antigo", "id.atual", "id.continuo") %in% names(contasBancarias))) {
          message("contasBancarias não possui as colunas necessárias. IDs serão definidos como NA.")
          rep(NA_character_, length(id.corrente))
        } else {
          message(sprintf("contasBancarias carregado com %d registros.", nrow(contasBancarias)))

          # Safe case_when with available data
          dplyr::case_when(
            id.corrente %in% contasBancarias$id.antigo
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.antigo)
              ],
            id.corrente %in% contasBancarias$id.atual
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.atual)
              ],
            TRUE ~ NA_character_
          )
        }
      }
    )

  message(sprintf("Total de registros antes da filtragem: %d", nrow(coberturaCompleta)))

  # Garantir que as colunas necessárias existam após o join
  if (!"empresa" %in% names(coberturaCompleta)) {
    coberturaCompleta$empresa <- NA_character_
    message("Coluna 'empresa' criada com valores NA")
  }
  if (!"arquivo.tipo" %in% names(coberturaCompleta)) {
    coberturaCompleta$arquivo.tipo <- NA_character_
    message("Coluna 'arquivo.tipo' criada com valores NA")
  }
  if (!"conta" %in% names(coberturaCompleta)) {
    coberturaCompleta$conta <- NA_character_
    message("Coluna 'conta' criada com valores NA")
  }

  # Verificar quantos registros têm empresa não-NA
  registrosComEmpresa <- sum(!is.na(coberturaCompleta$empresa) & coberturaCompleta$empresa != "", na.rm = TRUE)
  message(sprintf("Registros com empresa válida: %d", registrosComEmpresa))

  # Se não há empresas válidas, vamos manter todos os dados e usar arquivo.tipo
  if (registrosComEmpresa == 0) {
    message("Nenhum registro com empresa válida encontrado. Mantendo todos os dados.")
    # Não filtrar por empresa, apenas garantir que arquivo.tipo existe
  } else {
    # Filtrar apenas registros com empresa não-NA
    coberturaCompleta <- coberturaCompleta %>%
      dplyr::filter(!is.na(empresa) & empresa != "")
  }

  # Verificar se a coluna conta existe antes de modificá-la
  if ("conta" %in% names(coberturaCompleta)) {
    coberturaCompleta <- coberturaCompleta %>%
      dplyr::mutate(conta = stringr::str_remove(conta, "-") %>% stringr::str_sub(-4, -1))
  }

  message(sprintf("Total de registros retornados: %d", nrow(coberturaCompleta)))
  if (nrow(coberturaCompleta) > 0) {
    message(sprintf("Colunas disponíveis: %s", paste(names(coberturaCompleta), collapse = ", ")))
  }

  return(coberturaCompleta)
}

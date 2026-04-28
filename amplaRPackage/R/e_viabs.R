#' @title Consolidação dos dados de viabilidade a partir do Google Drive
#'
#' @description
#' A função **e_viabs** acessa uma pasta no Google Drive, busca todos os
#' arquivos cujo nome começa com "viabilidade" (sem distinção de maiúsculas)
#' e termina com ".xlsx", e extrai dados consolidados de cada um deles.
#'
#' @param f_id_pasta_gdrive_c Código (ID) da pasta no Google Drive que contém
#'   os arquivos de viabilidade.
#'
#' @details
#' A função executa as seguintes etapas:
#' \enumerate{
#'   \item Autentica no Google Drive via \code{googledrive} (usa credenciais
#'     já armazenadas em cache; solicita autenticação interativa se necessário).
#'   \item Lista todos os itens (arquivos e subpastas) diretamente na pasta
#'     informada.
#'   \item Busca arquivos correspondentes ao padrão
#'     \code{(?i)^viabilidade.*\\.xlsx$} tanto na pasta raiz quanto em cada
#'     subpasta de primeiro nível.
#'   \item Faz o download de cada arquivo para uma pasta temporária.
#'   \item Extrai os dados de cada arquivo chamando \code{e_viab()}.
#'   \item Consolida os resultados de todos os arquivos em uma única tabela.
#' }
#'
#' @return
#' Retorna uma lista nomeada de tibbles, com um elemento por aba extraida
#' (atualmente: \code{def} e \code{flx}) e mais um tibble \code{check} com
#' a conferência entre as duas abas. Cada tibble consolida os dados
#' correspondentes de todos os arquivos encontrados, com a coluna
#' \code{empreendimento} (primeira coluna) identificando o arquivo de origem.
#' O tibble \code{check} possui as colunas \code{empreendimento},
#' \code{variavel}, \code{def}, \code{flx} e \code{diferenca}, contendo
#' apenas as variáveis que aparecem em ambas as abas.
#'
#' @examples
#' \dontrun{
#' viabs <- e_viabs("1AbCdEfGhIjKlMnOpQrStUvWxYz")
#' print(viabs)
#' }
#'
#' @importFrom googledrive drive_ls as_id drive_download
#' @importFrom purrr map map_dfr set_names
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate group_by summarise inner_join arrange
#' @importFrom tidyr pivot_longer
#' @importFrom fs path
#'
#' @export

e_viabs <- function(f_id_pasta_gdrive_c) {
  padrao_viab_c <- "(?i)^viabilidade.*\\.xlsx$"

  # Auxiliar: filtra dribble por padrão de nome de arquivo
  filtrar_viab <- function(dribble_t) {
    dribble_t[str_detect(dribble_t$name, padrao_viab_c), ]
  }

  # Auxiliar: se houver mais de um arquivo elegível, pede ao usuário para
  # escolher um único; caso contrário, retorna o que tiver.
  escolher_unico <- function(dribble_t, nome_pasta_c) {
    if (nrow(dribble_t) <= 1) {
      return(dribble_t)
    }
    if (!interactive()) {
      stop(
        "Multiple viability files found in '", nome_pasta_c,
        "'. Run in an interactive session to choose one.",
        call. = FALSE
      )
    }
    cat("\nMultiple viability files found in '",
      nome_pasta_c, "':\n",
      sep = ""
    )
    escolha_n <- utils::menu(
      choices = dribble_t$name,
      title = "Choose the file to use:"
    )
    if (escolha_n == 0) {
      stop(
        "Selection cancelled by user for folder '", nome_pasta_c, "'.",
        call. = FALSE
      )
    }
    dribble_t[escolha_n, ]
  }

  # Lista todos os itens na pasta raiz
  itens.raiz_t <- googledrive::drive_ls(
    path = googledrive::as_id(f_id_pasta_gdrive_c)
  )

  # Arquivos viabilidade diretamente na raiz (escolha única se houver vários)
  arquivos.raiz_t <- escolher_unico(filtrar_viab(itens.raiz_t), "(raiz)")

  # Subpastas na raiz (itens sem extensão .xlsx)
  subpastas_t <- itens.raiz_t[
    !str_detect(itens.raiz_t$name, "\\.xlsx$"),
  ]

  # Arquivos viabilidade dentro de cada subpasta (um nível de profundidade);
  # garante um único arquivo por subpasta
  arquivos.sub_t <- map_dfr(
    seq_len(nrow(subpastas_t)),
    ~ {
      nome_pasta_c <- subpastas_t$name[[.x]]
      conteudo_t <- tryCatch(
        googledrive::drive_ls(path = subpastas_t[.x, ]),
        error = function(e) NULL
      )
      if (is.null(conteudo_t)) {
        return(NULL)
      }
      escolher_unico(filtrar_viab(conteudo_t), nome_pasta_c)
    }
  )

  # Consolida todos os arquivos viabilidade encontrados
  arquivos.viab_t <- dplyr::bind_rows(arquivos.raiz_t, arquivos.sub_t)

  if (nrow(arquivos.viab_t) == 0) {
    stop("No viability file found in the given folder.", call. = FALSE)
  }

  # Process a single file: download to a temp file, extract, then delete.
  # The temp file is removed even if extraction fails, so nothing accumulates
  # on disk across runs.
  processar_viab <- function(linha_dribble) {
    nome_c <- linha_dribble$name
    caminho.temp_c <- tempfile(
      pattern = "viab_",
      fileext = paste0("_", nome_c)
    )
    on.exit(unlink(caminho.temp_c, force = TRUE), add = TRUE)
    googledrive::drive_download(
      file = linha_dribble,
      path = caminho.temp_c,
      overwrite = TRUE
    )
    abas_l <- e_viab(caminho.temp_c)
    # Derive empreendimento from the file name the same way e_viab_flx does
    # (drop extension, strip the "Viabilidade " prefix), so that all tabs
    # use a consistent empreendimento value.
    empreendimento_c <- stringr::str_remove(
      tools::file_path_sans_ext(nome_c),
      "(?i).*viabilidade\\s+"
    )
    purrr::map(abas_l, function(tab_t) {
      if (nrow(tab_t) == 0) {
        return(tab_t)
      }
      # If the tab already identifies the project (e.g. flx), don't add
      # empreendimento. Otherwise, prepend it.
      if ("empreendimento" %in% names(tab_t)) {
        return(tab_t)
      }
      tab_t %>%
        mutate(empreendimento = empreendimento_c) %>%
        dplyr::relocate(empreendimento)
    })
  }

  # Process each file: yields a list of per-file lists of tibbles
  por_arquivo_l <- purrr::map(
    seq_len(nrow(arquivos.viab_t)),
    ~ processar_viab(arquivos.viab_t[.x, ])
  )

  # Transpose and bind: one consolidated tibble per tab name
  nomes_abas_c <- names(por_arquivo_l[[1]])
  resultado_l <- purrr::set_names(
    purrr::map(nomes_abas_c, function(nome_aba_c) {
      dplyr::bind_rows(purrr::map(por_arquivo_l, nome_aba_c))
    }),
    nomes_abas_c
  )

  # Tibble de conferência: compara, por empreendimento, cada variável de
  # `def` cujo nome também apareça como valor de `variavel` em `flx`.
  # Mantém apenas as variáveis com correspondência nas duas abas.
  if (all(c("def", "flx") %in% names(resultado_l))) {
    def_t <- resultado_l$def
    flx_t <- resultado_l$flx

    flx_soma_t <- flx_t %>%
      dplyr::group_by(empreendimento, variavel) %>%
      dplyr::summarise(flx = sum(valor, na.rm = TRUE), .groups = "drop")

    def_long_t <- def_t %>%
      tidyr::pivot_longer(
        cols = -empreendimento,
        names_to = "variavel",
        values_to = "def"
      )

    resultado_l$check <- def_long_t %>%
      dplyr::inner_join(
        flx_soma_t,
        by = c("empreendimento", "variavel")
      ) %>%
      dplyr::mutate(diferenca = def - flx) %>%
      dplyr::arrange(empreendimento, variavel)
  }

  resultado_l
}

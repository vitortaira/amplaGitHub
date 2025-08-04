#' @title Extração de Empreendimentos Informakon
#'
#' @description
#' A função e_ik_empr() extrai os dados de empreendimentos dos arquivos na pasta
#' "informakon" e os retorna em um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#'
#' @return Data frame com dados dos empreendimentos consolidados.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' empreendimentos_df <- e_ik_empr()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select rename case_when n filter
#' @importFrom stringr str_sub str_detect str_extract str_remove
#' @importFrom fs dir_ls
#' @importFrom tibble tibble
#' @export
e_ik_empr <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("informakon")) {
  # Função interna para buscar o arquivo de empreendimentos mais recente
  obter_caminho_empreendimentos <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }
    # Busca arquivos que começam com "empreendimentos_" e ignora os "Informakon"
    caminhos_empr <- fs::dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_empr <- caminhos_empr[
      basename(caminhos_empr) %>% stringr::str_detect("^empreendimentos") &
        !basename(caminhos_empr) %>% stringr::str_detect("^Informakon")
    ]
    if (length(caminhos_empr) == 0) {
      stop("Nenhum arquivo de empreendimentos encontrado na pasta informakon.")
    }
    # Determina a data final (YYYYMMDD) mais recente
    data_final_por_arquivo <- sapply(caminhos_empr, function(path) {
      basename(path) %>%
        stringr::str_extract("[^_]+$") %>%
        stringr::str_remove("\\.xlsx$") %>%
        as.Date(format = "%Y%m%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_empr[indice_recente]
  }

  # Carrega o arquivo de empreendimentos mais recente
  caminho_arquivo_empreendimentos <- obter_caminho_empreendimentos()

  # Tenta ler o arquivo com diferentes opções de skip para encontrar os headers corretos
  # Primeiro tenta sem skip
  empreendimentos_raw <- tryCatch(
    {
      # Suprimir warnings sobre New names usando suppressMessages
      temp_df <- suppressMessages(readxl::read_excel(caminho_arquivo_empreendimentos))
      # Verifica se a primeira linha parece ser header
      if (all(is.na(temp_df[1, ])) || any(stringr::str_detect(names(temp_df), "^\\.\\.\\.\\d+$"))) {
        # Se tem colunas unnamed, tenta com skip = 1
        suppressMessages(readxl::read_excel(caminho_arquivo_empreendimentos, skip = 1))
      } else {
        temp_df
      }
    },
    error = function(e) {
      suppressMessages(readxl::read_excel(caminho_arquivo_empreendimentos, skip = 1))
    }
  )

  # Remover colunas vazias ou sem nome útil
  colunas_validas <- !stringr::str_detect(names(empreendimentos_raw), "^\\.\\.\\.\\d+$") & 
                     !names(empreendimentos_raw) %in% c("", NA)
  empreendimentos_raw <- empreendimentos_raw[, colunas_validas, drop = FALSE]

  # Mostra as colunas disponíveis no arquivo para debug
  message(sprintf("Colunas disponíveis no arquivo: %s", paste(names(empreendimentos_raw), collapse = ", ")))

  # Verifica se as colunas esperadas existem e faz o mapeamento
  # Ajuste os nomes das colunas conforme a estrutura real do arquivo
  colunas_mapeamento <- list(
    "codigo.empreendimento" = c("Empreendimento", "Código", "codigo", "cod", "Code", "Id"),
    "nome.empreendimento" = c("Nome do Empreendimento", "Nome", "nome_empreendimento", "Name", "Descrição"),
    "nucleo" = c("Núcleo", "nucleo", "Core", "Nucleus"),
    "empresa" = c("Empresa", "empresa", "Company"),
    "filial" = c("Filial", "filial", "Branch"),
    "observacoes" = c("Obs", "obs", "Observações", "observacoes", "Notes"),
    "criado.por" = c("Criado por", "criado_por", "Created By"),
    "criado.em" = c("Criado em", "criado_em", "Created At"),
    "alterado.por" = c("Alterado por", "alterado_por", "Modified By"),
    "alterado.em" = c("Alterado em", "alterado_em", "Modified At")
  )

  # Função para encontrar coluna correspondente
  encontrar_coluna <- function(nomes_possiveis, colunas_disponiveis) {
    for (nome in nomes_possiveis) {
      if (nome %in% colunas_disponiveis) {
        return(nome)
      }
    }
    return(NA_character_)
  }

  # Mapeia as colunas disponíveis
  colunas_encontradas <- lapply(colunas_mapeamento, encontrar_coluna, names(empreendimentos_raw))

  # Cria tibble com colunas padronizadas
  empreendimentos_df <- empreendimentos_raw %>%
    dplyr::mutate(
      # Cria colunas padronizadas usando as encontradas ou NA
      codigo.empreendimento = if (!is.na(colunas_encontradas$codigo.empreendimento)) {
        as.character(.data[[colunas_encontradas$codigo.empreendimento]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      nome.empreendimento = if (!is.na(colunas_encontradas$nome.empreendimento)) {
        as.character(.data[[colunas_encontradas$nome.empreendimento]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      nucleo = if (!is.na(colunas_encontradas$nucleo)) {
        as.character(.data[[colunas_encontradas$nucleo]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      empresa = if (!is.na(colunas_encontradas$empresa)) {
        as.character(.data[[colunas_encontradas$empresa]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      filial = if (!is.na(colunas_encontradas$filial)) {
        as.character(.data[[colunas_encontradas$filial]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      observacoes = if (!is.na(colunas_encontradas$observacoes)) {
        as.character(.data[[colunas_encontradas$observacoes]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      criado.por = if (!is.na(colunas_encontradas$criado.por)) {
        as.character(.data[[colunas_encontradas$criado.por]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      criado.em = if (!is.na(colunas_encontradas$criado.em)) {
        as.Date(.data[[colunas_encontradas$criado.em]])
      } else {
        rep(as.Date(NA), dplyr::n())
      },
      alterado.por = if (!is.na(colunas_encontradas$alterado.por)) {
        as.character(.data[[colunas_encontradas$alterado.por]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      alterado.em = if (!is.na(colunas_encontradas$alterado.em)) {
        as.Date(.data[[colunas_encontradas$alterado.em]])
      } else {
        rep(as.Date(NA), dplyr::n())
      },
      # Metadados do arquivo
      arquivo = caminho_arquivo_empreendimentos,
      arquivo.tabela.tipo = "empr",
      arquivo.tipo = "empr",
      arquivo.fonte = "ik"
    ) %>%
    # Remove linhas que parecem ser headers
    dplyr::filter(!is.na(.data$codigo.empreendimento) &
      .data$codigo.empreendimento != "" &
      !stringr::str_detect(.data$codigo.empreendimento, "^(Código|codigo|Code|Empreendimento)$")) %>%
    dplyr::select(
      "codigo.empreendimento", "nome.empreendimento", "nucleo", "empresa", "filial",
      "observacoes", "criado.por", "criado.em", "alterado.por", "alterado.em",
      "arquivo", "arquivo.tabela.tipo", "arquivo.tipo", "arquivo.fonte"
    )

  message(sprintf("Total de empreendimentos extraídos: %d", nrow(empreendimentos_df)))
  message(sprintf("Arquivo fonte: %s", basename(caminho_arquivo_empreendimentos)))

  # Mostra quais colunas foram encontradas
  colunas_encontradas_nomes <- sapply(names(colunas_encontradas), function(x) {
    if (!is.na(colunas_encontradas[[x]])) {
      paste0(x, " <- ", colunas_encontradas[[x]])
    } else {
      paste0(x, " <- [NÃO ENCONTRADA]")
    }
  })
  message("Mapeamento de colunas:")
  for (mapeamento in colunas_encontradas_nomes) {
    message(sprintf("  %s", mapeamento))
  }

  return(empreendimentos_df)
}

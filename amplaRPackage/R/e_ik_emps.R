#' @title Extração de Empresas Informakon
#'
#' @description
#' A função e_ik_emps() extrai os dados de empresas dos arquivos na pasta
#' "informakon" e os retorna em um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#'
#' @return Data frame com dados das empresas consolidadas.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' empresas_df <- e_ik_emps()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select rename case_when n filter
#' @importFrom stringr str_sub str_detect str_extract str_remove
#' @importFrom fs dir_ls
#' @importFrom tibble tibble
#' @export
e_ik_emps <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("informakon")) {
  # Função interna para buscar o arquivo de empresas mais recente
  obter_caminho_empresas <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }
    # Busca arquivos que começam com "empresas_" e ignora os "Informakon"
    caminhos_emps <- fs::dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_emps <- caminhos_emps[
      basename(caminhos_emps) %>% stringr::str_detect("^empresas") &
        !basename(caminhos_emps) %>% stringr::str_detect("^Informakon")
    ]
    if (length(caminhos_emps) == 0) {
      stop("Nenhum arquivo de empresas encontrado na pasta informakon.")
    }
    # Determina a data final (YYYYMMDD) mais recente
    data_final_por_arquivo <- sapply(caminhos_emps, function(path) {
      basename(path) %>%
        stringr::str_extract("[^_]+$") %>%
        stringr::str_remove("\\.xlsx$") %>%
        as.Date(format = "%Y%m%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_emps[indice_recente]
  }

  # Carrega o arquivo de empresas mais recente
  caminho_arquivo_empresas <- obter_caminho_empresas()

  # Lê o arquivo Excel
  empresas_raw <- readxl::read_excel(caminho_arquivo_empresas, skip = 1)

  # Mostra as colunas disponíveis no arquivo para debug
  message(sprintf("Colunas disponíveis no arquivo: %s", paste(names(empresas_raw), collapse = ", ")))

  # Verifica se as colunas esperadas existem e faz o mapeamento
  # Ajuste os nomes das colunas conforme a estrutura real do arquivo
  colunas_mapeamento <- list(
    "codigo.empresa" = c("Filial", "Código", "codigo", "cod", "Code", "Id"),
    "nome.filial" = c("Nome da Filial", "Nome", "Razão Social", "razao_social", "empresa", "Name"),
    "cnpj" = c("CNPJ", "cnpj", "documento", "Document"),
    "cidade" = c("Cidade", "cidade", "City"),
    "pais" = c("País", "pais", "Country"),
    "uf" = c("UF", "uf", "estado", "Estado", "State"),
    "municipio" = c("Município", "municipio", "Municipality"),
    "empresa" = c("Empresa", "empresa", "Enterprise", "Code"),
    "razao.social.empresa" = c("Razão Social da Empresa", "razao_social_empresa", "Company Name")
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
  colunas_encontradas <- lapply(colunas_mapeamento, encontrar_coluna, names(empresas_raw))

  # Cria tibble com colunas padronizadas
  empresas_df <- empresas_raw %>%
    dplyr::mutate(
      # Cria colunas padronizadas usando as encontradas ou NA
      codigo.empresa = if (!is.na(colunas_encontradas$codigo.empresa)) {
        as.character(.data[[colunas_encontradas$codigo.empresa]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      nome.filial = if (!is.na(colunas_encontradas$nome.filial)) {
        as.character(.data[[colunas_encontradas$nome.filial]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      cnpj = if (!is.na(colunas_encontradas$cnpj)) {
        as.character(.data[[colunas_encontradas$cnpj]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      cidade = if (!is.na(colunas_encontradas$cidade)) {
        as.character(.data[[colunas_encontradas$cidade]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      pais = if (!is.na(colunas_encontradas$pais)) {
        as.character(.data[[colunas_encontradas$pais]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      uf = if (!is.na(colunas_encontradas$uf)) {
        as.character(.data[[colunas_encontradas$uf]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      municipio = if (!is.na(colunas_encontradas$municipio)) {
        as.character(.data[[colunas_encontradas$municipio]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      empresa = if (!is.na(colunas_encontradas$empresa)) {
        as.character(.data[[colunas_encontradas$empresa]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      razao.social.empresa = if (!is.na(colunas_encontradas$razao.social.empresa)) {
        as.character(.data[[colunas_encontradas$razao.social.empresa]])
      } else {
        rep(NA_character_, dplyr::n())
      },
      # Metadados do arquivo
      arquivo = caminho_arquivo_empresas,
      arquivo.tabela.tipo = "emps",
      arquivo.tipo = "emps",
      arquivo.fonte = "ik"
    ) %>%
    # Remove row if codigo.empresa is exactly "Filial" (header row)
    dplyr::filter(.data$codigo.empresa != "Filial" | is.na(.data$codigo.empresa)) %>%
    dplyr::select(
      "codigo.empresa", "nome.filial", "cnpj", "cidade", "pais", "uf",
      "municipio", "empresa", "razao.social.empresa",
      "arquivo", "arquivo.tabela.tipo", "arquivo.tipo", "arquivo.fonte"
    )

  message(sprintf("Total de empresas extraídas: %d", nrow(empresas_df)))
  message(sprintf("Arquivo fonte: %s", basename(caminho_arquivo_empresas)))

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

  return(empresas_df)
}

#' @title Relatório Consolidado de Empresas e Empreendimentos
#'
#' @description
#' A função r_empresas() consolida dados de empresas (e_ik_emps) com dados de
#' empreendimentos (e_ik_empr), fazendo um full join baseado no código da filial
#' e na combinação empresa.filial dos empreendimentos.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#'
#' @return Tibble com dados consolidados de empresas e empreendimentos.
#'   Colunas incluem informações da empresa (CNPJ, endereço) e empreendimentos
#'   associados (código, nome, núcleo).
#'
#' @details
#' A função realiza as seguintes operações:
#' \itemize{
#'   \item Extrai dados de empresas usando \code{e_ik_emps()}
#'   \item Extrai dados de empreendimentos usando \code{e_ik_empr()}
#'   \item Cria chave de join combinando empresa.filial nos empreendimentos
#'   \item Executa full join preservando registros de ambas as fontes
#'   \item Adiciona metadados sobre a origem dos dados
#' }
#'
#' @examples
#' \dontrun{
#' # Gerar relatório consolidado
#' empresas_consolidadas <- r_empresas()
#'
#' # Verificar empresas sem empreendimentos
#' empresas_sem_empr <- empresas_consolidadas %>%
#'   dplyr::filter(is.na(codigo.empreendimento))
#'
#' # Verificar empreendimentos sem dados da empresa
#' empr_sem_empresa <- empresas_consolidadas %>%
#'   dplyr::filter(is.na(cnpj))
#' }
#'
#' @importFrom dplyr full_join mutate select filter case_when
#' @importFrom stringr str_c str_sub str_extract str_remove
#' @importFrom tibble tibble
#' @export
r_empresas <- function(f_caminho.pasta.ik_c = caminhos_pastas("informakon")) {
  # Extrai dados de empresas
  message("Extraindo dados de empresas...")
  dados_empresas <- e_ik_emps(f_caminho.pasta.ik_c)

  # Extrai dados de empreendimentos
  message("Extraindo dados de empreendimentos...")
  dados_empreendimentos <- e_ik_empr(f_caminho.pasta.ik_c)

  # Cria chave de join para empreendimentos (empresa.filial)
  dados_empreendimentos_com_chave <- dados_empreendimentos %>%
    dplyr::mutate(
      chave_join = paste0(.data$empresa, ".", .data$filial)
    )

  # Renomeia codigo.filial em empresas para chave_join para facilitar o join
  dados_empresas_com_chave <- dados_empresas %>%
    dplyr::mutate(
      chave_join = .data$codigo.filial
    )

  message("Realizando join entre empresas e empreendimentos...")

  # Executa full join preservando todos os registros
  dados_consolidados <- dados_empresas_com_chave %>%
    dplyr::full_join(
      dados_empreendimentos_com_chave,
      by = "chave_join",
      suffix = c(".empresa", ".empreendimento")
    ) %>%
    dplyr::mutate(
      # Indica origem dos dados
      tem_dados_empresa = !is.na(.data$cnpj),
      tem_dados_empreendimento = !is.na(.data$codigo.empreendimento),
      origem_dados = dplyr::case_when(
        .data$tem_dados_empresa & .data$tem_dados_empreendimento ~ "ambos",
        .data$tem_dados_empresa & !.data$tem_dados_empreendimento ~ "apenas_empresa",
        !.data$tem_dados_empresa & .data$tem_dados_empreendimento ~ "apenas_empreendimento",
        TRUE ~ "sem_dados"
      ),
      # Consolida informações de empresa quando disponível em ambas as fontes
      empresa_final = dplyr::case_when(
        !is.na(.data$empresa.empresa) ~ .data$empresa.empresa,
        !is.na(.data$empresa.empreendimento) ~ .data$empresa.empreendimento,
        TRUE ~ NA_character_
      ),
      filial_final = dplyr::case_when(
        !is.na(.data$codigo.filial) ~ stringr::str_extract(.data$codigo.filial, "\\d+$"),
        !is.na(.data$filial) ~ .data$filial,
        TRUE ~ NA_character_
      ),
      # Criar novas colunas conforme solicitado
      codigo.empresa = stringr::str_sub(.data$codigo.filial, 1, 3),
      nome.empresa = .data$razao.social.empresa,
      arquivo = paste(
        ifelse(is.na(.data$arquivo.empresa), "", .data$arquivo.empresa),
        ifelse(is.na(.data$arquivo.empreendimento), "", .data$arquivo.empreendimento),
        sep = "; "
      ) %>% stringr::str_remove("^; |; $") # Remove separadores nas pontas se um dos arquivos for NA
    ) %>%
    dplyr::select(
      # Ordem solicitada pelo usuário
      "codigo.empreendimento",
      "nome.empreendimento",
      "codigo.filial",
      "nome.filial",
      "codigo.empresa",
      "nome.empresa",
      "nucleo",
      "cnpj",
      "municipio",
      "cidade",
      "uf",
      "pais",
      "criado.por",
      "criado.em",
      "alterado.por",
      "alterado.em",
      "observacoes",
      "arquivo"
    )

  # Relatório do join
  total_registros <- nrow(dados_consolidados)
  registros_com_empreendimento <- sum(!is.na(dados_consolidados$codigo.empreendimento), na.rm = TRUE)
  registros_com_empresa <- sum(!is.na(dados_consolidados$cnpj), na.rm = TRUE)
  registros_ambos <- sum(!is.na(dados_consolidados$codigo.empreendimento) & !is.na(dados_consolidados$cnpj), na.rm = TRUE)
  registros_apenas_empresa <- registros_com_empresa - registros_ambos
  registros_apenas_empr <- registros_com_empreendimento - registros_ambos

  message(sprintf("Consolidação concluída:"))
  message(sprintf("  Total de registros: %d", total_registros))
  message(sprintf("  Com dados de ambas as fontes: %d", registros_ambos))
  message(sprintf("  Apenas dados da filial: %d", registros_apenas_empresa))
  message(sprintf("  Apenas dados de empreendimento: %d", registros_apenas_empr)) # Alerta sobre registros órfãos
  if (registros_apenas_empresa > 0) {
    message(sprintf("⚠️  %d filiais sem empreendimentos associados", registros_apenas_empresa))
  }

  if (registros_apenas_empr > 0) {
    message(sprintf("⚠️  %d empreendimentos sem dados da filial", registros_apenas_empr))
  }

  return(dados_consolidados)
}

#' Extrair dados de contas bancárias da Informakon
#'
#' Esta função extrai os dados de contas bancárias dos arquivos da Informakon.
#' Os dados incluem informações sobre agentes financeiros, contas correntes,
#' tipos de conta e outras informações bancárias.
#'
#' A função espera arquivos com as seguintes colunas:
#' Tipo de Agente, Cód. Agente Financeiro, Agente Financeiro, Conta,
#' Tipo de Conta, Nº Agência, Limite de Crédito, Empresa, Filial,
#' Núcleo, Inativo, Praça, Cartão de Crédito
#'
#' @param f_caminho.pasta.ik_c Caminho para a pasta da Informakon.
#'   Por padrão utiliza `caminhos_pastas("informakon")`.
#'
#' @return Um tibble com as colunas padronizadas:
#' \itemize{
#'   \item tipo.agente: Tipo do agente financeiro
#'   \item codigo.agente: Código do agente financeiro
#'   \item agente.financeiro: Nome do banco/instituição financeira
#'   \item conta.corrente: Número da conta corrente
#'   \item tipo.conta: Tipo da conta
#'   \item numero.agencia: Número da agência
#'   \item limite.credito: Limite de crédito
#'   \item empresa: Nome da empresa
#'   \item filial: Nome da filial
#'   \item nucleo: Nome do núcleo
#'   \item inativo: Status ativo/inativo
#'   \item praca: Informação de praça
#'   \item cartao.credito: Possui cartão de crédito
#'   \item saldo.conciliado: Saldo conciliado (quando disponível)
#'   \item saldo.razao: Saldo no razão (quando disponível)
#'   \item saldo.aplicado: Saldo aplicado (quando disponível)
#'   \item pendentes: Valores pendentes (quando disponível)
#'   \item pendentes.futuros: Valores pendentes futuros (quando disponível)
#'   \item disponibilidade: Disponibilidade total (quando disponível)
#'   \item arquivo: Caminho do arquivo fonte
#'   \item arquivo.tabela.tipo: Tipo da tabela ("cnts")
#'   \item arquivo.tipo: Tipo do arquivo ("cnts")
#'   \item arquivo.fonte: Fonte dos dados ("ik")
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' contas <- e_ik_cnts()
#' head(contas)
#' }
e_ik_cnts <- function(f_caminho.pasta.ik_c = caminhos_pastas("informakon")) {
  # Encontrar arquivos de contas disponíveis
  arquivos_contas <- fs::dir_ls(f_caminho.pasta.ik_c,
    glob = "*contas_informakon_*.xlsx"
  ) %>%
    stringr::str_subset("contas_informakon_\\d{8}\\.xlsx") %>%
    sort() # Ordenar por nome (que inclui data)

  if (length(arquivos_contas) == 0) {
    stop("Nenhum arquivo de contas da Informakon foi encontrado.")
  }

  # Tentar ler arquivos do mais recente para o mais antigo
  dados_brutos <- NULL
  arquivo_usado <- NULL

  for (arquivo in rev(arquivos_contas)) {
    tryCatch(
      {
        dados_brutos <- readxl::read_excel(arquivo, sheet = 1)
        arquivo_usado <- arquivo
        break
      },
      error = function(e) {
        message(paste("Erro ao ler arquivo", basename(arquivo), ":", e$message))
        NULL
      }
    )
  }

  if (is.null(dados_brutos)) {
    stop("Nenhum arquivo de contas pôde ser lido com sucesso.")
  }

  # Verificar se tem as colunas esperadas
  colunas_esperadas <- c(
    "Tipo de Agente", "Cód. Agente Financeiro", "Agente Financeiro", "Conta",
    "Tipo de Conta", "Nº Agência", "Limite de Crédito", "Empresa", "Filial",
    "Núcleo", "Inativo", "Praça", "Cartão de Crédito"
  )

  colunas_disponiveis <- names(dados_brutos)
  colunas_faltando <- setdiff(colunas_esperadas, colunas_disponiveis)

  if (length(colunas_faltando) > 0) {
    stop(
      "Colunas esperadas não encontradas: ", paste(colunas_faltando, collapse = ", "),
      "\nColunas disponíveis: ", paste(colunas_disponiveis, collapse = ", ")
    )
  }

  # Informar sobre o arquivo sendo processado
  message("Colunas disponíveis no arquivo: ", paste(colunas_disponiveis, collapse = ", "))

  # Processar dados com a estrutura esperada
  contas_final <- dados_brutos %>%
    dplyr::rename(
      tipo.agente = `Tipo de Agente`,
      codigo.agente = `Cód. Agente Financeiro`,
      agente.financeiro = `Agente Financeiro`,
      conta.corrente = Conta,
      tipo.conta = `Tipo de Conta`,
      numero.agencia = `Nº Agência`,
      limite.credito = `Limite de Crédito`,
      empresa = Empresa,
      filial = Filial,
      nucleo = Núcleo,
      inativo = Inativo,
      praca = Praça,
      cartao.credito = `Cartão de Crédito`
    ) %>%
    dplyr::filter(!is.na(agente.financeiro) | !is.na(conta.corrente)) %>%
    dplyr::mutate(
      # Padronizar dados
      limite.credito = as.numeric(limite.credito),
      inativo = case_when(
        toupper(as.character(inativo)) == "S" ~ TRUE,
        toupper(as.character(inativo)) == "N" ~ FALSE,
        TRUE ~ NA
      ),
      praca = as.logical(praca),
      cartao.credito = as.logical(cartao.credito),
      # Adicionar colunas para saldos (não disponíveis neste tipo de arquivo)
      saldo.conciliado = NA_real_,
      saldo.razao = NA_real_,
      saldo.aplicado = NA_real_,
      pendentes = NA_real_,
      pendentes.futuros = NA_real_,
      disponibilidade = NA_real_,
      # Metadados do arquivo
      arquivo = arquivo_usado,
      arquivo.tabela.tipo = "cnts",
      arquivo.tipo = "cnts",
      arquivo.fonte = "ik"
    )

  # Remover linha de totais se existir
  contas_final <- contas_final %>%
    dplyr::filter(!(is.na(agente.financeiro) & is.na(conta.corrente) &
      !is.na(limite.credito)))

  # Padronizar colunas para ter a mesma estrutura independente do tipo de arquivo
  colunas_padrao <- c(
    "tipo.agente", "codigo.agente", "agente.financeiro", "conta.corrente",
    "tipo.conta", "numero.agencia", "limite.credito", "empresa", "filial",
    "nucleo", "inativo", "praca", "cartao.credito", "saldo.conciliado",
    "saldo.razao", "saldo.aplicado", "pendentes", "pendentes.futuros",
    "disponibilidade", "arquivo", "arquivo.tabela.tipo", "arquivo.tipo",
    "arquivo.fonte"
  )

  # Adicionar colunas que podem estar faltando com NA
  for (col in colunas_padrao) {
    if (!col %in% names(contas_final)) {
      contas_final[[col]] <- NA
    }
  }

  # Selecionar apenas as colunas padrão na ordem correta
  contas_final <- contas_final %>%
    dplyr::select(dplyr::all_of(colunas_padrao))

  # Informar sobre os resultados
  message("Total de contas extraídas: ", nrow(contas_final))
  message("Arquivo fonte: ", basename(arquivo_usado))
  message("Mapeamento de colunas:")
  message("  tipo.agente <- Tipo de Agente")
  message("  codigo.agente <- Cód. Agente Financeiro")
  message("  agente.financeiro <- Agente Financeiro")
  message("  conta.corrente <- Conta")
  message("  tipo.conta <- Tipo de Conta")
  message("  numero.agencia <- Nº Agência")
  message("  limite.credito <- Limite de Crédito")
  message("  empresa <- Empresa")
  message("  filial <- Filial")
  message("  nucleo <- Núcleo")
  message("  inativo <- Inativo")
  message("  praca <- Praça")
  message("  cartao.credito <- Cartão de Crédito")

  return(contas_final)
}

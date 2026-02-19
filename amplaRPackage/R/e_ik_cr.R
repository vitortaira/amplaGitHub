#' @title Extração de Contas a Receber Informakon
#'
#' @description
#' A função e_ik_cr() extrai os dados de contas recebidas dos arquivos na pasta
#' "fechamento_in", preenche-os em uma planilha xlsx (opcional) e os retorna em
#' um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "fechamento_in".
#'   Valor padrão: \code{caminhos_pastas("fechamento_in")}.
#' @param xlsx Logical. Se \code{TRUE}, cria um arquivo xlsx com os dados extraídos.
#'   Valor padrão: \code{FALSE}.
#'
#' @return Data frame com dados das contas recebidas consolidadas.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' cr_df <- e_ik_cr()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub str_detect
#' @importFrom lubridate floor_date
#' @export
e_ik_cr <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("fechamento_in"),
    xlsx = FALSE) {
  # Função interna para buscar o arquivo de contas recebidas mais recente
  obter_caminho_cr <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'fechamento_in' não foi encontrada.")
    }
    # Busca arquivos que começam com "cr-"
    caminhos_cr <- dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_cr <- caminhos_cr[
      basename(caminhos_cr) %>% str_detect("^cr-")
    ]
    if (length(caminhos_cr) == 0) {
      stop("Nenhum arquivo de contas recebidas encontrado na pasta fechamento_in.")
    }
    # Determina a data final (YYYY_MM_DD) mais recente
    # Padrão: cr-YYYY_MM_DD-YYYY_MM_DD.xlsx
    data_final_por_arquivo <- sapply(caminhos_cr, function(path) {
      basename(path) %>%
        str_extract("\\d{4}_\\d{2}_\\d{2}(?=\\.xlsx$)") %>%
        as.Date(format = "%Y_%m_%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_cr[indice_recente]
  }

  # Carrega o arquivo de contas recebidas mais recente
  caminho_arquivo_cr <- obter_caminho_cr()
  cr_df <- read_excel(caminho_arquivo_cr, skip = 3) %>%
    # Padroniza nomes
    rename(
      empreendimento = Empreendimento,
      cliente = Cliente,
      contrato = Contrato,
      torre = Torre,
      apto = Apto,
      esp = Esp,
      parcela = Parcela,
      elemento = Elemento,
      vencimento = Vencimento,
      data.pagamento = `Data Pagto`,
      `r/f` = `R/F`,
      agente = Agente,
      principal = Principal,
      juros = Juros,
      reajuste = Reajuste,
      encargos = Encargos,
      juros.mora = `Juros de Mora`,
      multa = Multa,
      seguro = Seguro,
      desconto = Desconto,
      cart = `Cart.`,
      total = Total
    ) %>%
    mutate(
      agente = as.character(agente),
      apto = as.integer(apto),
      cart = as.factor(cart),
      cliente = as.character(cliente),
      contrato = as.character(contrato),
      data.pagamento = as.Date(data.pagamento, format = "%d/%m/%Y"),
      desconto = as.numeric(desconto),
      elemento = as.character(elemento),
      empreendimento = as.character(empreendimento),
      empresa = empreendimento %>% str_sub(1, 3),
      encargos = as.numeric(encargos),
      esp = as.character(esp),
      juros = as.numeric(juros),
      juros.mora = as.numeric(juros.mora),
      mes = floor_date(data.pagamento, "month"),
      multa = as.numeric(multa),
      parcela = as.character(parcela),
      principal = as.numeric(principal),
      `r/f` = as.factor(`r/f`),
      reajuste = as.numeric(reajuste),
      seguro = as.numeric(seguro),
      torre = as.character(torre),
      total = as.numeric(total),
      vencimento = as.Date(vencimento),
      arquivo = caminho_arquivo_cr,
      arquivo.tipo = "cr",
      arquivo.tabela.tipo = "cr",
      arquivo.fonte = "ik"
    ) %>%
    select(
      empreendimento, empresa, cliente, contrato, torre, apto, esp,
      parcela, elemento, vencimento, data.pagamento, mes, `r/f`, agente,
      principal, juros, reajuste, encargos, juros.mora, multa,
      seguro, desconto, cart, total, arquivo, arquivo.tipo,
      arquivo.tabela.tipo, arquivo.fonte
    )

  # Se solicitado, salva em xlsx
  if (xlsx) {
    # Ajuste este caminho e nome de arquivo conforme necessário
    writexl::write_xlsx(cr_df, paste0(f_caminho.pasta.ik_c, "/cr_consolidadas.xlsx"))
  }

  return(cr_df)
}

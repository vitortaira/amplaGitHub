# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik_rec.R
#' @title Extração de Receitas Informakon
#'
#' @description
#' A função e_ik_rec() extrai os dados de receitas dos arquivos na pasta
#' "informakon", preenche-os em uma planilha xlsx (opcional) e os retorna em
#' um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#' @param xlsx Logical. Se \code{TRUE}, cria um arquivo xlsx com os dados extraídos.
#'   Valor padrão: \code{FALSE}.
#'
#' @return Data frame com dados das receitas consolidadas.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' receitas_df <- e_ik_rec()
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub str_detect
#' @importFrom lubridate floor_date
#' @export
e_ik_rec <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("informakon"),
    xlsx = FALSE) {
  # Função interna para buscar o arquivo de receitas mais recente
  obter_caminho_receitas <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }
    # Busca arquivos que começam com "receitas_" e ignora os "Informakon"
    caminhos_rec <- dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_rec <- caminhos_rec[
      basename(caminhos_rec) %>% str_detect("^receitas") &
        !basename(caminhos_rec) %>% str_detect("^Informakon")
    ]
    if (length(caminhos_rec) == 0) {
      stop("Nenhum arquivo de receitas encontrado na pasta informakon.")
    }
    # Determina a data final (YYYYMMDD) mais recente
    data_final_por_arquivo <- sapply(caminhos_rec, function(path) {
      basename(path) %>%
        str_extract("[^_]+$") %>%
        str_remove("\\.xlsx$") %>%
        as.Date(format = "%Y%m%d")
    })
    indice_recente <- which.max(data_final_por_arquivo)
    caminhos_rec[indice_recente]
  }

  # Carrega o arquivo de receitas mais recente
  caminho_arquivo_receitas <- obter_caminho_receitas()
  receitas_df <- read_excel(caminho_arquivo_receitas, skip = 3) %>%
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
      arquivo = caminho_arquivo_receitas,
      arquivo.tipo = "rec",
      arquivo.tabela.tipo = "rec",
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
    writexl::write_xlsx(receitas_df, paste0(f_caminho.pasta.ik_c, "/receitas_consolidadas.xlsx"))
  }

  return(receitas_df)
}

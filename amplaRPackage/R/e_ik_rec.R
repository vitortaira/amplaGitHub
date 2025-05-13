# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaRPackage\R\e_ik_rec.R
#' @title Extração de Receitas Informakon
#'
#' @description
#' A função e_ik_rec() extrai os dados de receitas dos arquivos na pasta
#' "informakon", preenche-os em uma planilha xlsx (opcional) e os retorna em
#' um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{c_caminhos_pastas("informakon")}.
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
    f_caminho.pasta.ik_c = c_caminhos_pastas("informakon"),
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
    mutate(
      Agente = as.character(Agente),
      Apto = as.integer(Apto),
      `Cart.` = as.factor(`Cart.`),
      Cliente = as.character(Cliente),
      Contrato = as.character(Contrato),
      `Data Pagto` = as.Date(`Data Pagto`, format = "%d/%m/%Y"),
      Desconto = as.numeric(Desconto),
      Elemento = as.character(Elemento),
      Empreendimento = as.character(Empreendimento),
      Empresa = Empreendimento %>% str_sub(1, 3),
      Encargos = as.numeric(Encargos),
      Esp = as.character(Esp),
      Juros = as.numeric(Juros),
      `Juros de Mora` = as.numeric(`Juros de Mora`),
      `Mês` = floor_date(`Data Pagto`, "month"),
      Multa = as.numeric(Multa),
      Parcela = as.character(Parcela),
      Principal = as.numeric(Principal),
      `R/F` = as.factor(`R/F`),
      Reajuste = as.numeric(Reajuste),
      Seguro = as.numeric(Seguro),
      Torre = as.character(Torre),
      Total = as.numeric(Total),
      Vencimento = as.Date(Vencimento),
      Arquivo = caminho_arquivo_receitas,
      Arquivo_tipo_tabela = "rec",
      Arquivo_tipo = "rec",
      Arquivo_fonte = "ik"
    ) %>%
    select(
      Empreendimento, Empresa, Cliente, Contrato, Torre, Apto, Esp,
      Parcela, Elemento, Vencimento, `Data Pagto`, `Mês`, `R/F`, Agente,
      Principal, Juros, Reajuste, Encargos, `Juros de Mora`, Multa,
      Seguro, Desconto, `Cart.`, Total, Arquivo, Arquivo_tipo_tabela,
      Arquivo_tipo, Arquivo_fonte
    )

  # Se solicitado, salva em xlsx
  if (xlsx) {
    # Ajuste este caminho e nome de arquivo conforme necessário
    writexl::write_xlsx(receitas_df, paste0(f_caminho.pasta.ik_c, "/receitas_consolidadas.xlsx"))
  }

  return(receitas_df)
}

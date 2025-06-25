# Este script demonstra como importar dados de um arquivo .ofx para o R.
# Ele utiliza o pacote `reticulate` para chamar a biblioteca Python `ofxparse`,
# proporcionando uma abordagem robusta para a leitura de arquivos OFX.

# Passo 1: Instalar o pacote reticulate, se você ainda não o tiver.
# Descomente a linha abaixo e execute-a no console se o pacote não estiver instalado.
# install.packages("reticulate")

# Passo 2: Carregar os pacotes necessários.
# Usaremos `reticulate` para a interface com Python, `tibble` para uma melhor visualização dos dados,
# e `dplyr` para manipulação de dados.

#' Importa dados de transações de um arquivo OFX de forma robusta
#'
#' @description
#' Esta função utiliza a biblioteca Python `ofxparse` através do pacote `reticulate`
#' para ler e analisar arquivos OFX. Esta abordagem é mais robusta do que depender
#' de pacotes R que podem não estar atualizados ou disponíveis para a sua versão do R.
#'
#' A função lida com a dependência do Python automaticamente. Na primeira vez que
#' for executada, ela verificará se o `ofxparse` está instalado no ambiente
#' reticulate e, se não estiver, irá instalá-lo. Isso pode levar um momento.
#' As execuções subsequentes serão mais rápidas.
#'
#' @param caminho_arquivo_ofx O caminho completo para o arquivo .ofx que você deseja importar.
#' @return Um `tibble` contendo os dados das transações, com colunas limpas e formatadas.
#' As colunas incluem: `data`, `descricao`, `valor`, `tipo`, `id_transacao` e `memo`.
#' @export
#'
#' @examples
#' \dontrun{
#' caminho <- "caminho/para/seu/extrato.ofx"
#' transacoes_tbl <- importar_dados_ofx(caminho)
#' print(transacoes_tbl)
#' }
#' @import reticulate
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% mutate rename select all_of
#' @importFrom lubridate as_datetime
importar_dados_ofx <- function(caminho_arquivo_ofx) {
  # --- Validação e Gestão de Dependências ---

  # 1. Verificar se o arquivo existe
  if (!file.exists(caminho_arquivo_ofx)) {
    stop("Arquivo não encontrado. Verifique o caminho fornecido: ", caminho_arquivo_ofx)
  }

  # 2. Informar qual Python está sendo usado para fins de diagnóstico
  message("Usando o Python de: ", reticulate::py_config()$python)

  # 3. Verificar e, se necessário, instalar a dependência Python `ofxparse`
  if (!reticulate::py_module_available("ofxparse")) {
    message("A biblioteca Python 'ofxparse' não foi encontrada.")
    message("Instalando agora... (Isso só acontecerá uma vez)")
    tryCatch(
      {
        reticulate::py_install("ofxparse", pip = TRUE)
        message("'ofxparse' instalado com sucesso.")
      },
      error = function(e) {
        stop("Falha ao instalar 'ofxparse'. Verifique sua configuração de Python e reticulate.")
      }
    )
  }

  # --- Lógica de Parsing (Python via Reticulate) ---

  # 4. Passar o caminho do arquivo R para uma variável no ambiente Python
  #    Isso é mais robusto do que formatar a string do script com o caminho.
  reticulate::py$caminho_py <- caminho_arquivo_ofx

  # 5. Definir e executar o script Python para analisar o arquivo OFX
  #    O resultado será um DataFrame do Pandas, que o reticulate converterá para um data.frame do R.
  transacoes_df <- reticulate::py_run_string("
import pandas as pd
from ofxparse import OfxParser

# A variável 'caminho_py' foi definida no ambiente R
with open(caminho_py, 'rb') as f:
    ofx = OfxParser.parse(f)

account = ofx.account
statement = account.statement
transactions = statement.transactions

data = []
for t in transactions:
    data.append({
        'date': t.date,
        'payee': t.payee,
        'type': t.type,
        'amount': t.amount,
        'id': t.id,
        'memo': t.memo
    })

result = pd.DataFrame(data)
")$result

  # --- Limpeza e Formatação dos Dados (R) ---

  # 6. Converter para tibble e realizar a limpeza final
  if (is.null(transacoes_df) || nrow(transacoes_df) == 0) {
    warning("Nenhuma transação encontrada no arquivo OFX.")
    return(tibble::tibble())
  }

  transacoes_tbl <- tibble::as_tibble(transacoes_df) %>%
    dplyr::mutate(
      # Converter a data para o formato de data e hora do R
      date = lubridate::as_datetime(date),
      # Garantir que o valor seja numérico
      amount = as.numeric(amount)
    ) %>%
    # Renomear colunas para nomes mais amigáveis em português
    dplyr::rename(
      data = .data$date,
      descricao = .data$payee,
      valor = .data$amount,
      tipo = .data$type,
      id_transacao = .data$id,
      memo = .data$memo
    ) %>%
    # Selecionar e reordenar as colunas de interesse
    dplyr::select(dplyr::all_of(c("data", "descricao", "valor", "tipo", "id_transacao", "memo")))

  message("Arquivo OFX importado com sucesso. Total de ", nrow(transacoes_tbl), " transações.")
  return(transacoes_tbl)
}

# --- Exemplo de Uso ---
# O código abaixo demonstra como usar a função.
# Para executar, primeiro carregue o pacote com `devtools::load_all()` no console,
# e depois execute estas linhas interativamente.

# # Defina o caminho para o seu arquivo .ofx
# caminho_do_arquivo_ofx <- "c:\\Users\\Ampla\\AMPLA INCORPORADORA LTDA\\Financeiro - Documentos\\UP VILA SONIA\\1. CONTABIL\\1. 2025\\01.2025\\EXTRATO 2399 - JANEIRO.ofx"
#
# # Chame a função para importar os dados
# # A primeira execução pode demorar um pouco para instalar a dependência
# extrato_janeiro_tbl <- importar_dados_ofx(caminho_do_arquivo_ofx)
#
# # Exiba as primeiras linhas do tibble importado
# print(head(extrato_janeiro_tbl))
#
# # Agora você pode usar o tibble `extrato_janeiro_tbl` para suas análises.
# # Por exemplo, para ver um resumo dos tipos de transação:
# # print(extrato_janeiro_tbl %>% count(tipo))

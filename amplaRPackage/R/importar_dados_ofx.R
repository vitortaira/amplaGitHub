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

  # 4. Obter o caminho para o script Python dentro do pacote
  #    Isso garante que o script seja encontrado, não importa como o pacote seja carregado.
  caminho_script_py <- system.file("python", "parse_ofx.py", package = "amplaRPackage")

  if (caminho_script_py == "") {
      stop("Não foi possível encontrar o script 'parse_ofx.py'. O pacote foi instalado corretamente?")
  }


  # 5. Carregar o script Python
  reticulate::source_python(caminho_script_py)

  # 6. Chamar a função Python para analisar o arquivo OFX
  #    A função `parse_ofx_file` está agora disponível no ambiente R.
  transacoes_df <- parse_ofx_file(caminho_arquivo_ofx)


  # --- Limpeza e Formatação dos Dados (R) ---

  # 7. Converter o data.frame resultante para um tibble e limpar os nomes
  transacoes_tbl <- transacoes_df %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      data = date,
      descricao = payee,
      valor = amount,
      id_transacao = id
    ) %>%
    dplyr::mutate(
      data = lubridate::as_datetime(data),
      valor = as.numeric(valor)
    ) %>%
    dplyr::select(
      dplyr::all_of(c("data", "descricao", "valor", "tipo", "id_transacao", "memo"))
    )

  return(transacoes_tbl)
}

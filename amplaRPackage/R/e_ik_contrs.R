e_ik_contrs <- function(caminhos.pasta.cobranca_c = caminhos_pastas("cobranca")) {
  # Certifique-se de que o caminho é um diretório válido
  if (!dir.exists(caminhos.pasta.cobranca_c)) {
    stop("O caminho fornecido não é um diretório: ", caminhos.pasta.cobranca_c)
  }

  caminhos.contrs_c <- e_metadados("contr")

  # Mensagem para depuração: quantidade de arquivos de contratos encontrados
  message(length(caminhos.contrs_c), " arquivos do tipo contr")

  contrs_t <-
    caminhos.contrs_c %>%
    map_dfr(~ {
      tryCatch(
        {
          result <- e_ik_contr(.x)
          result
        },
        error = function(e) {
          warning("Erro ao processar o arquivo ", .x, ": ", e$message)
          return(NULL) # Retorna NULL para arquivos com erro
        }
      )
    }) %>%
    mutate(
      arquivo.tabela.tipo = "contr",
      arquivo.tipo = "contr",
      arquivo.fonte = "cef"
    )

  return(contrs_t)
}

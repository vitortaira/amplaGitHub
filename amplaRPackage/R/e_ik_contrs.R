e_ik_contrs <- function(caminhos.pasta.cobranca_c = c_caminhos_pastas("cobranca")) {
  # Make sure the path is a directory path
  if (!dir.exists(caminhos.pasta.cobranca_c)) {
    stop("The path provided is not a directory: ", caminhos.pasta.cobranca_c)
  }

  caminhos.contrs_c <-
    dir_ls(caminhos.pasta.cobranca_c, recurse = TRUE, type = "file") %>%
    keep(~ str_detect(.x, "(?i)contratos-.*\\.xlsx"))

  # Debug the paths to make sure they're valid files
  message("Found ", length(caminhos.contrs_c), " contract files")

  contrs_t <-
    caminhos.contrs_c %>%
    map_dfr(~ {
      tryCatch(
        {
          result <- e_ik_contr(.x)
          message("Successfully processed: ", .x)
          result
        },
        error = function(e) {
          warning("Error processing file ", .x, ": ", e$message)
          return(NULL) # Return NULL for failed files
        }
      )
    }) %>%
    mutate(
      Arquivo_tipo_tabela = "contr",
      Arquivo_tipo = "contr",
      Arquivo_fonte = "cef"
    )

  return(contrs_t)
}

c_extratos <- function() {
  bind_rows(
    e_metadados("xcef") %>%
      mutate(
        arquivo.subtipo = c_cef_xcef(caminho)
      ),
    e_metadados("xita") %>%
      mutate(
        arquivo.subtipo = "xita1"
      )
  ) %>%
    mutate(
      arquivo.extensao = fs::path_ext(caminho),
      banco = fs::path_file(caminho) %>% str_extract("(?<=-).*?(?=_)"),
      conta = fs::path_file(caminho) %>% str_extract("(?<=_)[0-9]+(?=-)"),
      id.corrente = stringr::str_c(
        empresa,
        "-",
        banco,
        "_",
        stringr::str_remove_all(conta, "-") %>%
          stringr::str_sub(-4, -1)
      ),
      id = {
        # Safe ID assignment with proper error handling
        if (is.null(contasBancarias) || nrow(contasBancarias) == 0) {
          message(
            "Mapeamento de contas bancárias não está disponível. ",
            "Usando id.corrente como fallback."
          )
          id.corrente
        } else if (!all(c("id.antigo", "id.atual", "id.continuo") %in%
          names(contasBancarias))) {
          message(
            "Mapeamento de contas bancárias não possui as colunas ",
            "necessárias. Usando id.corrente como fallback."
          )
          id.corrente
        } else {
          message(sprintf(
            "Mapeamento de contas bancárias carregado com %d registros.",
            nrow(contasBancarias)
          ))

          # Safe case_when with id.corrente as fallback
          dplyr::case_when(
            id.corrente %in% contasBancarias$id.antigo
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.antigo)
              ],
            id.corrente %in% contasBancarias$id.atual
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.atual)
              ],
            TRUE ~ id.corrente
          )
        }
      }
    )
}

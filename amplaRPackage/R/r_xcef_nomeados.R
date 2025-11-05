r_xcef_nomeados <- function(mes, ano) {
  xcefs <- e_cef_xcefs()
  eprs <- e_cef_eprs() %>%
    mutate(contrato = str_sub(contrato, -5, -1)) %>%
    rename(contrato.5 = contrato) %>%
    dplyr::select(empresa, contrato.5, nome.mutuario)
  # Retorna
  xcefn <- left_join(xcefs, eprs, by = c("empresa", "contrato.5")) %>%
    dplyr::filter(
      month(data.movimentacao) == mes, year(data.movimentacao) == ano
    ) %>%
    select(
      empresa, data.lancamento, data.movimentacao, documento,
      nome.mutuario, descricao, valor, saldo, conta.interno, conta, agencia,
      produto, cnpj, cpf.cnpj, nome.razao, periodo.inicio, periodo.fim,
      data.consulta, arquivo
    )
  gerar_xlsx(
    data = list(
      "Extratos nomeados" = xcefn,
      EPRs = e_cef_eprs() %>%
      select(-arquivo.tabela.tipo, -arquivo.tipo, -arquivo.fonte)
    ),
    tab_colours = c(
      `Extratos nomeados` = "darkblue",
      EPRs = "blue"
    ),
    col_dates = c(
      "data.lancamento", "data.movimentacao", "periodo.inicio", "periodo.fim"
    ),
    col_headers =
    col_monetary = c("valor", "saldo"),
    col_width_auto = c("descricao", "nome.mutuario"),
    col_width_spec = c("nome.mutuario" = 30),
    save = list(
      nome_arquivo = sprintf(
        "Extratos_CEF_nomeados-%s.xlsx",
        format(Sys.time(), "%Y%m%d_%H%M%S")
      ),
      caminho_destino = normalizePath(
        file.path(Sys.getenv("USERPROFILE"), "Downloads"),
        winslash = "\\", mustWork = FALSE
      )
    )
  )
  return(xcefn)
}

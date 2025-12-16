r_inad <- function() {
  # inad -----------------------------------------------------------------------

  # Consolida os dados dos arquivos do tipo inad
  inads_t <- e_ik_inads(xlsx = FALSE)
  # Tabela com todos os caminhos dos arquivos do tipo inad
  caminhos.inads_t <- e_metadados("inad")
  # Tabela com os caminhos dos arquivos mais recentes do tipo inad
  caminhos.inads.recentes_t <- caminhos.inads_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # contr ----------------------------------------------------------------------

  # Consolida os dados dos arquivos do tipo contr
  contrs_t <- e_ik_contrs_inad()
  # Tabela com todos os caminhos dos arquivos do tipo contr
  caminhos.contrs_t <- e_metadados("contr")
  # Tabela com os caminhos dos arquivos mais recentes do tipo contr
  caminhos.contrs.recentes_t <- caminhos.contrs_t %>%
    arrange(desc(data)) %>%
    distinct(empresa, .keep_all = TRUE)

  # join -----------------------------------------------------------------------

  # Aba "Parcelas"
  r_inad.parcelas_t <-
    inads_t %>%
    dplyr::filter(arquivo %in% caminhos.inads.recentes_t$caminho) %>%
    left_join(
      contrs_t %>%
        dplyr::filter(arquivo %in% caminhos.contrs.recentes_t$caminho) %>%
        select(-"cliente"),
      by = c("contrato.ampla", "empreendimento"),
      suffix = c(".inad", ".contr")
    ) %>%
    mutate(
      cliente = stringr::str_to_title(cliente),
      repassado = if_else(repassado %in% c(NA, "Não"), "Não", "Sim")
    ) %>%
    dplyr::select(
      empreendimento, cliente, total, repassado,
      contrato.ampla, contrato.cef, unidade, quantidade.parcelas,
      parcela, atraso, vencimento, ele,
      principal, juros, encargos, juros.mora, multa, seguro,
      everything() # now grab all the other cols (incl. arquivo.*)
    ) %>%
    relocate(starts_with("arquivo"), .after = last_col()) %>%
    distinct() %>%
    arrange(desc(total))
  # Aba "Clientes"
  r_inad.clientes_t <-
    r_inad.parcelas_t %>%
    group_by(cliente) %>%
    summarise(
      total.cliente = sum(total, na.rm = TRUE),
      quantidade.parcelas = first(quantidade.parcelas),
      atraso.medio.ponderado = round(
        sum((atraso / 30) * total, na.rm = TRUE) / sum(total, na.rm = TRUE),
        0
      ),
      atraso.maximo = round(max(atraso, na.rm = TRUE) / 30, 0),
      empreendimento = first(empreendimento),
      repassado = first(repassado)
    ) %>%
    ungroup() %>%
    mutate(
      status = NA_character_,
      anotacoes = NA_character_
    ) %>%
    select(
      empreendimento, cliente, total.cliente, quantidade.parcelas,
      atraso.medio.ponderado, atraso.maximo, repassado, status, anotacoes
    ) %>%
    arrange(desc(total.cliente))

  # Lista nomeada com os dataframes e os nomes das abas correspondentes
  dfs_l <- list(
    Parcelas = r_inad.parcelas_t,
    Clientes = r_inad.clientes_t
  )

  # xlsx -----------------------------------------------------------------------

  # Definindo o nome do arquivo dinamicamente
  nome.xlsx_c <-
    str_c(
      "Inadimplencia-",
      format(Sys.time(), "%Y_%m_%d-%H_%M_%S"),
      ".xlsx"
    )

  # Caminho do template
  caminho.template_c <- str_c(
    caminhos_pastas("templates"), "/Template-Inadimplencia.xlsx"
  )

  # Caminho de destino
  caminho.destino_c <- str_c(caminhos_pastas("cobranca"), "/Consolidados")

  # Larguras específicas das colunas
  colunas_larguras <- c(
    "alterado.por" = 18,
    "anotacoes" = 60,
    "arquivo.tipo.contr" = 18,
    "arquivo.tabela.tipo.contr" = 24,
    "arquivo.fonte.contr" = 20,
    "arquivo.tipo.inad" = 18,
    "arquivo.tabela.tipo.inad" = 24,
    "arquivo.fonte.inad" = 20,
    "atraso" = 6,
    "atraso.medio.ponderado" = 25,
    "autorizado" = 10,
    "cliente" = 35,
    "contrato.alternativo" = 20,
    "contrato.ampla" = 15,
    "contrato.cef" = 15,
    "cotista" = 9,
    "cpf.cnpj" = 15,
    "criado.por" = 18,
    "data.contrato" = 12,
    "ele" = 6,
    "empreendimento" = 16,
    "esp.contr" = 9,
    "esp.inad" = 9,
    "id.cartao" = 9,
    "identificacao.imovel" = 20,
    "atraso.maximo" = 35,
    "moeda" = 9,
    "parcela" = 10,
    "quantidade.parcelas" = 20,
    "r/f" = 6,
    "repassado" = 12,
    "sit" = 6,
    "status" = 30,
    "tipo.contrato" = 15,
    "unidade" = 55,
    "usuario.autorizacao" = 18,
    "vencimento" = 12
  )

  # Gerar a planilha usando gerar_xlsx
  gerar_xlsx(
    data = dfs_l,
    wb_load = caminho.template_c,
    col_width_def = 18,
    col_width_spec = colunas_larguras,
    col_monetary = c(
      "principal", "juros", "encargos", "juros.mora", "multa", "seguro",
      "total", "total.cliente"
    ),
    col_dates = c("vencimento", "data.contrato"),
    table = TRUE,
    save = list(nome.xlsx_c, caminho.destino_c)
  )

  # Mensagens de verificação ---------------------------------------------------

  if (nrow(caminhos.inads.recentes_t) > 0) {
    meses <- format(caminhos.inads.recentes_t$data, "%Y-%m")
    if (length(unique(meses)) == 1) {
      message(
        "\u2705 Os relatórios mais recentes de inadimplência de todos ",
        "os empreendimentos são do mês ", unique(meses)
      )
    } else {
      msg <- paste0(
        "\u274C Os relatórios mais recentes de inadimplência são de ",
        "meses diferentes entre os empreendimentos:\n",
        capture.output(print(
          caminhos.inads.recentes_t[, c("caminho", "data")],
          row.names = FALSE
        )) %>%
          paste(collapse = "\n")
      )
      message(msg)
    }
  }
  # Mensagem de verificação para contratos
  if (nrow(caminhos.contrs.recentes_t) > 0) {
    meses_contrs <- format(caminhos.contrs.recentes_t$data, "%Y-%m")
    if (length(unique(meses_contrs)) == 1) {
      message(
        "\u2705 Os contratos mais recentes de todos os empreendimentos ",
        "são do mês ", unique(meses_contrs)
      )
    } else {
      msg_contrs <- paste0(
        "\u274C Os contratos mais recentes são de meses diferentes ",
        "entre os empreendimentos:\n",
        capture.output(print(
          caminhos.contrs.recentes_t[, c("caminho", "data")],
          row.names = FALSE
        )) %>%
          paste(collapse = "\n")
      )
      message(msg_contrs)
    }
  }
  return(dfs_l)
}

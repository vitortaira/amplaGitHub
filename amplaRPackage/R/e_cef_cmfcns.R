#' @title Consolidação dos dados dos relatórios CMF_CN
#'
#' @description
#' A função **e_cef_cmfcns** consolida os dados dos relatórios CMF_CN
#' que estão na pasta "Relatorios - CIWEB".
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#'
#' @details
#' A função percorre a pasta especificada buscando arquivos PDF que
#' contenham "MOV_FINANC_CN" no nome. Para cada arquivo encontrado,
#' chama a função \code{e_cef_cmfcn} para realizar a extração dos dados e,
#' posteriormente, consolida os resultados em um tibble.
#'
#' @return
#' Retorna um tibble consolidado com os dados extraídos dos arquivos CMF_CN.
#'
#' @examples
#' \dontrun{
#' f_caminho.pasta.ciweb_c <- "caminho/para/a/pasta/Relatorios - CIWEB"
#' resultado <- e_cef_cmfcns(f_caminho.pasta.ciweb_c)
#' print(resultado)
#' }
#'
#' @importFrom purrr keep
#' @importFrom dplyr mutate rename
#' @importFrom stringr str_detect str_sub
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#'
#' @export

e_cef_cmfcns <- function(f_caminho.pasta.ciweb_c = caminhos_pastas("ciweb")) {
  # Extrai o número dos contratos dos empreendimentos com a CEF
  contratos.empreendimentos.cef <- dplyr::select(
    e_cef_ecns()$ecn_e, empreendimento, contrato
  ) %>%
    distinct() %>%
    mutate(
      contrato = str_sub(contrato, 1, 12),
      empresa = case_when(
        str_detect(empreendimento, "(?i)up\\s?jardim\\s?prud") ~ "AMP",
        str_detect(empreendimento, "(?i)up\\s?vila\\s?sonia") ~ "AVS",
        str_detect(empreendimento, "(?i)up\\s?select\\s?vila") ~ "GRA",
        str_detect(empreendimento, "(?i)up\\s?esta[cç][aã]o\\s?s[aã]o\\s?lucas") ~ "LUC",
        str_detect(empreendimento, "(?i)up\\s?esta[cç][aã]o\\s?vila") ~ "SN2",
        str_detect(empreendimento, "(?i)move\\s?vila\\s?s[oô]nia") ~ "SN4",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-empreendimento)

  # Criar lookup de contrato -> empresa
  contratos.lookup <- setNames(
    contratos.empreendimentos.cef$empresa,
    contratos.empreendimentos.cef$contrato
  )
  # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
  caminhos.cmfcn_c <-
    dir_ls(f_caminho.pasta.ciweb_c, recurse = TRUE, type = "file") %>%
    keep(~ str_detect(.x, "(?i)mov_financ_cn.pdf"))
  # Identifica o arquivo mais recente de cada empreendimento
  contratos.empreendimentos.12.primeiros_c <-
    caminhos.cmfcn_c %>%
    str_extract("\\d{12}") %>%
    unique()
  caminhos.cmfcn.recentes_c <-
    caminhos.cmfcn_c %>%
    tibble(caminho = .) %>%
    mutate(
      empreendimento = str_extract(caminho, "\\d{12}"),
      data.arquivo = str_extract(path_file(caminho), "^\\d{8}") %>% ymd()
    ) %>%
    group_by(empreendimento) %>%
    slice_max(data.arquivo, n = 1) %>%
    pull(caminho)
  cmfcns_l <- list()
  cmfcns_t <- data.frame()
  for (i_caminho.cmfcn_c in caminhos.cmfcn.recentes_c) {
    cmfcn <- tryCatch(
      e_cef_cmfcn(i_caminho.cmfcn_c),
      error = function(e) {
        message(sprintf(
          "Falha ao extrair CMF_CN: %s | erro: %s",
          basename(i_caminho.cmfcn_c), conditionMessage(e)
        ))
        NULL
      }
    )
    if (!is.null(cmfcn) && nrow(cmfcn) > 0) {
      cmfcns_l[[i_caminho.cmfcn_c]] <- cmfcn
      cmfcns_t <- bind_rows(cmfcns_t, cmfcn)
    }
  }
  cmfcns_t %<>%
    mutate(
      contrato.6 = contrato %>% str_sub(-6, -1),
      arquivo.tabela.tipo = "cmfcn",
      arquivo.tipo = "cmfcn",
      arquivo.fonte = "cef"
    ) %>%
    rename(
      data.movimento = data.remessa,
    ) %>%
    mutate(
      natureza = case_when(
        str_detect(lancamentos, "(?i)amort\\.|amortizacao") &
          !str_detect(lancamentos, "(?i)cre\\sbloqueado")
        ~ "amortizacao.pj",
        str_detect(lancamentos, "(?i)cre\\sbloqueado") &
          !str_detect(lancamentos, "(?i)terr")
        ~ "bloqueio - repasse.cef.obra",
        str_detect(lancamentos, "(?i)cre\\sbloqueado.*terr")
        ~ "bloqueio - repasse.cef.terreno",
        str_detect(lancamentos, "(?i)fim\\sde\\sobra") ~ "fim.obra",
        str_detect(lancamentos, "(?i)rem\\sterr") &
          !str_detect(lancamentos, "(?i)cre\\sbloqueado")
        ~ "remuneracao.terreno",
        str_detect(lancamentos, "(?i)rem\\svend") &
          !str_detect(lancamentos, "(?i)cre\\sbloqueado")
        ~ "remuneracao.venda",
        str_detect(lancamentos, "(?i)financ|fgts|subs/desc") &
          !str_detect(lancamentos, "(?i)cre\\sbloqueado|financ\\.pj")
        ~ "repasse.cef.obra",
        str_detect(lancamentos, "(?i)terreno") &
          !str_detect(lancamentos, "(?i)cre\\sbloqueado")
        ~ "repasse.cef.terreno",
        TRUE ~ NA_character_
      )
    ) %>%
    as_tibble() %>%
    # Desbloqueios: herdar natureza da transação bloqueada e remover bloqueadas
    arrange(data.movimento) %>%
    group_by(contrato, valor) %>%
    mutate(
      desbloqueio = str_detect(lancamentos, "(?i)des.*fin\\snao\\sprd|desb\\sfinanc/ter"),
      tem.desbloqueio = any(desbloqueio),
      bloqueio = !desbloqueio & tem.desbloqueio & row_number() < which(desbloqueio)[1],
      natureza = if_else(
        desbloqueio,
        str_replace(last(natureza[!desbloqueio]), "^bloqueio - ", ""),
        natureza
      ),
      empresa = contratos.lookup[str_extract(arquivo, "\\d{12}")]
    ) %>%
    ungroup() %>%
    dplyr::filter(!bloqueio) %>%
    select(-desbloqueio, -tem.desbloqueio, -bloqueio) %>%
    select(
      empresa, contrato, data.lancamento, data.movimento, lancamentos, np,
      `conta.sidec/nsgd`, valor, situacao, mot, contrato.6, natureza, arquivo,
      arquivo.tipo, arquivo.tabela.tipo, arquivo.fonte
    )
  return(cmfcns_t)
}

#' @title Agrupamento de CMF_CN por contrato e mês
#'
#' @description
#' Agrupa os dados de e_cef_cmfcns() por contrato, pivotando valores mensais.
#'
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#' @param agrupamento Tipo de agrupamento: "natureza" ou "lancamentos".
#'
#' @return
#' Tibble com uma linha por contrato e colunas mensais de valores.
#'
#' @export
e_cef_cmfcns_mensal <- function(
    f_caminho.pasta.ciweb_c = caminhos_pastas("ciweb"),
    agrupamento = c("natureza", "lancamentos")) {
  agrupamento <- match.arg(agrupamento)

  e_cef_cmfcns(f_caminho.pasta.ciweb_c) %>%
    dplyr::filter(!is.na(valor)) %>%
    mutate(mes = floor_date(data.movimento, "month")) %>%
    group_by(contrato, .data[[agrupamento]], arquivo, mes) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = mes,
      values_from = valor,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(
      contrato, all_of(agrupamento), arquivo, total,
      any_of(sort(names(.)[!names(.) %in% c("contrato", agrupamento, "arquivo", "total")]))
    )
}

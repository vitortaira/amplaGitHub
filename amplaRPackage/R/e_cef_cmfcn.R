#' @title Extração dos dados de um arquivo CMF_CN
#'
#' @description
#' A função **e_cef_cmfcn** extrai e organiza os dados de um arquivo CMF_CN
#' em PDF, consolidando informações sobre lançamentos financeiros.
#'
#' @param f_caminho.arquivo.cmfcn_c Caminho para o arquivo PDF do CMF_CN.
#'
#' @details
#' A função lê o arquivo PDF, processa as informações e retorna um tibble
#' contendo os dados organizados, como contratos, datas, valores e situações.
#'
#' @return
#' Retorna um tibble com os dados extraídos do arquivo CMF_CN.
#'
#' @examples
#' \dontrun{
#' f_caminho.arquivo.cmfcn_c <- "caminho/para/arquivo/MOV_FINANC_CN.pdf"
#' resultado <- e_cef_cmfcn(f_caminho.arquivo.cmfcn_c)
#' print(resultado)
#' }
#'
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_split str_squish str_remove str_detect str_starts
#' @importFrom stringr str_extract str_trim str_replace str_remove_all
#' @importFrom stringr str_count str_sub word
#' @importFrom purrr map discard keep
#' @importFrom dplyr mutate filter select group_by summarise pull rename
#' @importFrom tidyr separate pivot_longer extract
#' @importFrom tibble as_tibble_col tibble
#'
#' @export

e_cef_cmfcn <-
  function(f_caminho.arquivo.cmfcn_c) {
    # paginas_l e linhas_c ----------------------------------------------------

    paginas_l <-
      pdf_text(f_caminho.arquivo.cmfcn_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    emitente_c <- paginas_l[[1]] %>%
      last() %>%
      str_remove("^Emitente: ")
    linhas_c <-
      unlist(paginas_l, use.names = FALSE) %>%
      keep(~ !str_starts(.x, "Emitente:")) %>%
      as_tibble_col(column_name = "Linhas") %>%
      # Concatenar linhas do cabeçalho
      mutate(
        Linhas =
          if_else(str_detect(Linhas, "^CONTRATO"),
            str_c(Linhas, lead(Linhas), sep = " "),
            Linhas
          ),
        Drop = dplyr::lag(str_detect(Linhas, "^CONTRATO"), default = FALSE)
      ) %>%
      dplyr::filter(!Drop) %>%
      select(Linhas) %>%
      # Concatenar linhas iniciadas em contratos com iniciadas em conta SIDEC/NSGD
      mutate(
        Linhas =
          if_else(str_detect(Linhas, "^[0-9]{12}"),
            str_c(Linhas, lead(Linhas), sep = " "),
            Linhas
          ),
        Drop = dplyr::lag(str_detect(Linhas, "^[0-9]{12}"), default = FALSE)
      ) %>%
      dplyr::filter(!Drop) %>%
      mutate(Grupo = cumsum(!str_detect(Linhas, "^(CIF|CL)"))) %>%
      group_by(Grupo) %>%
      summarise(Linhas = str_c(Linhas, collapse = "; "), .groups = "drop") %>%
      pull(Linhas)
    # Lançamentos
    lancamentos_t <-
      tibble(Linhas = linhas_c %>% head(str_which(linhas_c, "^TOTAIS")[1])) %>%
      # tibble(Linhas = linhas_c) %>%
      dplyr::filter(str_detect(Linhas, "^[0-9]{12}")) %>%
      mutate(
        CONTRATO = Linhas %>% str_extract("^[0-9]{12}"),
        Linhas = Linhas %>% str_remove("^[0-9]{12}") %>% str_trim(),
        `DT. LANCTO` =
          Linhas %>%
            str_extract("^\\d{2}/\\d{2}/\\d{4}") %>%
            as.Date(format = "%d/%m/%Y"),
        Linhas =
          Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        `DT. REMES.` =
          Linhas %>%
            str_extract("^\\d{2}/\\d{2}/\\d{4}") %>%
            as.Date(format = "%d/%m/%Y"),
        Linhas =
          Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        `MOT.` = Linhas %>% str_sub(-2, -1) %>% as.integer(),
        Linhas = Linhas %>% str_sub(1, -3) %>% str_trim(),
        SITUACAO =
          Linhas %>% str_remove(".*\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>% str_trim(),
        VALOR =
          Linhas %>%
            str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>%
            str_trim() %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        Linhas =
          Linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}.*") %>% str_trim(),
        `CONTA SIDEC/NSGD` =
          if_else(
            Linhas %>% str_detect("\\d{5}\\."),
            Linhas %>% str_extract("\\d{5}\\..*"),
            NA
          ),
        Linhas =
          if_else(
            Linhas %>% str_detect("\\d{5}\\."),
            Linhas %>% str_remove("\\d{5}\\..*") %>% str_trim(),
            Linhas
          ),
        NP = Linhas %>% str_sub(-2, -1) %>% as.integer(),
        LANCAMENTOS = Linhas %>% str_sub(1, -3) %>% str_trim(),
        Arquivo = f_caminho.arquivo.cmfcn_c
      ) %>%
      select(
        CONTRATO, `DT. LANCTO`, `DT. REMES.`, LANCAMENTOS, NP,
        `CONTA SIDEC/NSGD`, VALOR, SITUACAO, `MOT.`, Arquivo
      )
    # TOTAL DT. REM
    # max.cols_i <-
    #  max(str_count(linhas_c[str_which(linhas_c, "^TOTAL")], fixed(";"))) + 1
    # nomes.cols_c <- str_c("Col", seq_len(max.cols_i))
    # total.dt.rem_t <-
    #  tibble(Linhas = linhas_c %>% head(str_which(linhas_c, "^TOTAIS")[1])) %>%
    #  dplyr::filter(str_detect(Linhas, "^TOTAL")) %>%
    #  separate(
    #    col = Linhas,
    #    into = nomes.cols_c,
    #    sep = ";",
    #    fill = "right"
    #  ) %>%
    #  mutate(
    #    Col1 = Col1 %>% str_remove("^TOTAL DT.REM "),
    #    `DT. REMES.` = Col1 %>% word() %>% as.Date(format = "%d/%m/%Y"),
    #    Col1 = Col1 %>% str_remove(word(Col1)) %>% str_trim()
    #  ) %>%
    #  pivot_longer(
    #    cols = starts_with("Col"),
    #    names_to = "Var",
    #    values_to = "Linhas"
    #  ) %>%
    #  extract(
    #    col = Linhas,
    #    into = c("Descricao", "Valor"),
    #    regex = "^(.*?)(\\d{1,3}(?:\\.\\d{3})*,\\d{2})",
    #    remove = FALSE
    #  ) %>%
    #  select(-Var) %>%
    #  dplyr::filter(!is.na(Linhas)) %>%
    #  mutate(
    #    Linhas = Linhas %>% str_trim(),
    #    VALOR =
    #      Linhas %>%
    #        word(-1) %>%
    #        str_remove_all("\\.") %>%
    #        str_replace("\\,", "\\.") %>%
    #        as.numeric(),
    #    DESCRICAO =
    #      Linhas %>%
    #        str_remove(str_c(word(., -1), "$")) %>%
    #        str_trim() %>%
    #        str_remove_all(" "),
    #  ) %>%
    #  select(-Linhas)
    ###############################################################################
    #    teste.datas.lancamentos_t <-
    #      lancamentos_t %>%
    #      group_by(`DT. REMES.`) %>%
    #      summarise(Total.lancamentos = sum(VALOR))
    #    teste.datas.total_t <-
    #      total.dt.rem_t %>%
    #      group_by(`DT. REMES.`) %>%
    #      summarise(Total.total = sum(VALOR))
    #    teste.datas_t <-
    #      full_join(
    #        teste.lancamentos_t,
    #        teste.total_t,
    #        by = "DT. REMES."
    #      ) %>%
    #      mutate(
    #        Igual = if_else(near(Total.lancamentos, Total.total), TRUE, FALSE)
    #        #`L>T` =
    #        #  if_else(
    #        #    (Total.lancamentos > Total.total)
    #        #      & !near(Total.lancamentos, Total.total),
    #        #    TRUE,
    #        #    FALSE
    #        #  ),
    #        #`L<T` =
    #        #  if_else(
    #        #    (Total.lancamentos < Total.total)
    #        #      & !near(Total.lancamentos, Total.total),
    #        #    TRUE,
    #        #    FALSE
    #        #  )
    #      )
    ############################################################################
    return(lancamentos_t)
  }

# Teste -------------------------------------------------------------------

# comeco.linhas_c <- word(linhas_c)
# comeco.linhas.letras_c <-
#  comeco.linhas_c %>%
#  keep(~ str_detect(.x, "^[A-Z]") & !str_detect(.x, "^CIF|^CL"))
# comeco.linhas.numeros_c <-
#  comeco.linhas_c %>% keep(~ str_detect(.x, "^\\d"))
# sort(unique(diff(str_which(linhas.lancamentos_c, "^CONTRATO"))))
# f_caminho.arquivo.cmfcn_c <-
#  here::here(
#    "Relatórios - Documentos", "Relatorios - CIWEB", "2. UP Jardim Prudencia",
#    "11.03.25", "CMF", "20250311_123907_698_PP_177770016646_MOV_FINANC_CN.pdf"
#  )
# e_cef_cmfcn(f_caminho.arquivo.cmfcn_c)
# shell.exec(f_caminho.arquivo.cmfcn_c)

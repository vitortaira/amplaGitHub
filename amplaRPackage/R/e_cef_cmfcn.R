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
      as_tibble_col(column_name = "linhas") %>%
      # Concatenar linhas do cabeçalho
      mutate(
        linhas =
          if_else(str_detect(linhas, "^CONTRATO"),
            str_c(linhas, lead(linhas), sep = " "),
            linhas
          ),
        drop = dplyr::lag(str_detect(linhas, "^CONTRATO"), default = FALSE)
      ) %>%
      dplyr::filter(!drop) %>%
      select(linhas) %>%
      # Concatenar linhas iniciadas em contratos com iniciadas em conta SIDEC/NSGD
      mutate(
        linhas =
          if_else(str_detect(linhas, "^[0-9]{12}"),
            str_c(linhas, lead(linhas), sep = " "),
            linhas
          ),
        drop = dplyr::lag(str_detect(linhas, "^[0-9]{12}"), default = FALSE)
      ) %>%
      dplyr::filter(!drop) %>%
      mutate(grupo = cumsum(!str_detect(linhas, "^(CIF|CL)"))) %>%
      group_by(grupo) %>%
      summarise(linhas = str_c(linhas, collapse = "; "), .groups = "drop") %>%
      pull(linhas)
    # Lançamentos
    lancamentos_t <-
      tibble(linhas = linhas_c %>% head(str_which(linhas_c, "^TOTAIS")[1])) %>%
      # tibble(linhas = linhas_c) %>%
      dplyr::filter(str_detect(linhas, "^[0-9]{12}")) %>%
      mutate(
        contrato = linhas %>% str_extract("^[0-9]{12}"),
        linhas = linhas %>% str_remove("^[0-9]{12}") %>% str_trim(),
        data.lancamento =
          linhas %>%
            str_extract("^\\d{2}/\\d{2}/\\d{4}") %>%
            as.Date(format = "%d/%m/%Y"),
        linhas =
          linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        data.remessa =
          linhas %>%
            str_extract("^\\d{2}/\\d{2}/\\d{4}") %>%
            as.Date(format = "%d/%m/%Y"),
        linhas =
          linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
        mot = linhas %>% str_sub(-2, -1) %>% as.integer(),
        linhas = linhas %>% str_sub(1, -3) %>% str_trim(),
        situacao =
          linhas %>% str_remove(".*\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>% str_trim(),
        valor =
          linhas %>%
            str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>%
            str_trim() %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        linhas =
          linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}.*") %>% str_trim(),
        `conta.sidec/nsgd` =
          if_else(
            linhas %>% str_detect("\\d{5}\\."),
            linhas %>% str_extract("\\d{5}\\..*"),
            NA
          ),
        linhas =
          if_else(
            linhas %>% str_detect("\\d{5}\\."),
            linhas %>% str_remove("\\d{5}\\..*") %>% str_trim(),
            linhas
          ),
        np = linhas %>% str_sub(-2, -1) %>% as.integer(),
        lancamentos = linhas %>% str_sub(1, -3) %>% str_trim(),
        arquivo = f_caminho.arquivo.cmfcn_c
      ) %>%
      select(
        contrato, data.lancamento, data.remessa, lancamentos, np,
        `conta.sidec/nsgd`, valor, situacao, mot, arquivo
      )
    # TOTAL DT. REM
    # max.cols_i <-
    #  max(str_count(linhas_c[str_which(linhas_c, "^TOTAL")], fixed(";"))) + 1
    # nomes.cols_c <- str_c("Col", seq_len(max.cols_i))
    # total.dt.rem_t <-
    #  tibble(linhas = linhas_c %>% head(str_which(linhas_c, "^TOTAIS")[1])) %>%
    #  dplyr::filter(str_detect(linhas, "^TOTAL")) %>%
    #  separate(
    #    col = linhas,
    #    into = nomes.cols_c,
    #    sep = ";",
    #    fill = "right"
    #  ) %>%
    #  mutate(
    #    Col1 = Col1 %>% str_remove("^TOTAL DT.REM "),
    #    data.remessa = Col1 %>% word() %>% as.Date(format = "%d/%m/%Y"),
    #    Col1 = Col1 %>% str_remove(word(Col1)) %>% str_trim()
    #  ) %>%
    #  pivot_longer(
    #    cols = starts_with("Col"),
    #    names_to = "Var",
    #    values_to = "linhas"
    #  ) %>%
    #  extract(
    #    col = linhas,
    #    into = c("Descricao", "Valor"),
    #    regex = "^(.*?)(\\d{1,3}(?:\\.\\d{3})*,\\d{2})",
    #    remove = FALSE
    #  ) %>%
    #  select(-Var) %>%
    #  dplyr::filter(!is.na(linhas)) %>%
    #  mutate(
    #    linhas = linhas %>% str_trim(),
    #    valor =
    #      linhas %>%
    #        word(-1) %>%
    #        str_remove_all("\\.") %>%
    #        str_replace("\\,", "\\.") %>%
    #        as.numeric(),
    #    DESCRICAO =
    #      linhas %>%
    #        str_remove(str_c(word(., -1), "$")) %>%
    #        str_trim() %>%
    #        str_remove_all(" "),
    #  ) %>%
    #  select(-linhas)
    ###############################################################################
    #    teste.datas.lancamentos_t <-
    #      lancamentos_t %>%
    #      group_by(data.remessa) %>%
    #      summarise(Total.lancamentos = sum(valor))
    #    teste.datas.total_t <-
    #      total.dt.rem_t %>%
    #      group_by(data.remessa) %>%
    #      summarise(Total.total = sum(valor))
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

# e_cef_cmfcn(f_caminho.arquivo.cmfcn_c)
# shell.exec(f_caminho.arquivo.cmfcn_c)

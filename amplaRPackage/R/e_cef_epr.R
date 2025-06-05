# Descrição ---------------------------------------------------------------
#' @title Extracao dos dados do PDF de um relatorio EPR da CEF
#'
#' @description
#' Extrai e organiza dados de um relatorio EPR da CEF em PDF.
#'
#' @param f_caminho.epr_c Caminho completo para o arquivo PDF contendo o
#' relatorio EPR da CEF.
#'
#' @details
#' Utiliza o pacote pdftools para ler o arquivo e manipular o texto,
#' identificando padroes que auxiliam na extracao das informacoes.
#'
#' @return
#' Retorna uma tibble com as seguintes colunas:
#' - contrato       : Character
#' - NOME MUTUARIO  : Character
#' - uno            : Character
#' - ORR            : Character
#' - to             : Integer
#' - cod            : Character
#' - DT. ASSIN      : Date
#' - TIPO UND       : Character
#' - GAR. AUT       : Integer
#' - DT. INC. CTR   : Date
#' - DT. INC. REG   : Date
#' - VR RETIDO      : Numeric
#' - VR amoRTIZ     : Numeric
#' - amo            : Character
#'
#' @examples
#' \dontrun{
#' # Exemplo 1: Uso basico
#' epr <- e_cef_epr(f_caminho.epr_c = "caminho/para/o/relatorio_epr.pdf")
#' print(epr)
#'
#' # Exemplo 2: Integrando com outras funcoes de tratamento de dados
#' epr_filtrado <- e_cef_epr("caminho/para/o/relatorio_epr.pdf") %>%
#'   filter(valor.retido > 1000)
#' summary(epr_filtrado)
#' }
#'
#' @seealso
#' Consulte \code{\link{e_cef_eprs}}.
#'
#' @references
#' Consulte \code{\link{pdf_text}} para extracao de texto de arquivos PDF.
#'
#' @export

e_cef_epr <-
  function(f_caminho.epr_c) {
    # Define paginas_l
    paginas_l <-
      pdf_text(f_caminho.epr_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    # Define linhas_c
    epr_t <-
      paginas_l %>%
      unlist(use.names = FALSE) %>%
      keep(~ str_starts(.x, "\\d{12}")) %>%
      as_tibble_col(column_name = "linhas") %>%
      mutate(
        contrato = linhas %>% str_extract("^\\d{12}"),
        linhas = linhas %>% str_remove("^\\d{12}\\s?"),
        nome.mutuario = linhas %>% str_remove("\\d{5}.*") %>% str_trim(),
        linhas = linhas %>% str_extract("\\d{5}.*"),
        uno = linhas %>% str_extract("^\\d{5}"),
        linhas = linhas %>% str_remove("^\\d{5}\\s?"),
        orr = linhas %>% str_extract("^\\d{3}"),
        linhas = linhas %>% str_remove("^\\d{3}\\s?"),
        to = linhas %>% str_extract("^\\d{1}") %>% as.integer(),
        linhas = linhas %>% str_remove("^\\d{1}\\s?"),
        cod = linhas %>% str_extract("^\\d{3}"),
        linhas = linhas %>% str_remove("^\\d{3}\\s?"),
        data.assinatura =
          linhas %>%
            str_extract("^\\d{2}/\\d{2}/\\d{2}") %>%
            as.Date(format = "%d/%m/%y"),
        linhas = linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{2}\\s?"),
        data.inc.ctr =
          linhas %>%
            str_extract("\\d{2}/\\d{2}/\\d{2}") %>%
            as.Date(format = "%d/%m/%y"),
        linhas = linhas %>% str_remove("\\d{2}/\\d{2}/\\d{2}\\s?"),
        data.inc.reg =
          linhas %>%
            str_extract("\\d{2}/\\d{2}/\\d{2}") %>%
            as.Date(format = "%d/%m/%y"),
        linhas = linhas %>% str_remove("\\d{2}/\\d{2}/\\d{2}\\s?"),
        valor.retido =
          linhas %>%
            str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        linhas = linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?"),
        valor.amortiz =
          linhas %>%
            str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}") %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        linhas = linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?"),
        amo = linhas %>% word(-1),
        linhas = linhas %>% str_remove("\\s?\\S+$"),
        gar.aut = linhas %>% word(-1) %>% as.integer(),
        tipo.unidade =
          if_else(
            linhas %>% as.character() %>% str_count("\\S+") == 1,
            NA_character_,
            linhas %>% as.character() %>% word(1)
          ),
        arquivo = f_caminho.epr_c
      ) %>%
      select(
        contrato, nome.mutuario, uno, orr, to, cod, data.assinatura,
        tipo.unidade, gar.aut, data.inc.ctr, data.inc.reg, valor.retido,
        valor.amortiz, amo, arquivo
      )
    return(epr_t)
  }

# Teste -------------------------------------------------------------------

# f_caminho.epr_c <-
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - CIWEB",
#    "1. UP Vila Sonia", "11.03.25", "EPR",
#    "20250311_123902_696_PP_177770014920_contratoS_EMPREEND.pdf"
#  )
#  here::here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRAto 2429 - FEVEREIRO.pdf"
#  )
# extrato <- dados_epr(f_caminho.epr_c)
# shell.exec(f_caminho.epr_c)

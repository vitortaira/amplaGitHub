# Descrição ---------------------------------------------------------------

#' @title Extração dos dados do PDF de um extrato da CEF
#'
#' @description
#' Extrai e organiza dados de um extrato bancário da CEF em PDF.
#'
#' @param f_caminho.arquivo_c Caminho completo para o arquivo PDF contendo o
#' extrato da CEF.
#'
#' @details
#' Utiliza o pacote pdftools para ler o arquivo e manipular o texto,
#' identificando padrões que auxiliam na extração das informações.
#'
#' @return
#' Retorna uma tibble com as seguintes colunas:
#'   - Data de lançamento  : Date
#'   - Data de movimento   : Date
#'   - Documento           : Character
#'   - Histórico           : Character
#'   - Valor               : Numeric
#'   - Saldo               : Numeric
#'   - Conta_interno       : Character
#'   - Conta               : Character
#'   - Agência             : Character
#'   - Produto             : Character
#'   - CNPJ                : Character
#'   - Cliente             : Character
#'   - Período_início      : Date
#'   - Período_fim         : Date
#'   - Data_consulta       : POSIXct
#'
#' @examples
#' \dontrun{
#' # Exemplo 1: Uso básico
#' extrato <- e_cef_extrato(
#'   f_caminho.arquivo_c = "caminho/para/o/extrato.pdf"
#' )
#' print(extrato)
#'
#' # Exemplo 2: Integrando com outras funções de tratamento de dados
#' library(dplyr)
#' extrato_filtrado <- e_cef_extrato("caminho/para/o/extrato.pdf") %>%
#'   filter(Valor > 0)
#' summary(extrato_filtrado)
#' }
#'
#' @seealso
#' Consulte \code{\link{e_cef_extratos}}.
#'
#' @references
#' Consulte \code{\link{pdf_text}} para extração de texto de arquivos PDF.
#'
#' @export

# Pacotes -----------------------------------------------------------------

library(pdftools) # Funções para extração de dados em PDF

e_cef_extrato <-
  function(f_caminho.arquivo_c) {
    # Define paginas_l
    paginas_l <-
      pdf_text(f_caminho.arquivo_c) %>%
      map(
        ~ str_split(.x, "\n")[[1]] %>%
          str_squish() %>%
          discard(~ .x == "")
      )
    # Se o extrato da CEF for do tipo com o título "Extrato por período"
    if (
      paginas_l[[1]] %>% str_detect("Extra([A-Za-z]{2})? por per") %>% sum() > 0
    ) {
      # Define linhas_c
      linhas_c <-
        unlist(paginas_l, use.names = FALSE)
      # Metadados
      cliente_c <-
        linhas_c %>%
        keep(~ str_starts(.x, "Cliente:")) %>%
        str_remove("^Cliente: ") %>%
        str_trim()
      conta_c <-
        linhas_c %>%
        keep(~ str_starts(.x, "Conta[A-Za-z]?:")) %>%
        str_remove("^Conta[A-Za-z]?:\\s?") %>%
        str_trim()
      data.consulta_h <-
        case_when(
          str_starts(nth(linhas_c, 1), "\\d{2}/\\d{2}/\\d{4}") ~
            nth(linhas_c, 1) %>%
            str_extract("\\d{2}/\\d{2}/\\d{4}\\,\\s?\\d{2}\\:\\d{2}") %>%
            str_replace("\\,\\s?", "-") %>%
            as.POSIXct(format = "%d/%m/%Y-%H:%M"),
          sum(str_starts(linhas_c, "Data:")) > 0 ~
            (linhas_c %>%
              keep(~ str_starts(.x, "Data:")) %>%
              str_extract("\\d{2}/\\d{2}/\\d{4}\\s?-\\s?\\d{2}\\:\\d{2}") %>%
              str_remove_all(" "))[1] %>%
            as.POSIXct(format = "%d/%m/%Y-%H:%M"),
          TRUE ~ NA
        )
      mes.consultado_d <-
        linhas_c %>%
        keep(~ str_starts(.x, "Mês:")) %>%
        str_remove("^Mês: ") %>%
        str_trim() %>%
        str_replace_all(
          c(
            "Janeiro"   = "01",
            "Fevereiro" = "02",
            "Março"     = "03",
            "Abril"     = "04",
            "Maio"      = "05",
            "Junho"     = "06",
            "Julho"     = "07",
            "Agosto"    = "08",
            "Setembro"  = "09",
            "Outubro"   = "10",
            "Novembro"  = "11",
            "Dezembro"  = "12"
          )
        )
      periodo.consultado_c <-
        str_c(
          linhas_c %>%
            keep(~ str_starts(.x, "Período:")) %>%
            str_remove("^Período: ") %>%
            str_replace_all(" ", "") %>%
            str_trim() %>%
            str_extract(".*(?=-)") %>%
            if_else(str_length(.) == 1, str_c("0", .), .),
          "/",
          mes.consultado_d,
          "-",
          linhas_c %>%
            keep(~ str_starts(.x, "Período:")) %>%
            str_remove("^Período: ") %>%
            str_replace_all(" ", "") %>%
            str_trim() %>%
            str_extract("(?<=-).*") %>%
            if_else(str_length(.) == 1, str_c("0", .), .),
          "/",
          mes.consultado_d
        )
      linhas_c %<>%
        keep(
          ~ !str_starts(.x, "https") &
            !str_starts(.x, "file:") &
            !str_ends(.x, "CaIXA") &
            !str_starts(.x, "\\d{2}/\\d{2}/\\d{4}\\,")
        ) %>%
        str_remove_all("\\°|\\º")
      # metadados_t <-
      #  tibble(
      #    Agência = conta_c %>% str_sub(1, 4),
      #    Cliente = cliente_c,
      #    CNPJ = NA,
      #    Conta   = conta_c %>% word(-1) %>% str_trim,
      #    Data    = data.consulta_h,
      #    Período_início =
      #      periodo.consultado_c %>%
      #      str_remove("-.*") %>%
      #      as.Date(format = "%d/%m/%Y"),
      #    Período_fim =
      #      periodo.consultado_c %>%
      #      str_remove(".*-") %>%
      #      as.Date(format = "%d/%m/%Y"),
      #    Produto = conta_c %>% str_sub(6, -1) %>% str_extract("\\s\\d{4}\\s")
      #  )
      # Índices
      indice.comeco_i <- linhas_c %>%
        str_which("^\\d{2}") %>%
        nth(1)
      indice.fim_i <- linhas_c %>%
        str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
        last()
      # Define extrato_t
      extrato_t <-
        linhas_c %>%
        as_tibble_col(column_name = "Linhas") %>%
        slice(indice.comeco_i:indice.fim_i) %>%
        mutate(
          `Data Mov.` =
            if_else(
              Linhas %>% word() == "000000",
              periodo.consultado_c %>%
                str_extract(".*(?=-)") %>%
                as.Date(format = "%d/%m/%Y"),
              Linhas %>% word() %>% as.Date(format = "%d/%m/%Y")
            ),
          Linhas = Linhas %>% str_remove("^\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
          `Nr. Doc` = Linhas %>% word(),
          Linhas = Linhas %>% str_remove(str_c("^", word(.))) %>% str_trim(),
          Saldo =
            Linhas %>%
              str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$") %>%
              str_remove("\\s?C") %>%
              str_remove_all("\\.") %>%
              str_replace("\\,", "\\.") %>%
              if_else(
                str_detect(., "D$"),
                str_c("-", .) %>% str_remove("\\s?D$"),
                .
              ) %>%
              as.numeric(),
          Linhas =
            Linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?$"),
          Valor =
            Linhas %>%
              str_extract("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?") %>%
              str_remove("\\s?C") %>%
              str_remove_all("\\.") %>%
              str_replace("\\,", "\\.") %>%
              if_else(
                str_detect(., "D$"),
                str_c("-", .) %>% str_remove("\\s?D$"),
                .
              ) %>%
              as.numeric(),
          Histórico =
            Linhas %>% str_remove("\\d{1,3}(?:\\.\\d{3})*,\\d{2}\\s?[C|D]?"),
          `Data Lanc.` = NA,
          # Metadados
          Conta = conta_c %>% word(-1) %>% str_trim(),
          Agência = conta_c %>% str_sub(1, 4),
          Produto = conta_c %>% str_sub(6, -1) %>% str_extract("\\s\\d{4}\\s"),
          CNPJ = NA,
          Cliente = cliente_c,
          Período_início =
            periodo.consultado_c %>%
              str_remove("-.*") %>%
              as.Date(format = "%d/%m/%Y"),
          Período_fim =
            periodo.consultado_c %>%
              str_remove(".*-") %>%
              as.Date(format = "%d/%m/%Y"),
          Data_consulta = data.consulta_h,
          Conta_interno =
            f_caminho.arquivo_c %>%
              basename() %>%
              str_extract("\\d{4}")
        ) %>%
        select(
          `Data Lanc.`, `Data Mov.`, `Nr. Doc`, Histórico, Valor, Saldo,
          # Metadados
          Conta_interno, Conta, Agência, Produto, CNPJ, Cliente, Período_início,
          Período_fim, Data_consulta
        ) %>%
        rename(
          `Data de lançamento` = `Data Lanc.`,
          `Data de movimento` = `Data Mov.`,
          Documento = `Nr. Doc`
        )
      return(extrato_t)
    }
    # Se o extrato da CEF for do tipo sem o título "Extrato por período"
    else {
      # Define linhas_c
      linhas_c <-
        unlist(paginas_l, use.names = FALSE) %>%
        keep(~ !str_starts(.x, "Data de lançamento"))
      # Metadados
      agencia_c <-
        linhas_c %>%
        keep(~ str_starts(.x, "Agência:")) %>%
        str_remove("\\s*Produto:.*") %>%
        str_sub(-4, -1)
      cliente_c <- linhas_c %>%
        nth(1) %>%
        str_trim()
      cnpj_c <-
        linhas_c %>%
        nth(2) %>%
        str_remove("^CNPJ:\\s*") %>%
        str_remove_all("[A-Za-z]") %>%
        str_trim()
      conta_c <-
        linhas_c %>%
        keep(~ str_starts(.x, "Agência:")) %>%
        str_remove("\\s*\\d{2}/\\d{2}/\\d{4}.*") %>%
        str_remove(".*Conta:\\s*") %>%
        str_trim()
      data.consulta_h <-
        linhas_c %>%
        keep(~ str_starts(.x, "Agência:")) %>%
        str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}") %>%
        str_trim() %>%
        as.POSIXct(format = "%d/%m/%Y %H:%M")
      periodo.consultado_c <-
        linhas_c %>%
        keep(~ str_detect(.x, "Lançamentos de")) %>%
        str_remove(".*amentos de\\s?") %>%
        str_remove_all(" ") %>%
        str_replace("à", "-") %>%
        str_trim()
      produto_c <-
        linhas_c %>%
        keep(~ str_starts(.x, "Agência:")) %>%
        str_remove("\\s*Conta:.*") %>%
        str_remove(".*Produto:\\s*") %>%
        str_trim()
      # metadados_t <-
      #  tibble(
      #    Agência = agencia_c,
      #    Cliente = cliente_c,
      #    CNPJ = cnpj_c,
      #    Conta   = conta_c,
      #    Data    = data.consulta_h,
      #    Período_início =
      #      periodo.consultado_c %>%
      #      str_remove("-.*") %>%
      #      as.Date(format = "%d/%m/%Y"),
      #    Período_fim =
      #      periodo.consultado_c %>%
      #      str_remove(".*-") %>%
      #      as.Date(format = "%d/%m/%Y"),
      #    Produto = produto_c
      #  )
      # Índices
      indice.comeco_i <- linhas_c %>% str_which("^Extrato") + 1
      indice.fim_i <- linhas_c %>%
        str_which("^\\d{2}/\\d{2}/\\d{4}") %>%
        last()
      # Define extrato_t
      extrato_t <-
        linhas_c %>%
        as_tibble_col(column_name = "Linhas") %>%
        slice(indice.comeco_i:indice.fim_i) %>%
        mutate(
          `Data de lançamento` =
            Linhas %>%
              str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
              as.Date(format = "%d/%m/%Y"),
          Linhas = Linhas %>% str_remove("\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
          `Data de movimento` =
            Linhas %>%
              str_extract("\\d{2}/\\d{2}/\\d{4}") %>%
              as.Date(format = "%d/%m/%Y"),
          Linhas = Linhas %>% str_remove("\\d{2}/\\d{2}/\\d{4}") %>% str_trim(),
          Documento = Linhas %>% str_remove("[A-Za-z].*") %>% str_trim(),
          Linhas = Linhas %>% str_extract("(?i)[A-Za-z].*") %>% str_trim(),
          Histórico = Linhas %>% str_remove("R\\$.*") %>% str_trim(),
          Linhas = Linhas %>% str_extract("(?<=R\\$).*") %>% str_trim(),
          `Valor(R$)` =
            Linhas %>% str_remove("R\\$.*") %>% str_remove_all("\\.") %>%
              str_replace("\\,", "\\.") %>% as.numeric(),
          `Saldo(R$)` =
            Linhas %>% str_extract("(?<=R\\$).*") %>% str_trim() %>%
              str_remove_all("\\.") %>% str_replace("\\,", "\\.") %>% as.numeric(),
          # Metadados
          Agência = agencia_c,
          Cliente = cliente_c,
          CNPJ = cnpj_c,
          Conta = conta_c,
          Data_consulta = data.consulta_h,
          Período_início =
            periodo.consultado_c %>%
              str_remove("-.*") %>%
              as.Date(format = "%d/%m/%Y"),
          Período_fim =
            periodo.consultado_c %>%
              str_remove(".*-") %>%
              as.Date(format = "%d/%m/%Y"),
          Produto = produto_c,
          Conta_interno =
            f_caminho.arquivo_c %>%
              basename() %>%
              str_extract("\\d{4}")
        ) %>%
        select(
          `Data de lançamento`, `Data de movimento`, Documento, Histórico,
          `Valor(R$)`, `Saldo(R$)`,
          # Metadados
          Conta_interno, Conta, Agência, Produto, CNPJ, Cliente, Período_início,
          Período_fim, Data_consulta
        ) %>%
        rename(
          Saldo = `Saldo(R$)`,
          Valor = `Valor(R$)`
        )
      return(extrato_t)
    }
  }

# Teste -------------------------------------------------------------------

# f_caminho.arquivo_c <- caminhos.extratos.cef_c[1]
#  here("Relatórios - Documentos", "Relatorios - Extratos",
#    "Estação", "Fevereiro 2025", "CAIXA -  2419 - FEVEREIRO.pdf"
#  )
#  here("..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#    "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#  )
# extrato <- e_cef_extrato(f_caminho.arquivo_c)
# shell.exec(f_caminho.arquivo_c)

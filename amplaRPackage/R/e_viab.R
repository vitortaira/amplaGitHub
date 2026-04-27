#' @title Extracao de dados de um arquivo de viabilidade
#'
#' @description
#' A funcao **e_viab** le um unico arquivo .xlsx de viabilidade e
#' extrai dados de abas especificas, retornando uma lista nomeada de tibbles.
#'
#' @param f_caminho_arquivo_c Caminho local para o arquivo .xlsx de
#'   viabilidade.
#'
#' @return
#' Retorna uma lista nomeada com os seguintes elementos:
#' \itemize{
#'   \item def: Tibble com indicadores extraidos da aba
#'     "Demonstrativo Eco-Finc".
#'   \item flx: Tibble (placeholder) com dados extraidos da aba "Fluxo".
#' }
#'
#' @examples
#' \dontrun{
#' dados_l <- e_viab("caminho/para/Viabilidade Projeto.xlsx")
#' dados_l$def
#' dados_l$flx
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter pull across everything mutate select
#' @importFrom magrittr set_names
#' @importFrom stringr str_c str_squish str_starts str_detect
#' @importFrom tibble tibble
#' @importFrom lubridate parse_date_time
#'
#' @export

e_viab <- function(f_caminho_arquivo_c) {
  list(
    def = e_viab_def(f_caminho_arquivo_c),
    flx = e_viab_flx(f_caminho_arquivo_c)
  )
}

# Helper: extracts data from the "Demonstrativo Eco-Finc" tab
e_viab_def <- function(f_caminho_arquivo_c) {
  viab.original_t <- suppressMessages(suppressWarnings(
    read_excel(
      f_caminho_arquivo_c,
      sheet = "Demonstrativo Eco-Finc"
    ) %>%
      set_names(str_c("_", seq_len(ncol(.)))) %>%
      mutate(across(everything(), ~ str_squish(as.character(.))))
  ))

  vgv_n <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_4`) %>%
    as.numeric()

  despesas.obra_n <- viab.original_t %>%
    filter(str_detect(`_1`, "(?i)constru[cç][aã]o")) %>%
    pull(`_7`) %>%
    as.numeric()

  impostos.lucro_n <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?para\\s?venda")) %>%
    pull(`_10`) %>%
    as.numeric()

  impostos.receita_n <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?para\\s?venda")) %>%
    pull(`_10`) %>%
    as.numeric()

  lucro.liq_n <- viab.original_t %>%
    filter(str_starts(`_1`, "(?i)lucro\\s?l[ií]quido")) %>%
    pull(`_7`) %>%
    as.numeric()

  terreno.permuta.fisica_n <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)terreno\\s?permuta\\s?f[ií]sica")) %>%
    pull(`_7`) %>%
    as.numeric()

  unidades.venda_n <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_5`) %>%
    as.numeric()

  tibble(
    vgv = vgv_n,
    despesas.obra = despesas.obra_n,
    impostos.lucro = impostos.lucro_n,
    impostos.receita = impostos.receita_n,
    lucro.liquido = lucro.liq_n,
    terreno.permuta.fisica = terreno.permuta.fisica_n,
    unidades.venda = unidades.venda_n
  )
}

# Helper: extracts monthly data from the "Fluxo" tab in long format.
# Currently extracts the variables "CEF obra" (column under header
# "Frac Obra"), "CEF terreno" (column under header "Frac Terreno") and
# "Uni" (column under header "Uni" between "Curva de Venda" and
# "Pró-Soluto Direto"), with months from the column under the header "Mes".
# Data rows span from the header row + 1 down to the row before the first
# row containing "SubTotal" in any of its cells.
e_viab_flx <- function(f_caminho_arquivo_c) {
  # Mapping: spreadsheet-header label (key) -> spec for locating the column
  # in the Fluxo tab and naming the variable in the output.
  # Each spec is a list with:
  #   - header: regex matching the header text in the variable's column
  #   - nome:   value placed in the `variavel` column of the output
  #   - after:  (optional) regex for a header that must occur to the LEFT
  #   - before: (optional) regex for a header that must occur to the RIGHT
  # `after`/`before` disambiguate when `header` is not unique in the row.
  variaveis_l <- list(
    "Fraç Obra" = list(
      header = "(?i)^fra[cç]\\s?obra$",
      nome   = "CEF obra"
    ),
    "Fraç Terreno" = list(
      header = "(?i)^fra[cç]\\s?terreno$",
      nome   = "CEF terreno"
    ),
    "Uni" = list(
      header = "(?i)^uni$",
      nome   = "Unidades vendidas",
      after  = "(?i)^curva\\s?de\\s?venda$",
      before = "(?i)^pr[óo]-?\\s?soluto\\s?direto$"
    ),
    "Repasse ABC" = list(
      header = "(?i)^repasse\\s?abc$",
      nome   = "Repasse ABC"
    )
  )

  fluxo.bruto_t <- suppressMessages(suppressWarnings(
    read_excel(
      f_caminho_arquivo_c,
      sheet = "Fluxo",
      col_names = FALSE,
      col_types = "text"
    )
  ))

  cell_eq <- function(x, padrao_c) {
    !is.na(x) & str_detect(x, padrao_c)
  }

  # Locate the header row: the first row containing "Mes/Mês" and at least
  # one of the variable headers.
  achou_mes_lin <- apply(
    fluxo.bruto_t, 1, function(linha) any(cell_eq(linha, "(?i)^m[eê]s$"))
  )
  achou_qualquer_var_lin <- apply(
    fluxo.bruto_t, 1,
    function(linha) {
      any(vapply(
        variaveis_l, function(spec) any(cell_eq(linha, spec$header)),
        logical(1)
      ))
    }
  )
  linha_header_n <- which(achou_mes_lin & achou_qualquer_var_lin)[1]
  if (is.na(linha_header_n)) {
    stop(
      "Header row not found in 'Fluxo' tab of: ", f_caminho_arquivo_c,
      call. = FALSE
    )
  }

  linha_header_v <- as.character(unlist(fluxo.bruto_t[linha_header_n, ]))
  col_mes_n <- which(cell_eq(linha_header_v, "(?i)^m[eê]s$"))[1]

  # Locate the subtotal row (first occurrence after the header).
  apos_header_t <- fluxo.bruto_t[(linha_header_n + 1):nrow(fluxo.bruto_t), ]
  achou_subtotal_lin <- apply(
    apos_header_t, 1,
    function(linha) any(cell_eq(linha, "(?i)subtotal"))
  )
  pos_subtotal_n <- which(achou_subtotal_lin)[1]
  if (is.na(pos_subtotal_n)) {
    stop(
      "Row with 'SubTotal' not found in 'Fluxo' tab of: ",
      f_caminho_arquivo_c,
      call. = FALSE
    )
  }
  linha_subtotal_n <- linha_header_n + pos_subtotal_n

  # Data range: from the row right below the header to the row before subtotal
  inicio_n <- linha_header_n + 1
  fim_n <- linha_subtotal_n - 1

  vazio_t <- tibble(
    empreendimento = character(),
    variavel = character(),
    mes = as.Date(character()),
    valor = numeric()
  )
  if (fim_n < inicio_n) {
    return(vazio_t)
  }

  empreendimento_c <- tools::file_path_sans_ext(basename(f_caminho_arquivo_c))
  # Strip everything up to and including "Viabilidade " (case-insensitive),
  # so e.g. "viab_c9545_Viabilidade UP Jardim Prudência" becomes
  # "UP Jardim Prudência".
  empreendimento_c <- stringr::str_remove(
    empreendimento_c, "(?i).*viabilidade\\s+"
  )

  # Convert "mes" to Date. Values may be Excel serial numbers (since the
  # sheet was read as text) or already-formatted date strings.
  parse_mes <- function(x) {
    serial_n <- suppressWarnings(as.numeric(x))
    eh_serial <- !is.na(serial_n) & serial_n > 0
    resultado <- as.Date(rep(NA, length(x)))
    resultado[eh_serial] <- as.Date(
      serial_n[eh_serial],
      origin = "1899-12-30"
    )
    nao_serial <- !eh_serial & !is.na(x)
    if (any(nao_serial)) {
      resultado[nao_serial] <- suppressWarnings(
        lubridate::parse_date_time(
          x[nao_serial],
          orders = c("ymd", "dmy", "mdy", "Y-m", "m-Y", "b-Y", "B-Y"),
          quiet = TRUE
        ) %>%
          as.Date()
      )
    }
    resultado
  }

  mes_v <- parse_mes(unlist(fluxo.bruto_t[inicio_n:fim_n, col_mes_n]))

  # Extract one long tibble per variable, then bind them all together.
  purrr::map_dfr(names(variaveis_l), function(nome_var_c) {
    spec_l <- variaveis_l[[nome_var_c]]
    # Find all candidate columns matching the variable's header.
    candidatos_n <- which(cell_eq(linha_header_v, spec_l$header))
    # Apply optional positional constraints.
    if (length(candidatos_n) > 1 && !is.null(spec_l$after)) {
      pos_after_n <- which(cell_eq(linha_header_v, spec_l$after))
      if (length(pos_after_n) > 0) {
        candidatos_n <- candidatos_n[candidatos_n > min(pos_after_n)]
      }
    }
    if (length(candidatos_n) > 1 && !is.null(spec_l$before)) {
      pos_before_n <- which(cell_eq(linha_header_v, spec_l$before))
      if (length(pos_before_n) > 0) {
        candidatos_n <- candidatos_n[candidatos_n < max(pos_before_n)]
      }
    }
    col_var_n <- candidatos_n[1]
    if (is.na(col_var_n)) {
      return(vazio_t)
    }
    valor_v <- suppressWarnings(
      as.numeric(unlist(fluxo.bruto_t[inicio_n:fim_n, col_var_n]))
    )
    tibble(
      empreendimento = empreendimento_c,
      variavel = if (is.null(spec_l$nome)) nome_var_c else spec_l$nome,
      mes = mes_v,
      valor = valor_v
    )
  }) %>%
    dplyr::group_by(empreendimento, variavel) %>%
    dplyr::arrange(mes, .by_group = TRUE) %>%
    dplyr::filter({
      relevante <- !is.na(valor) & valor != 0
      if (!any(relevante)) {
        rep(FALSE, dplyr::n())
      } else {
        dplyr::row_number() >= which(relevante)[1] &
          dplyr::row_number() <= dplyr::last(which(relevante))
      }
    }) %>%
    # Interior NAs (gaps within the trimmed range, including unparseable
    # cells like "#N/A" or "Invalid Number") are treated as zeros.
    dplyr::mutate(valor = dplyr::coalesce(valor, 0)) %>%
    dplyr::ungroup()
}

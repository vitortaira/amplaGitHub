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

  Construção <- viab.original_t %>%
    filter(str_detect(`_1`, "(?i)constru[cç][aã]o")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  `Despesas financeiras` <- viab.original_t %>%
    filter(str_detect(`_3`, "(?i)juros/desp\\s?com\\s?fin.*")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  Incorporação <- viab.original_t %>%
    filter(str_detect(`_1`, "(?i)incorpora[cç][aã]o")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  lucro.liquido <- viab.original_t %>%
    filter(str_starts(`_1`, "(?i)lucro\\s?l[ií]quido")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  `Novos negócios` <- viab.original_t %>%
    filter(str_detect(`_1`, "(?i)novos\\s?neg[óo]cios")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  terreno.permuta.fisica <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)terreno\\s?permuta\\s?f[ií]sica")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  `Unidades vendidas` <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_5`) %>%
    as.numeric() %>%
    round(0)

  Vendas <- viab.original_t %>%
    filter(str_detect(`_1`, "(?i)^vendas$")) %>%
    pull(`_7`) %>%
    as.numeric() %>%
    round(2)

  vgv <- viab.original_t %>%
    filter(str_starts(`_3`, "(?i)vgv\\s?fluxo")) %>%
    pull(`_4`) %>%
    as.numeric() %>%
    round(2)

  tibble(
    "Construção" = Construção,
    "Despesas financeiras" = `Despesas financeiras`,
    "Incorporação" = Incorporação,
    lucro.liquido = lucro.liquido,
    "Novos negócios" = `Novos negócios`,
    terreno.permuta.fisica = terreno.permuta.fisica,
    "Unidades vendidas" = `Unidades vendidas`,
    "Vendas" = Vendas,
    vgv = vgv
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
  # Mapping: friendly variable name (key, used as the `variavel` value
  # in the output) -> spec for locating the column(s) in the Fluxo tab.
  # Each spec is a list with:
  #   - header:     regex matching the header text in the variable's column
  #                 (single-column variables only)
  #   - after:      (optional) regex for a header that must occur to the LEFT
  #   - before:    (optional) regex for a header that must occur to the RIGHT
  #   - under:      (optional) regex matched against the cells in any row
  #                 ABOVE the header (i.e. the column header sits UNDER
  #                 those super-header cells, with merged-cell values
  #                 fill-forwarded across each row). Useful for
  #                 disambiguating columns that share the same header but
  #                 live under different super-headers (e.g. "Vendas
  #                 Realizadas" vs. "Vendas Projetadas").
  #   - components: (optional) list of sub-specs (each with $header and
  #                 optional $after/$before/$under) whose columns will be
  #                 summed row-wise (NAs treated as 0). When provided, the
  #                 top-level $header/$after/$before/$under are ignored.
  #   - empreendimento: (optional) regex (or character vector of regexes)
  #                 matched against the file's empreendimento name. The
  #                 variable is only extracted when at least one of the
  #                 patterns matches; if omitted, the variable applies to
  #                 all files. Provide multiple patterns as a vector
  #                 (instead of one long alternation) for readability.
  # `after`/`before`/`under` disambiguate when `header` is not unique.
  variaveis_l <- list(
    "CEF obra" = list(
      header = "(?i)^fra[cç]\\s?obra$"
    ),
    "CEF terreno" = list(
      header = "(?i)^fra[cç]\\s?terreno$"
    ),
    "Construção" = list(
      components = list(
        list(header = "(?i)^obra$"),
        list(header = "(?i)^p[óo]s-?obra$"),
        list(header = "(?i)^taxa\\s?adm$")
      )
    ),
    "Despesas financeiras" = list(
      components = list(
        list(header = "(?i)^juros/desp$", before = "(?i)^libera[çc][ãa]o$"),
        list(header = "(?i)^juros/desp$", before = "(?i)^pis/cofins$")
      )
    ),
    "Empréstimo ABC" = list(
      empreendimento = "(?i)pomp[ée]ia",
      components = list(
        list(header = "(?i)^libera[çc][ãa]o$")
      )
    ),
    "Empréstimo CEF PJ" = list(
      empreendimento = c(
        "(?i)prud[êse]ncia",
        "(?i)up\\s?vila\\s?sonia",
        "(?i)select",
        "(?i)s[ãa]o\\s?lucas",
        "(?i)up\\s?esta[çc][ãa]o\\s?vila\\s?sonia",
        "(?i)up\\s?move"
      ),
      components = list(
        list(header = "(?i)^libera[çc][ãa]o$")
      )
    ),
    "Empréstimo Cyrela" = list(
      empreendimento = "(?i)sa[úu]de",
      components = list(
        list(header = "(?i)^libera[çc][ãa]o$")
      )
    ),
    "Incorporação" = list(
      components = list(
        list(header = "(?i)^projeto$"),
        list(header = "(?i)^dec\\s?[áa]rea\\s?c$"),
        list(header = "(?i)^statera$|(?i)^assessorias$"),
        list(header = "(?i)^incorp$"),
        list(
          header = "^(0?\\.\\d+|\\d+(\\.\\d+)?[eE][+-]?\\d+)$",
          after  = "(?i)^incorp$",
          before = "(?i)^p[óo]s-?obra$"
        )
      )
    ),
    "Novos negócios" = list(
      components = list(
        list(header = "(?i)^desp$"),
        list(header = "(?i)^terreno$"),
        list(header = "(?i)^outorga$")
      )
    ),
    "Pró soluto + Taxa extra" = list(
      components = list(
        list(
          header = "(?i)^direto$",
          under  = "(?i)vendas\\s?realizadas"
        ),
        list(
          header = "(?i)^direto$",
          under  = "(?i)vendas\\s?projetadas"
        )
      )
    ),
    "Repasse ABC" = list(
      header = "(?i)^repasse\\s?abc$"
    ),
    "Serviço das dívidas" = list(
      components = list(
        list(header = "(?i)^pagamento$", under = "(?i)financ.*$"),
        list(header = "(?i)^juros/desp$", before = "(?i)^libera[çc][ãa]o$"),
        list(header = "(?i)^pagamento$", under = "(?i)^\\w{3}\\.?-?\\d{2}$"),
        list(header = "(?i)^juros/desp$", before = "(?i)^pis/cofins$")
      )
    ),
    "Unidades vendidas" = list(
      header = "(?i)^uni$",
      after  = "(?i)^curva\\s?de\\s?venda$",
      before = "(?i)^pr[óo]-?\\s?soluto\\s?direto$"
    ),
    "Vendas" = list(
      components = list(
        list(header = "(?i)^comercial$"),
        list(header = "(?i)^pdv$"),
        list(header = "(?i)^marketing$")
      )
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
    !is.na(x) & str_detect(stringr::str_trim(x), padrao_c)
  }

  # Locate the header row: the first row containing "Mes/Mês" and at least
  # one of the variable headers.
  achou_mes_lin <- apply(
    fluxo.bruto_t, 1, function(linha) any(cell_eq(linha, "(?i)^m[eê]s$"))
  )
  # Collect all header regexes (single-column variables + components).
  headers_v <- unlist(lapply(variaveis_l, function(spec) {
    if (!is.null(spec$components)) {
      vapply(spec$components, function(c) c$header, character(1))
    } else {
      spec$header
    }
  }))
  achou_qualquer_var_lin <- apply(
    fluxo.bruto_t, 1,
    function(linha) {
      any(vapply(
        headers_v, function(h) any(cell_eq(linha, h)),
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
  # Fill vertically-merged headers: when a column's header cell on the
  # header row is NA, take the closest non-NA value from the rows above
  # (readxl returns NA for non-leading cells of a vertical merge).
  if (linha_header_n > 1) {
    na_cols_n <- which(is.na(linha_header_v) | !nzchar(linha_header_v))
    for (col_n in na_cols_n) {
      acima_v <- as.character(unlist(
        fluxo.bruto_t[seq_len(linha_header_n - 1), col_n]
      ))
      acima_v <- acima_v[!is.na(acima_v) & nzchar(acima_v)]
      if (length(acima_v) > 0) {
        linha_header_v[col_n] <- acima_v[length(acima_v)]
      }
    }
  }
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

  # All rows above the header, each fill-forwarded across the row to
  # propagate values from merged cells (readxl returns NA for non-leading
  # cells of a merge). A spec's `under` regex matches a column if it
  # matches the (filled-forward) value at that column in ANY row above.
  linhas_acima_l <- if (linha_header_n > 1) {
    lapply(seq_len(linha_header_n - 1), function(r) {
      v <- as.character(unlist(fluxo.bruto_t[r, ]))
      ultimo_c <- NA_character_
      for (i in seq_along(v)) {
        if (!is.na(v[i]) && nzchar(v[i])) {
          ultimo_c <- v[i]
        } else {
          v[i] <- ultimo_c
        }
      }
      v
    })
  } else {
    list()
  }

  # Helper: given a sub-spec with $header (and optional $after/$before/$under),
  # returns the matching column index in `linha_header_v`, or NA. The
  # optional `excluir_n` vector lists columns already claimed by previous
  # components (within the same multi-column variable); those are skipped
  # so that overlapping alternations like `^statera$|^incorp$` paired with
  # `^statera$|^assessorias$` resolve to distinct columns when both names
  # coexist in the spreadsheet.
  achar_coluna <- function(sub_spec_l, excluir_n = integer()) {
    candidatos_n <- which(cell_eq(linha_header_v, sub_spec_l$header))
    if (length(candidatos_n) > 1 && !is.null(sub_spec_l$after)) {
      pos_after_n <- which(cell_eq(linha_header_v, sub_spec_l$after))
      if (length(pos_after_n) > 0) {
        candidatos_n <- candidatos_n[candidatos_n > min(pos_after_n)]
      }
    }
    if (length(candidatos_n) > 1 && !is.null(sub_spec_l$before)) {
      pos_before_n <- which(cell_eq(linha_header_v, sub_spec_l$before))
      if (length(pos_before_n) > 0) {
        candidatos_n <- candidatos_n[candidatos_n < max(pos_before_n)]
      }
    }
    if (length(candidatos_n) > 1 && !is.null(sub_spec_l$under)) {
      bate_v <- vapply(candidatos_n, function(col_n) {
        any(vapply(
          linhas_acima_l,
          function(linha_v) cell_eq(linha_v[col_n], sub_spec_l$under),
          logical(1)
        ))
      }, logical(1))
      candidatos_n <- candidatos_n[bate_v]
    }
    # Prefer unclaimed candidates, but fall back to any candidate (so a
    # single shared column - e.g. when only "Statera" exists - can still
    # be picked up at least once).
    nao_claimed_n <- setdiff(candidatos_n, excluir_n)
    if (length(nao_claimed_n) > 0) {
      nao_claimed_n[1]
    } else if (length(candidatos_n) == 0) {
      NA_integer_
    } else {
      candidatos_n[1]
    }
  }

  # Helper: read a single data column as numeric (length = number of data rows)
  ler_coluna <- function(col_n) {
    suppressWarnings(
      as.numeric(unlist(fluxo.bruto_t[inicio_n:fim_n, col_n]))
    )
  }

  # Extract one long tibble per variable, then bind them all together.
  purrr::map_dfr(names(variaveis_l), function(nome_var_c) {
    spec_l <- variaveis_l[[nome_var_c]]

    # Optional empreendimento-scoped variables: skip when the file's
    # empreendimento doesn't match any of the spec's patterns (the field
    # may be a single regex or a character vector).
    if (!is.null(spec_l$empreendimento) &&
      !any(str_detect(empreendimento_c, spec_l$empreendimento))) {
      return(vazio_t)
    }

    # Single-column variable vs. multi-column (sum) variable.
    if (!is.null(spec_l$components)) {
      # Claim columns one component at a time, excluding columns already
      # picked by earlier components so overlapping alternations resolve
      # to distinct columns when the spreadsheet has both names.
      cols_n <- integer(0)
      for (sub_l in spec_l$components) {
        c_n <- achar_coluna(sub_l, excluir_n = cols_n)
        if (!is.na(c_n)) cols_n <- c(cols_n, c_n)
      }
      cols_n <- unique(cols_n)
      if (length(cols_n) == 0) {
        return(vazio_t)
      }
      # Row-wise sum across all found component columns; NAs treated as 0
      # so a missing month in one component doesn't blank out the total.
      valor_m <- vapply(cols_n, ler_coluna, numeric(fim_n - inicio_n + 1))
      valor_v <- rowSums(valor_m, na.rm = TRUE)
    } else {
      col_var_n <- achar_coluna(spec_l)
      if (is.na(col_var_n)) {
        return(vazio_t)
      }
      valor_v <- ler_coluna(col_var_n)
    }

    tibble(
      empreendimento = empreendimento_c,
      variavel = nome_var_c,
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
    # Round to 2 decimals for monetary variables; "Unidades vendidas" is
    # an integer count, so round to 0 decimals.
    dplyr::mutate(valor = dplyr::if_else(
      variavel == "Unidades vendidas",
      round(valor, 0),
      round(valor, 2)
    )) %>%
    dplyr::ungroup()
}

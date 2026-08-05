#' @title Fechamento otimizado
#'
#' @description
#' Versao otimizada de \code{\link{r_fechamento0}} que produz resultados
#' identicos, porem executa significativamente mais rapido gracas ao
#' carregamento paralelo de dados e eliminacao de chamadas duplicadas.
#'
#' @details
#' Otimizacoes aplicadas:
#' \enumerate{
#'   \item \strong{Paralelismo (Fase 1)}: Nove fontes de dados independentes
#'     sao carregadas simultaneamente via \code{future::multisession}.
#'   \item \strong{Paralelismo com cache (Fase 2)}: Funcoes que dependem de
#'     dados ja carregados (\code{e_cef_cmfcns} e \code{e_ik_car}) rodam em
#'     paralelo, com sub-dependencias injetadas nos workers.
#'   \item \strong{Cache via environment (Fase 3)}: \code{r_xcef()} roda com
#'     uma copia cujo environment contem as sub-dependencias em cache,
#'     evitando re-leitura de PDFs sem alterar o namespace do pacote.
#'   \item \strong{rowSums vetorizado}: Substituicao de
#'     \code{rowwise() + c_across()} por \code{rowSums(pick(...))}.
#'   \item \strong{Eliminacao de duplicatas}: A chamada duplicada a
#'     \code{e_cef_cmfcns()} dentro de \code{gerar_xlsx()} foi removida.
#' }
#'
#' @param xlsx Logico. Se \code{TRUE}, gera arquivo Excel (.xlsx) com os
#'   dados no template de fechamento. Padrao: \code{FALSE}.
#'
#' @param data Opcional. String no formato \code{"AAAA_MM_DD"} (ex.:
#'   \code{"2026_12_31"}) que ativa o cache de inputs. Quando informada,
#'   usa a subpasta correspondente em
#'   \code{caminhos_pastas("fechamento_in")}: se existir
#'   \code{inputs-AAAA_MM_DD.rds}, carrega os inputs do arquivo (pulando a
#'   extracao completa); caso contrario, executa a extracao normalmente e
#'   grava \code{inputs-AAAA_MM_DD.rds} e \code{inputs-AAAA_MM_DD.xlsx} na
#'   pasta. A data (convertida para \code{Date}) tambem e gravada em
#'   \code{inputs!A1} das planilhas geradas. Padrao: \code{NULL} (sem cache).
#'
#' @return Lista identica a retornada por \code{\link{r_fechamento0}}.
#'
#' @seealso \code{\link{r_fechamento0}}
#'
#' @examples
#' \dontrun{
#' resultado <- r_fechamento()
#' resultado <- r_fechamento(xlsx = TRUE)
#' }
#'
#' @importFrom future future value plan multisession
#' @importFrom dplyr rename mutate select filter group_by summarise left_join
#'   full_join bind_rows distinct arrange across slice_max ungroup any_of
#'   everything first coalesce pick
#' @importFrom tidyr complete pivot_wider nesting
#' @importFrom tidyselect where
#' @importFrom stringr str_remove_all str_length str_sub str_detect str_c
#'   str_replace word
#' @importFrom lubridate floor_date ymd
#' @importFrom magrittr %>% %<>%
#' @importFrom openxlsx2 wb_load wb_to_df
#' @importFrom tibble tibble
#' @importFrom rlang .data sym
#'
#' @export
r_fechamento <- function(xlsx = FALSE, data = NULL) {
  t0 <- Sys.time()
  msg <- function(txt) {
    d <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    message(sprintf("[%5.1fs] %s", d, txt))
  }

  # ── Configuracao de cache ──────────────────────────────────────────────────
  usar_cache <- !is.null(data)
  data_dt <- NULL
  if (usar_cache) {
    if (!is.character(data) || length(data) != 1 ||
      !grepl("^\\d{4}_\\d{2}_\\d{2}$", data)) {
      stop(
        "'data' deve ser uma string no formato 'AAAA_MM_DD' ",
        "(ex.: '2026_12_31')."
      )
    }
    data_dt <- as.Date(gsub("_", "-", data))
    pasta_cache_c <- file.path(caminhos_pastas("fechamento_in"), data)
    arquivo_rds_c <- file.path(
      pasta_cache_c, sprintf("inputs-%s.rds", data)
    )
  }

  # Inputs obrigatorios que devem existir (e nao estar vazios) no cache
  inputs_obrigatorios_c <- c(
    "estq", "ecns", "contrs", "cr", "desp", "unis",
    "xcefs", "empr", "cmfcns", "car", "cmfcn.xcef"
  )
  input_vazio <- function(x) {
    is.null(x) ||
      (is.data.frame(x) && nrow(x) == 0) ||
      (is.list(x) && !is.data.frame(x) && length(x) == 0)
  }

  # Extracao completa (Fases 1-3). Retorna lista nomeada de inputs brutos.
  extrair_inputs <- function() {
    # ── Fase 1: carregamento de dados independentes em paralelo ────────────────
    msg("Fase 1: Carregando dados independentes em paralelo...")
    .rprofile_caminho <- normalizePath(
      here::here(".Rprofile"),
      winslash = "/", mustWork = FALSE
    )
    cl <- parallelly::makeClusterPSOCK(
      parallelly::availableCores(),
      rscript_args = "--no-init-file",
      rscript_startup = bquote({
        invisible(capture.output(suppressMessages(suppressWarnings(
          source(.(.rprofile_caminho), local = TRUE)
        )), type = "output"))
      })
    )
    planoAnterior <- future::plan(future::cluster, workers = cl)
    on.exit(
      {
        future::plan(planoAnterior)
        parallel::stopCluster(cl)
      },
      add = TRUE
    )

    fut_estq <- future::future(
      {
        e_ana_estq()
      },
      seed = TRUE
    )
    fut_ecns <- future::future(
      {
        e_cef_ecns()
      },
      seed = TRUE
    )
    fut_contrs <- future::future(
      {
        e_ik_contrs()
      },
      seed = TRUE
    )
    fut_cr <- future::future(
      {
        e_ik_cr()
      },
      seed = TRUE
    )
    fut_desp <- future::future(
      {
        e_ik_desp()
      },
      seed = TRUE
    )
    fut_unis <- future::future(
      {
        e_ik_unis()
      },
      seed = TRUE
    )
    fut_xcefs <- future::future(
      {
        e_cef_xcefs()
      },
      seed = TRUE
    )
    fut_eprs <- future::future(
      {
        e_cef_eprs()
      },
      seed = TRUE
    )
    fut_nplpjs <- future::future(
      {
        e_cef_nplpjs()
      },
      seed = TRUE
    )
    fut_empr <- future::future(
      {
        e_ik_empr()
      },
      seed = TRUE
    )

    in.estq <- future::value(fut_estq)
    .cache_ecns <- future::value(fut_ecns)
    .cache_contrs <- future::value(fut_contrs)
    .cache_cr <- future::value(fut_cr)
    in.desp <- future::value(fut_desp)
    .cache_unis <- future::value(fut_unis)
    .cache_xcefs <- future::value(fut_xcefs)
    .cache_eprs <- future::value(fut_eprs)
    .cache_nplpjs <- future::value(fut_nplpjs)
    in.empr <- future::value(fut_empr)

    msg("Fase 1 concluida.")

    # ── Fase 2: carregamento de dados dependentes em paralelo ──────────────────
    msg("Fase 2: Carregando dados dependentes em paralelo...")

    # e_cef_cmfcns() chama e_cef_ecns() internamente — injetar cache no worker
    fut_cmfcns <- future::future(
      {
        ns <- asNamespace("amplaRPackage")
        tryCatch(unlockBinding("e_cef_ecns", ns), error = function(e) NULL)
        assign("e_cef_ecns", function(...) .inj_ecns, envir = ns)
        e_cef_cmfcns()
      },
      globals = list(.inj_ecns = .cache_ecns),
      seed = TRUE
    )

    # e_ik_car() chama e_ik_contrs() internamente — injetar cache no worker
    fut_car <- future::future(
      {
        ns <- asNamespace("amplaRPackage")
        tryCatch(unlockBinding("e_ik_contrs", ns), error = function(e) NULL)
        assign("e_ik_contrs", function(...) .inj_contrs, envir = ns)
        e_ik_car()
      },
      globals = list(.inj_contrs = .cache_contrs),
      seed = TRUE
    )

    .cache_cmfcns <- future::value(fut_cmfcns)
    .cache_car <- future::value(fut_car)

    msg("Fase 2 concluida.")

    # ── Fase 3: r_xcef() com todas sub-dependencias em cache ──────────────────
    # Usa override de environment para que r_xcef resolva as funcoes pesadas
    # a partir do cache, sem modificar o namespace do pacote.
    msg("Fase 3: Cruzamento CEF (r_xcef)...")

    r_xcef_fn <- get("r_xcef", envir = asNamespace("amplaRPackage"))
    envCache <- new.env(parent = environment(r_xcef_fn))
    envCache$e_cef_eprs <- function(...) .cache_eprs
    envCache$e_cef_cmfcns <- function(...) .cache_cmfcns
    envCache$e_cef_xcefs <- function(...) .cache_xcefs
    envCache$e_cef_nplpjs <- function(...) .cache_nplpjs

    r_xcef_com_cache <- r_xcef_fn
    environment(r_xcef_com_cache) <- envCache

    in.cmfcn.xcef_bruto <- r_xcef_com_cache()

    msg("Fase 3 concluida.")

    # Consolidar inputs brutos (nomes usados no cache e na validacao)
    list(
      estq       = in.estq,
      ecns       = .cache_ecns,
      contrs     = .cache_contrs,
      cr         = .cache_cr,
      desp       = in.desp,
      unis       = .cache_unis,
      xcefs      = .cache_xcefs,
      empr       = in.empr,
      cmfcns     = .cache_cmfcns,
      car        = .cache_car,
      cmfcn.xcef = in.cmfcn.xcef_bruto
    )
  }

  # ── Obter inputs: do cache (.rds) ou via extracao completa ─────────────────
  carregou_cache <- usar_cache && file.exists(arquivo_rds_c)
  if (carregou_cache) {
    msg(sprintf("Carregando inputs do cache: %s", basename(arquivo_rds_c)))
    inputs_l <- readRDS(arquivo_rds_c)
    msg("Inputs carregados do cache.")
  } else {
    inputs_l <- extrair_inputs()
  }

  # Validar presenca de todos os inputs obrigatorios
  faltantes_c <- inputs_obrigatorios_c[
    !inputs_obrigatorios_c %in% names(inputs_l) |
      vapply(
        inputs_obrigatorios_c,
        function(nm) input_vazio(inputs_l[[nm]]),
        logical(1)
      )
  ]
  if (length(faltantes_c) > 0) {
    warning(
      "Nem todos os inputs necessarios foram identificados. ",
      "Faltando ou vazio: ", paste(faltantes_c, collapse = ", "), "."
    )
  }

  # Gravar cache (.rds + .xlsx) quando 'data' foi informada e extraimos agora
  if (usar_cache && !carregou_cache) {
    msg(sprintf("Gravando cache em: %s", pasta_cache_c))
    dir.create(pasta_cache_c, showWarnings = FALSE, recursive = TRUE)
    saveRDS(inputs_l, file = arquivo_rds_c)

    # Backup xlsx: uma aba por input (apenas data.frames) + data em inputs!A1
    inputs_xlsx_l <- Filter(is.data.frame, c(
      list(estq = inputs_l$estq),
      inputs_l$ecns,
      list(
        contrs = inputs_l$contrs,
        cr     = inputs_l$cr,
        desp   = inputs_l$desp,
        unis   = inputs_l$unis,
        xcefs  = inputs_l$xcefs,
        empr   = inputs_l$empr,
        cmfcns = inputs_l$cmfcns,
        car    = inputs_l$car$car
      ),
      list(cmfcn.xcef = inputs_l[["cmfcn.xcef"]])
    ))
    # Ordenar abas pelo nome para facilitar a navegacao no Excel
    inputs_xlsx_l <- inputs_xlsx_l[order(names(inputs_xlsx_l))]
    gerar_xlsx(
      data = inputs_xlsx_l,
      save = list(sprintf("inputs-%s.xlsx", data), pasta_cache_c)
    )
    msg("Cache (.rds + .xlsx) gravado.")
  }

  # Desempacotar inputs para as variaveis usadas na Fase 4
  in.estq <- inputs_l$estq
  .cache_ecns <- inputs_l$ecns
  .cache_contrs <- inputs_l$contrs
  .cache_cr <- inputs_l$cr
  in.desp <- inputs_l$desp
  .cache_unis <- inputs_l$unis
  .cache_xcefs <- inputs_l$xcefs
  in.empr <- inputs_l$empr
  .cache_cmfcns <- inputs_l$cmfcns
  .cache_car <- inputs_l$car
  in.cmfcn.xcef_bruto <- inputs_l[["cmfcn.xcef"]]

  # ── Fase 4: processamento (logica identica ao r_fechamento0) ───────────────
  msg("Fase 4: Processando dados...")

  # Estoque (Ana)
  # in.estq ja carregado na Fase 1

  # CMF_CN
  in.cmfcns <- .cache_cmfcns

  # ECNs
  in.ecns <- .cache_ecns$ecn_u %>%
    rename(
      contrato.cef = contrato,
      repasse.cef.fin = financiamento,
      repasse.cef.desc.subs = desconto.subsidio,
      repasse.cef.fgts = fgts,
      repasse.cef.rec.prop = recursos.proprios,
      repasse.cef.obra.acum = valor.liberado.obra,
      repasse.cef.terreno.acum = valor.liberado.terreno
    ) %>%
    mutate(
      contrato.cef = str_remove_all(contrato.cef, "-") %>%
        if_else(str_length(.) == 13, str_sub(., 1, 12), .),
      contrato.cef.5 = str_sub(contrato.cef, -5, -1),
      repasse.cef.total = round(repasse.cef.fin + repasse.cef.desc.subs + repasse.cef.fgts + repasse.cef.rec.prop, 2),
      repasse.cef.incorrido = round(repasse.cef.terreno.acum + repasse.cef.obra.acum, 2),
      repasse.cef.a.incorrer = round(repasse.cef.total - repasse.cef.incorrido, 2)
    ) %>%
    dplyr::select(
      empresa, contrato.cef.5, repasse.cef.total, repasse.cef.incorrido,
      repasse.cef.a.incorrer, repasse.cef.fin, repasse.cef.desc.subs,
      repasse.cef.fgts, repasse.cef.rec.prop, repasse.cef.terreno.acum,
      repasse.cef.obra.acum, arquivo
    )

  # Contratos
  in.contr <-
    .cache_contrs %>%
    rename(contrato = contrato.ampla) %>%
    dplyr::filter(sit %in% c("A", "L", "R")) %>%
    dplyr::filter(!is.na(empresa))

  # Contas recebidas
  in.cr <- .cache_cr %>%
    rename(
      data.vencimento = vencimento,
      edificacao = torre,
      ele = elemento,
      r.f = `r/f`,
      unidade = apto
    ) %>%
    mutate(
      empreendimento = word(empreendimento),
      especie = if_else(str_detect(edificacao, "^(?i)vaga"),
        "Garagem",
        "Apartamento"
      ),
      contrato.cef = NA_character_,
      data.emissao = NA_Date_,
      disp = NA_character_,
      esp.con = NA_character_,
      juros.contrato = NA_real_,
      pavimento = NA_character_,
      repassado = NA_character_
    ) %>%
    dplyr::select(
      empreendimento, empresa, total, data.vencimento, data.pagamento, cliente,
      contrato, contrato.cef, repassado, ele, esp, esp.con, agente, parcela,
      principal, juros, juros.contrato, juros.mora, reajuste, encargos, multa,
      seguro, desconto, cart, r.f, edificacao, especie, unidade, data.emissao,
      disp, pavimento, arquivo, arquivo.tipo, arquivo.tabela.tipo, arquivo.fonte
    )

  # Despesas
  # in.desp ja carregado na Fase 1
  in.desp <- in.desp %>%
    mutate(categoria = NA_character_) %>%
    select(
      nucleo, nucleo.num, empresa, centro.negocio, categoria,
      n.siban, origem, tipo.entrada, documento, parcela,
      data.vencimento, data.pagamento, valor, `a/c`,
      documento.pagto, credor, classe, assunto.titulo,
      grupo.titulo, subgrupo.titulo, classificacao, `d/c`,
      cod.grupo.nuc, grupo.nucleo, cod.grupo.cen, grupo.centro,
      cod.classe.cen, classe.centro, arquivo, arquivo.tabela.tipo,
      arquivo.tipo, arquivo.fonte
    )

  # Contas a receber
  in.car <- .cache_car$car %>%
    rename(
      data.emissao = emissao,
      seguro = seguros,
      total = valor.atualizado
    ) %>%
    mutate(
      data.pagamento = NA_Date_,
      juros.mora = NA_real_,
      desconto = NA_real_,
      r.f = NA_character_,
      edificacao = NA_character_
    ) %>%
    dplyr::filter(
      empresa %in% c("AMP", "AVS", "CBL", "GRA", "LUC", "POM", "SAU", "SN2", "SN4")
    ) %>%
    dplyr::select(
      empreendimento, empresa, total, data.vencimento, data.pagamento, cliente,
      contrato, contrato.cef, repassado, ele, esp, esp.con, agente, parcela,
      principal, juros, juros.contrato, juros.mora, reajuste, encargos, multa,
      seguro, desconto, cart, r.f, edificacao, especie, unidade, data.emissao,
      disp, pavimento, arquivo, arquivo.tipo, arquivo.tabela.tipo, arquivo.fonte
    )

  # Receitas: cr + car
  in.rec <- bind_rows(in.cr, in.car) %>%
    dplyr::filter(empreendimento != "AMP.01.0001") %>%
    mutate(
      cruzada = TRUE,
      data.base = coalesce(data.pagamento, data.vencimento),
      especie = if_else(
        str_detect(especie, "(?i)garagens"), "Garagem", especie
      ),
      id = str_c(empresa, especie, unidade, sep = "-"),
      natureza = case_when(
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("CBL", "POM", "SAU") &
          repassado == "Não" ~ "parcela.cef.assinar",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("CBL", "POM", "SAU") ~ "parcela.cef.total.ik",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          empresa %in% c("CBL", "POM", "SAU") ~ "parcela.fin.total.ik",
        ele == "TAX" ~ "taxa.extra",
        TRUE ~ "pro.soluto"
      )
    ) %>%
    select(
      id, empresa, especie, unidade, total, data.vencimento, data.pagamento,
      data.base, natureza, everything()
    )

  # Unidades
  in.unis <- .cache_unis %>%
    rename(unidade = numero) %>%
    mutate(
      data = as.Date(data),
      empresa = str_sub(empreendimento, 1, 3),
      especie = case_when(
        str_detect(unidade, "(?i)moto") ~ "Moto",
        str_detect(especie, "(?i)garagens") ~ "Garagem",
        TRUE ~ especie
      ),
      unidade = str_remove_all(unidade, "[^\\d]*") %>% as.integer(),
      id = str_c(empresa, especie, unidade, sep = "-")
    )

  # Unidades consolidadas (Informakon + Ana Estoque)
  in.unis.cruzado <- full_join(
    in.unis, in.estq,
    by = "id", suffix = c(".ik", ".ana")
  ) %>%
    rename(
      status.ik = situacao,
      status.ana = Status
    ) %>%
    mutate(
      # Prioridade do status Informakon (menor = mais avancado no funil)
      status.p.ik = case_when(
        status.ik == "Permutado Terreno" ~ "1",
        status.ik == "Reserva Técnica" ~ "2",
        status.ik %in% c("Em Negociação", "Disponível") ~ "3",
        status.ik == "Vendido" ~ "4",
        is.na(status.ik) | status.ik == "" ~ "",
        TRUE ~ NA_character_
      ),
      # Prioridade do status Ana Estoque (mesma escala de status.p.ik)
      status.p.ana = case_when(
        status.ana == "Permuta" ~ "1",
        status.ana %in% c("Fora de venda", "Venda suspensa") ~ "2",
        status.ana %in% c("Reservada", "Em Negociação", "Disponível") ~ "3",
        status.ana == "Venda aprovada" ~ "4",
        is.na(status.ana) | status.ana == "" ~ "",
        TRUE ~ NA_character_
      ),
      # Sinaliza divergencia de prioridade entre as duas fontes
      checar = !is.na(status.p.ik) & !is.na(status.p.ana) &
        status.p.ik != "" & status.p.ana != "" &
        status.p.ik != status.p.ana
    ) %>%
    select(
      status.ik, status.ana, status.p.ik, status.p.ana, checar,
      everything()
    )

  # Extratos da CEF
  in.xcef <- .cache_xcefs

  # Extratos CEF cruzados com CMF_CN
  in.cmfcn.xcef <- in.cmfcn.xcef_bruto %>%
    rename(natureza = natureza.cmfcn) %>%
    mutate(
      data.movimentacao = coalesce(
        data.movimentacao.extrato,
        data.movimentacao.cmfcn
      )
    )

  # Determinar sequencia completa de meses para todos os tibbles CEF
  meses.cef <- c(
    floor_date(in.cmfcns$data.movimento, "month"),
    floor_date(in.xcef$data.movimentacao, "month"),
    floor_date(in.cmfcn.xcef$data.movimentacao, "month")
  ) %>%
    .[!is.na(.)] %>%
    {
      seq(min(.), max(.), by = "month")
    }

  # Extratos da CEF mensalizados por contrato
  in.xcef.mensal <- in.xcef %>%
    dplyr::filter(
      !is.na(natureza) &
        contrato.5 %in% in.ecns$contrato.cef.5
    ) %>%
    mutate(mes = floor_date(data.movimentacao, "month")) %>%
    group_by(empresa, contrato.5, natureza, mes) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    complete(empresa, contrato.5, natureza, mes = meses.cef, fill = list(valor = 0)) %>%
    pivot_wider(
      names_from = mes,
      values_from = valor,
      values_fill = 0
    ) %>%
    mutate(
      total = rowSums(pick(where(is.numeric)), na.rm = TRUE)
    ) %>%
    rename(contrato.cef.5 = contrato.5) %>%
    select(
      empresa, contrato.cef.5, natureza, total,
      any_of(as.character(sort(meses.cef)))
    )

  # Extratos CEF cruzados com CMF_CN mensalizados por contrato
  in.cmfcn.xcef.mensal <- in.cmfcn.xcef %>%
    mutate(mes = floor_date(data.movimentacao, "month")) %>%
    group_by(empresa, contrato.5, natureza, mes) %>%
    summarise(valor = sum(valor.cmfcn, na.rm = TRUE), .groups = "drop") %>%
    complete(empresa, contrato.5, natureza, mes = meses.cef, fill = list(valor = 0)) %>%
    pivot_wider(
      names_from = mes,
      values_from = valor,
      values_fill = 0
    ) %>%
    mutate(
      total = rowSums(pick(where(is.numeric)), na.rm = TRUE)
    ) %>%
    rename(contrato.cef.5 = contrato.5) %>%
    select(
      empresa, contrato.cef.5, natureza, total,
      any_of(as.character(sort(meses.cef)))
    ) %>%
    mutate(natureza = str_c(natureza, " (cruzado)"))

  # CMF_CN mensalizado (ajustado para mesma estrutura)
  in.cmfcns.mensal <- in.cmfcns %>%
    dplyr::filter(!is.na(valor)) %>%
    mutate(mes = floor_date(data.movimento, "month")) %>%
    group_by(empresa, contrato, natureza, mes) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    complete(
      empresa, contrato, natureza,
      mes = meses.cef, fill = list(valor = 0)
    ) %>%
    pivot_wider(
      names_from = mes,
      values_from = valor,
      values_fill = 0
    ) %>%
    mutate(
      total = rowSums(pick(where(is.numeric)), na.rm = TRUE)
    ) %>%
    mutate(contrato.cef.5 = str_sub(contrato, -5)) %>%
    select(
      empresa, contrato.cef.5, natureza, total,
      any_of(as.character(sort(meses.cef)))
    ) %>%
    dplyr::filter(total > 1e-3)

  # cef: cmfcns + ecns
  in.cef <- in.ecns %>%
    left_join(
      in.cmfcns.mensal %>%
        group_by(empresa, contrato.cef.5, natureza) %>%
        summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = natureza,
          values_from = total,
          values_fill = 0
        ),
      by = "contrato.cef.5",
      suffix = c(".ecns", ".cmfcns")
    ) %>%
    mutate(
      cef.obra = if_else(
        (abs(repasse.cef.obra.acum - repasse.cef.obra) < 1e-3) &
          !is.na(repasse.cef.obra),
        TRUE,
        FALSE
      ),
      cef.terreno = if_else(
        (abs(repasse.cef.terreno.acum - repasse.cef.terreno) < 1e-3) &
          !is.na(repasse.cef.terreno),
        TRUE,
        FALSE
      )
    ) %>%
    select(
      contrato.cef.5, repasse.cef.total, repasse.cef.incorrido,
      repasse.cef.a.incorrer, repasse.cef.fin, repasse.cef.desc.subs,
      repasse.cef.fgts, repasse.cef.rec.prop, repasse.cef.terreno.acum,
      repasse.cef.terreno, cef.terreno, repasse.cef.obra.acum,
      repasse.cef.obra, cef.obra, amortizacao.pj, remuneracao.terreno,
      remuneracao.venda
    )

  # Consolidar todos em in.cef.mensal
  in.cef.mensal <- bind_rows(
    in.cmfcns.mensal,
    in.xcef.mensal,
    in.cmfcn.xcef.mensal
  ) %>%
    dplyr::filter(total > 1e-3)

  # Totais por natureza que devem virar colunas
  totais <- in.rec %>%
    dplyr::filter(natureza %in% c("parcela.cef.total.ik", "parcela.cef.assinar", "taxa.extra")) %>%
    group_by(id, natureza) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = natureza,
      values_from = total,
      values_fill = 0
    )

  rec.uni <- in.unis %>%
    left_join(
      in.rec,
      by = "id",
      suffix = c(".unis", ".rec")
    ) %>%
    # Adicionar contrato.cef via in.contr
    left_join(
      in.contr %>% select(empresa, contrato, contrato.cef),
      by = c("empresa.unis" = "empresa", "contrato" = "contrato"),
      suffix = c("", ".contr")
    ) %>%
    # Consolidar colunas e identificar origem dos dados
    mutate(
      empresa = coalesce(empresa.unis, empresa.rec),
      especie = coalesce(especie.unis, especie.rec),
      pavimento = coalesce(pavimento.unis, pavimento.rec),
      unidade = coalesce(unidade.unis, unidade.rec),
      contrato.cef = coalesce(contrato.cef.contr, contrato.cef),
      data.mes = floor_date(data.base, "month"),
      cruzada = case_when(
        is.na(empresa.rec) ~ "in.unis",
        is.na(empresa.unis) ~ "in.rec",
        TRUE ~ "ambos"
      )
    ) %>%
    # Agregar por id, mes e natureza
    group_by(id, data.mes, natureza) %>%
    summarise(
      empresa = first(empresa),
      especie = first(especie),
      pavimento = first(pavimento),
      unidade = first(unidade),
      cliente = first(cliente.unis),
      data.venda = first(data),
      situacao = first(situacao),
      valor.venda = first(valor.venda),
      contrato = first(contrato),
      contrato.cef = first(contrato.cef),
      total = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Pivotar meses para colunas
    pivot_wider(
      names_from = data.mes,
      values_from = total,
      values_fill = 0,
      values_fn = sum
    ) %>%
    # Normalizar contrato.cef (13 -> 12 caracteres)
    mutate(
      contrato.cef = if_else(
        str_length(contrato.cef) == 13,
        str_sub(contrato.cef, 1, 12),
        contrato.cef
      ),
      contrato.cef.5 = str_sub(contrato.cef, -5, -1)
    ) %>%
    # Adicionar dados ECN
    left_join(in.ecns, by = c("empresa", "contrato.cef.5")) %>%
    mutate(
      checar = !is.na(contrato.cef.5) & is.na(repasse.cef.total),
      contrato.comeco = str_sub(contrato, 1, 4),
      contrato.fim = str_sub(contrato, -1) %>% as.integer()
    ) %>%
    # Somar valores mensais por (id, natureza, contrato.comeco) e manter contrato mais recente
    group_by(id, natureza, contrato.comeco) %>%
    mutate(across(matches("^\\d{4}-\\d{2}-\\d{2}$"), ~ sum(.x, na.rm = TRUE))) %>%
    slice_max(contrato.fim, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Priorizar contrato com maior soma mensal
    mutate(soma.meses = rowSums(across(matches("^\\d{4}-\\d{2}-\\d{2}$")), na.rm = TRUE)) %>%
    group_by(id, natureza) %>%
    slice_max(soma.meses, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Organizar colunas com meses em ordem cronologica
    select(
      id, empresa, especie, unidade, cliente, contrato, contrato.cef, pavimento,
      situacao, data.venda, valor.venda, repasse.cef.total, checar, natureza,
      soma.meses,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    ) %>%
    arrange(id, natureza)

  rec.uni.reduzido <- rec.uni %>%
    select(
      id, empresa, especie, unidade, cliente, contrato, contrato.cef,
      pavimento, situacao, data.venda, valor.venda, repasse.cef.total, checar
    ) %>%
    mutate(contrato.cef.5 = str_sub(contrato.cef, -5, -1)) %>%
    distinct()

  in.cef.detalhado <- left_join(
    in.cef.mensal,
    rec.uni.reduzido,
    by = c("empresa", "contrato.cef.5")
  ) %>%
    dplyr::filter(!is.na(id)) %>%
    select(
      id, empresa, especie, unidade, cliente, contrato, contrato.cef, pavimento,
      situacao, data.venda, valor.venda, repasse.cef.total, checar, natureza,
      total,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    ) %>%
    arrange(empresa, contrato, natureza) %>%
    rename(soma.meses = total)

  in.unis.cruzado.join <- in.unis.cruzado

  colunas.preco.tabela_c <- intersect(
    c("Preço Tab.", "Tab. Preço", "Tab. Preço Atual", "tab.preco"),
    names(in.unis.cruzado.join)
  )
  if (length(colunas.preco.tabela_c) > 0) {
    in.unis.cruzado.join$preco.tabela <- suppressWarnings(
      as.numeric(in.unis.cruzado.join[[colunas.preco.tabela_c[[1]]]])
    )
  } else {
    in.unis.cruzado.join$preco.tabela <- NA_real_
  }

  colunas.situacao.ana_c <- intersect(
    c("status.ana", "Status", "situacao.ana", "situacao"),
    names(in.unis.cruzado.join)
  )
  if (length(colunas.situacao.ana_c) > 0) {
    in.unis.cruzado.join$situacao.ana <-
      as.character(in.unis.cruzado.join[[colunas.situacao.ana_c[[1]]]])
  } else {
    in.unis.cruzado.join$situacao.ana <- NA_character_
  }

  rec.uni %<>% bind_rows(
    in.cef.detalhado %>%
      arrange(empresa, id, natureza)
  ) %>%
    # Adicionar colunas do R9 (Ana)
    left_join(
      in.unis.cruzado.join %>%
        select(id, situacao.ana, valor.venda.ana, preco.tabela) %>%
        distinct(),
      by = "id"
    ) %>%
    rename(
      situacao.ik = situacao,
      valor.venda.ik = valor.venda
    ) %>%
    # Reordenar colunas para garantir que meses estejam em ordem cronologica
    select(
      id, empresa, especie, unidade, cliente, contrato, contrato.cef, pavimento,
      situacao.ana, situacao.ik, data.venda, preco.tabela, valor.venda.ana,
      valor.venda.ik, repasse.cef.total, checar, natureza, soma.meses,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    )

  # ── Tabela `m.vb`: indicadores agregados por mes/empresa ─────────
  # Substitui formulas pesadas que viviam em Template-Fechamento.xlsx.
  # Para preservar `arquivo`, puxamos das fontes ORIGINAIS (cmfcns/rec/desp)
  # em vez de rec.uni, cujo pivot_wider descarta `arquivo`.

  # Lookup centro.negocio -> categoria, lido da named range `tb_ik_viab`
  # do Template-Fechamento.xlsx.
  caminho_template_c <- file.path(
    caminhos_pastas("templates"), "Template-Fechamento.xlsx"
  )
  wb_template <- openxlsx2::wb_load(caminho_template_c)
  ikViabBruto <- openxlsx2::wb_to_df(
    wb_template,
    named_region = "tb_ik_viab",
    col_names    = FALSE
  )
  categoria_lookup <- tibble::tibble(
    centro.negocio = as.character(ikViabBruto[[1]]),
    categoria      = as.character(ikViabBruto[[2]])
  ) %>%
    dplyr::filter(!is.na(centro.negocio), nzchar(centro.negocio)) %>%
    dplyr::distinct(centro.negocio, .keep_all = TRUE)

  # Pares (nucleo, empresa) destacados, lidos da named range `empresas`
  # do Template-Fechamento.xlsx. So estes alimentam a tabela `m.vb`.
  empresas_bruto <- openxlsx2::wb_to_df(wb_template, named_region = "empresas")
  destacados <- empresas_bruto %>%
    rename_with(~ c("nucleo", "empresa", "destacado")[1:length(.x)]) %>%
    dplyr::filter(isTRUE(as.logical(destacado)) | destacado == "TRUE") %>%
    dplyr::transmute(
      nucleo  = as.character(nucleo),
      empresa = as.character(empresa)
    ) %>%
    dplyr::filter(!is.na(empresa), nzchar(empresa))

  # Aplica o lookup nas despesas (sobrescreve a coluna `categoria` vazia
  # que vem de e_ik_desp).
  in.desp.cat <- in.desp %>%
    select(-any_of("categoria")) %>%
    left_join(categoria_lookup, by = "centro.negocio")

  # Lookup empresa -> nucleo, derivado de in.empr (fonte oficial de
  # empreendimentos). Em caso de divergencia, a primeira ocorrencia
  # prevalece. Fallback para in.desp se in.empr nao trouxer o par.
  empresa_nucleo_lookup <- dplyr::bind_rows(
    in.empr %>%
      dplyr::filter(!is.na(empresa), !is.na(nucleo), nzchar(empresa)) %>%
      dplyr::distinct(empresa, nucleo),
    in.desp %>%
      dplyr::filter(!is.na(empresa), !is.na(nucleo), nzchar(empresa)) %>%
      dplyr::distinct(empresa, nucleo)
  ) %>%
    dplyr::group_by(empresa) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Lookup reverso nucleo -> empresa, restringido aos pares destacados
  # do Template-Fechamento. Como um nucleo pode ter varias empresas,
  # o template define qual empresa "principal" representa o nucleo na
  # tabela `m.vb`. Fallback: primeira empresa do nucleo em in.empr.
  nucleo_empresa_lookup <- dplyr::bind_rows(
    destacados %>%
      dplyr::filter(!is.na(nucleo), !is.na(empresa), nzchar(nucleo)) %>%
      dplyr::distinct(nucleo, empresa),
    in.empr %>%
      dplyr::filter(!is.na(empresa), !is.na(nucleo), nzchar(nucleo)) %>%
      dplyr::distinct(nucleo, empresa)
  ) %>%
    dplyr::group_by(nucleo) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(empresa.lookup = empresa)

  # Agregador comum: agrega por (mes, <grupo>), somando valores e
  # concatenando `arquivo` distintos por "; ". Garante que ambas as
  # colunas (empresa, nucleo) estejam presentes no resultado:
  #   - grupo = "empresa": nucleo e resolvido via lookup.
  #   - grupo = "nucleo" : empresa fica NA (grao = nucleo).
  agregar_mes <- function(dados_t, coluna_data, coluna_valor,
                          variavel_c, fonte_c, grupo_c) {
    grupo_sym <- rlang::sym(grupo_c)

    base_t <- dados_t %>%
      mutate(
        .mes   = floor_date(.data[[coluna_data]], "month"),
        .valor = as.numeric(.data[[coluna_valor]])
      ) %>%
      dplyr::filter(!is.na(.mes), !is.na(!!grupo_sym)) %>%
      group_by(.mes, !!grupo_sym) %>%
      summarise(
        valor   = sum(.valor, na.rm = TRUE),
        arquivo = paste(sort(unique(arquivo)), collapse = "; "),
        .groups = "drop"
      ) %>%
      rename(mes = .mes)

    base_t <- if (grupo_c == "empresa") {
      base_t %>% left_join(empresa_nucleo_lookup, by = "empresa")
    } else {
      base_t %>% mutate(empresa = NA_character_)
    }

    base_t %>%
      mutate(variavel = variavel_c, fonte = fonte_c) %>%
      select(mes, empresa, nucleo, variavel, valor, fonte, arquivo)
  }

  placeholder_mes <- function(variavel_c, fonte_c) {
    tibble::tibble(
      mes      = as.Date(character()),
      empresa  = character(),
      nucleo   = character(),
      variavel = variavel_c,
      valor    = numeric(),
      fonte    = fonte_c,
      arquivo  = character()
    )
  }

  # Apara zeros das pontas (leading/trailing) e preenche meses internos
  # com 0 — produzindo uma serie temporal continua e enxuta por grupo.
  aparar_e_completar <- function(g, key) {
    if (nrow(g) == 0) {
      return(g)
    }
    g <- g %>% arrange(mes)
    nz <- which(!is.na(g$valor) & g$valor != 0)
    if (length(nz) == 0) {
      return(g[0, ])
    }
    g <- g[min(nz):max(nz), ]
    meses_v <- seq(min(g$mes), max(g$mes), by = "month")
    tibble::tibble(mes = meses_v) %>%
      dplyr::left_join(g, by = "mes") %>%
      dplyr::mutate(valor = dplyr::coalesce(valor, 0))
  }

  pos_processar_desp_mensal <- function(dados_t, filtrar_destacados = TRUE) {
    coluna_chave_c <- if ("centro.negocio" %in% names(dados_t)) {
      "centro.negocio"
    } else {
      "variavel"
    }

    tem_classe <- "classe" %in% names(dados_t)

    if (filtrar_destacados) {
      dados_t <- dados_t %>%
        # Manter apenas empresas/nucleos destacados no Template-Fechamento.
        # Linhas de grao empresa sao filtradas por empresa; de
        # grao nucleo, por nucleo.
        dplyr::filter(
          (!is.na(empresa) & empresa %in% destacados$empresa) |
            (is.na(empresa) & nucleo %in% destacados$nucleo)
        )
    }
    dados_t %>%
      # Preencher celulas vazias em empresa/nucleo cruzando com in.empr:
      # - linhas de grao nucleo ficam com empresa NA: preencher via
      #   nucleo_empresa_lookup (empresa principal do nucleo).
      # - linhas de grao empresa ja recebem nucleo via empresa_nucleo_lookup
      #   no agregar_mes(); reforcamos aqui caso algum par tenha escapado.
      dplyr::left_join(nucleo_empresa_lookup, by = "nucleo") %>%
      dplyr::mutate(
        empresa = dplyr::coalesce(empresa, empresa.lookup)
      ) %>%
      dplyr::select(-empresa.lookup) %>%
      dplyr::left_join(
        empresa_nucleo_lookup %>% dplyr::rename(nucleo.lookup = nucleo),
        by = "empresa"
      ) %>%
      dplyr::mutate(
        nucleo = dplyr::coalesce(nucleo, nucleo.lookup)
      ) %>%
      dplyr::select(-nucleo.lookup) %>%
      # Serie temporal continua por (empresa, nucleo, chave), aparada
      # nas pontas (sem leading/trailing zeros) e com gaps internos = 0.
      {
        if (tem_classe) {
          dplyr::group_by(
            ., empresa, nucleo, .data[[coluna_chave_c]], classe, fonte
          )
        } else {
          dplyr::group_by(
            ., empresa, nucleo, .data[[coluna_chave_c]], fonte
          )
        }
      } %>%
      dplyr::group_modify(aparar_e_completar) %>%
      dplyr::ungroup() %>%
      {
        if (tem_classe) {
          dplyr::select(
            ., mes, empresa, nucleo, all_of(coluna_chave_c), classe,
            valor, fonte, arquivo
          )
        } else {
          dplyr::select(
            ., mes, empresa, nucleo, all_of(coluna_chave_c), valor,
            fonte, arquivo
          )
        }
      } %>%
      dplyr::arrange(.data[[coluna_chave_c]], nucleo, empresa, mes)
  }

  in.m.vb <- bind_rows(
    in.cmfcns %>%
      dplyr::filter(natureza == "repasse.cef.obra") %>%
      agregar_mes(
        "data.movimento", "valor",
        "Repasse CEF obra", "CEF", "empresa"
      ),
    in.cmfcns %>%
      dplyr::filter(natureza == "repasse.cef.terreno") %>%
      agregar_mes(
        "data.movimento", "valor",
        "Repasse CEF terreno", "CEF", "empresa"
      ),
    in.desp.cat %>%
      dplyr::filter(str_detect(categoria, "(?i)^constru\u00e7\u00e3o$")) %>%
      agregar_mes(
        "data.pagamento", "valor",
        "Constru\u00e7\u00e3o", "Informakon", "nucleo"
      ),
    in.desp.cat %>%
      dplyr::filter(str_detect(categoria, "(?i)^despesas\\s+financeiras$")) %>%
      agregar_mes(
        "data.pagamento", "valor",
        "Despesas financeiras", "Informakon", "nucleo"
      ),
    placeholder_mes("Empr\u00e9stimo CEF PJ", "CEF"),
    in.desp.cat %>%
      dplyr::filter(str_detect(categoria, "(?i)^incorpora\u00e7\u00e3o$")) %>%
      agregar_mes(
        "data.pagamento", "valor",
        "Incorpora\u00e7\u00e3o", "Informakon", "nucleo"
      ),
    in.desp.cat %>%
      dplyr::filter(str_detect(categoria, "(?i)^novos\\s+neg[oó]cios$")) %>%
      agregar_mes(
        "data.pagamento", "valor",
        "Novos neg\u00f3cios", "Informakon", "nucleo"
      ),
    in.rec %>%
      dplyr::filter(natureza %in% c("pro.soluto", "taxa.extra")) %>%
      agregar_mes(
        "data.base", "total",
        "Pr\u00f3 soluto + Taxa extra", "Informakon", "empresa"
      ),
    placeholder_mes("Unidades vendidas", "Anapro"),
    in.desp.cat %>%
      dplyr::filter(str_detect(categoria, "(?i)^vendas$")) %>%
      agregar_mes(
        "data.pagamento", "valor",
        "Vendas", "Informakon", "nucleo"
      )
  ) %>%
    pos_processar_desp_mensal()

  in.desp.m.ik <- in.desp %>%
    dplyr::mutate(
      mes = floor_date(data.pagamento, "month"),
      centro.negocio = as.character(centro.negocio),
      classe = as.character(classe),
      valor = as.numeric(valor),
      fonte = "Informakon"
    ) %>%
    dplyr::filter(
      !is.na(mes), !is.na(nucleo),
      !is.na(centro.negocio), nzchar(centro.negocio)
    ) %>%
    dplyr::group_by(mes, nucleo, centro.negocio, classe, fonte) %>%
    dplyr::summarise(
      valor = sum(valor, na.rm = TRUE),
      arquivo = paste(sort(unique(arquivo)), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(empresa = NA_character_) %>%
    dplyr::select(
      mes, empresa, nucleo, centro.negocio, classe, valor, fonte, arquivo
    ) %>%
    pos_processar_desp_mensal(filtrar_destacados = FALSE)

  msg("Fase 4 concluida.")

  if (xlsx) {
    msg("Gerando arquivo Excel...")
    gerar_xlsx(
      data = list(
        rec = in.rec,
        rec.uni = rec.uni,
        unis.cruzado = in.unis.cruzado,
        m.vb = in.m.vb,
        desp.m.ik = in.desp.m.ik,
        contr = in.contr,
        desp = in.desp,
        ecns = in.ecns,
        empr = in.empr,
        estq = in.estq,
        unis = in.unis
      ),
      wb_load = str_c(caminhos_pastas("templates"), "/Template-Fechamento.xlsx"),
      tab_colours = c(
        rec = "darkgray",
        rec.uni = "darkgray",
        unis.cruzado = "darkgray",
        m.vb = "darkgray",
        desp.m.ik = "darkgray",
        contr = "white",
        desp = "white",
        ecns = "white",
        empr = "white",
        estq = "white",
        unis = "white"
      ),
      col_headers = list(
        rec.uni = list(
          checar = list(colour = "yellow"),
          repasse.cef.total = list(colour = "blue", font_colour = "white")
        ),
        unis.cruzado = list(
          status.p.ik = list(colour = "yellow"),
          status.p.ana = list(colour = "yellow"),
          checar = list(colour = "yellow")
        )
      ),
      col_dates = c(
        "data", "data.emissao", "data.lancamento", "data.movimentacao",
        "data.movimento", "data.pagamento", "data.venda", "data.vencimento",
        "periodo.inicio", "periodo.fim"
      ),
      col_groups = list(
        rec.uni = list(
          list(
            cols = c(
              "empresa", "especie", "unidade", "cliente", "contrato",
              "contrato.cef", "pavimento"
            ),
            hidden = TRUE
          )
        )
      ),
      tab_freeze = c(
        rec.uni = "situacao"
      ),
      col_monetary = c(
        "amortizacao.pj", "desconto", "encargos", "juros", "juros.contrato",
        "juros.mora", "multa", "preco.tabela", "principal", "reajuste",
        "remuneracao.venda", "repasse.cef.a.incorrer", "repasse.cef.desc.subs",
        "repasse.cef.fgts", "repasse.cef.fin", "repasse.cef.incorrido",
        "repasse.cef.obra", "repasse.cef.obra.acum", "repasse.cef.rec.prop",
        "repasse.cef.terreno", "repasse.cef.terreno.acum", "repasse.cef.total",
        "saldo", "seguro", "soma.meses", "total", "valor", "valor.c.d",
        "valor.imovel", "valor.venda.ana", "valor.venda.ik",
        # Colunas de meses (YYYY-MM-DD)
        names(rec.uni)[str_detect(names(rec.uni), "^\\d{4}-\\d{2}-\\d{2}$")]
      ),
      col_width_auto = c(
        "cliente", "conta.sidec/nsgd", "corretor", "descricao", "edificacao",
        "imobiliaria", "lancamentos", "nome.razao", "obs.situacao", "pavimento",
        "setor"
      ),
      col_formulas = list(
        desp = list(
          categoria = "VLOOKUP([@[centro.negocio]],tb_ik_viab,2)"
        )
      ),
      col_width_spec = c(
        empreendimento = 30,
        id = 22
      ),
      cell_values = if (usar_cache) {
        list(inputs = list(A1 = data_dt))
      } else {
        NULL
      },
      save = list(
        nome_arquivo = sprintf("Fechamento-%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")),
        caminho_destino = normalizePath(
          file.path(Sys.getenv("USERPROFILE"), "Downloads"),
          winslash = "\\", mustWork = FALSE
        )
      )
    )
    msg("Excel gerado.")
  }

  msg("Concluido!")

  list(
    # Inputs combinados
    cef = in.cef,
    cef.detalhado = in.cef.detalhado,
    cef.mensal = in.cef.mensal,
    cmfcn.xcef = in.cmfcn.xcef,
    cmfcn.xcef.mensal = in.cmfcn.xcef.mensal,
    m.vb = in.m.vb,
    desp.m.ik = in.desp.m.ik,
    rec = in.rec,
    rec.uni = rec.uni,
    unis.cruzado = in.unis.cruzado,
    xcef.mensal = in.xcef.mensal,
    # Inputs originais
    car = in.car,
    cmfcns = in.cmfcns,
    cmfcns.mensal = in.cmfcns.mensal,
    contr = in.contr,
    cr = in.cr,
    desp = in.desp,
    ecns = in.ecns,
    empr = in.empr,
    estq = in.estq,
    unis = in.unis,
    xcef = in.xcef
  )
}

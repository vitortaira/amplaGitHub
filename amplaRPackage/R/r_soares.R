r_soares <- function(xlsx = FALSE) {
  # CMF_CN
  in.cmfcns.mensal <- e_cef_cmfcns_mensal() %>%
    rename(contrato.cef = contrato)
  # ECNs
  in.ecns <- e_cef_ecns()$ecn_u %>%
    rename(contrato.cef = contrato) %>%
    mutate(
      contrato.cef = str_remove_all(contrato.cef, "-") %>%
        if_else(str_length(.) == 13, str_sub(., 1, 12), .),
      parcela.cef.total = financiamento + desconto.subsidio + fgts + recursos.proprios,
      parcela.cef.incorrido = valor.liberado.terreno + valor.liberado.obra,
      parcela.cef.a.incorrer = parcela.cef.total - parcela.cef.incorrido
    ) %>%
    dplyr::select(
      contrato.cef, parcela.cef.total, parcela.cef.incorrido,
      parcela.cef.a.incorrer, financiamento, desconto.subsidio, fgts,
      recursos.proprios, valor.liberado.terreno, valor.liberado.obra, arquivo
    )
  # cef: cmfcns + ecns
  in.cef <- in.ecns %>%
    left_join(
      in.cmfcns.mensal %>%
        select(contrato.cef, natureza, total, arquivo) %>%
        tidyr::pivot_wider(
          names_from = natureza,
          values_from = total,
          values_fill = 0
        ),
      by = "contrato.cef",
      suffix = c(".ecns", ".cmfcns")
    )
  # Contratos
  in.contr <-
    e_ik_contrs() %>%
    rename(contrato = contrato.ampla) %>%
    dplyr::select(id.contr, empresa, contrato, contrato.cef, repassado) %>%
    dplyr::filter(!is.na(empresa))
  # Contas recebidas
  in.cr <- e_ik_rec() %>%
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
    dplyr::filter(
      empresa %in% c("AMP", "AVS", "GRA", "LUC", "POM", "SN2", "SN4") &
        !str_detect(empreendimento, "(?i)sic[ií]lia")
    ) %>%
    dplyr::select(
      empreendimento, empresa, total, data.vencimento, data.pagamento, cliente,
      contrato, contrato.cef, repassado, ele, esp, esp.con, agente, parcela,
      principal, juros, juros.contrato, juros.mora, reajuste, encargos, multa,
      seguro, desconto, cart, r.f, edificacao, especie, unidade, data.emissao,
      disp, pavimento, arquivo, arquivo.tipo, arquivo.tabela.tipo, arquivo.fonte
    )
  # Contas a receber
  in.car <- e_ik_car()$car %>%
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
      empresa %in% c("AMP", "AVS", "GRA", "LUC", "POM", "SN2", "SN4")
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
      data = coalesce(data.pagamento, data.vencimento),
      especie = if_else(
        str_detect(especie, "(?i)garagens"), "Garagem", especie
      ),
      id = str_c(empresa, especie, unidade, sep = "-"),
      natureza = case_when(
        ele == "TAX" ~ "taxa.extra",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Não" ~ "parcela.cef.assinar",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Sim" ~ "parcela.cef.total.ik",
        TRUE ~ "pro.soluto"
      )
    )
  # dplyr::filter(id != "AVS-Apartamento-2")
  # Unidades
  in.unis <- e_ik_unis() %>%
    rename(unidade = numero) %>%
    mutate(
      empresa = str_sub(empreendimento, 1, 3),
      especie = case_when(
        str_detect(unidade, "(?i)moto") ~ "Moto",
        str_detect(especie, "(?i)garagens") ~ "Garagem",
        TRUE ~ especie
      ),
      unidade = str_remove_all(unidade, "[^\\d]*") %>% as.integer(),
      id = str_c(empresa, especie, unidade, sep = "-")
    ) %>%
    dplyr::filter(
      empresa %in% c("AMP", "AVS", "GRA", "LUC", "POM", "SN2", "SN4") &
        !str_detect(empreendimento, "Sicília")
    )
  # Extratos da CEF
  in.xcef <- e_cef_xcefs()
  # Extratos CEF cruzados com CMF_CN
  in.cmfcn_xcef <- r_xcef()
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

    natureza <- in.unis %>%
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
      data.mes = floor_date(coalesce(data.pagamento, data.vencimento), "month"),
      cruzada = case_when(
      is.na(empresa.rec) ~ "in.unis",
      is.na(empresa.unis) ~ "in.rec",
      TRUE ~ "ambos"
      )
    ) %>%
    # Agregar por id, mês e natureza
    group_by(id, data.mes, natureza) %>%
    summarise(
      empresa = first(empresa),
      especie = first(especie),
      pavimento = first(pavimento),
      unidade = first(unidade),
      cliente = first(cliente.unis),
      data.venda = first(data.unis),
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
    # Normalizar contrato.cef (13 → 12 caracteres)
    mutate(
      contrato.cef = if_else(
      str_length(contrato.cef) == 13,
      str_sub(contrato.cef, 1, 12),
      contrato.cef
      )
    ) %>%
    # Adicionar dados ECN
    left_join(in.ecns, by = "contrato.cef") %>%
    mutate(
      checar = !is.na(contrato.cef) & is.na(parcela.cef.total),
      contrato.comeco = str_sub(contrato, 1, 4),
      contrato.fim = str_sub(contrato, -1) %>% as.integer()
    ) %>%
    # Priorizar contrato mais recente dentro de cada série
    group_by(id, natureza, contrato.comeco) %>%
    slice_max(contrato.fim, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Priorizar contrato com maior soma mensal
    rowwise() %>%
    mutate(soma.meses = sum(c_across(matches("^\\d{4}-\\d{2}-\\d{2}$")), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(id, natureza) %>%
    slice_max(soma.meses, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Organizar colunas com meses ordenados cronologicamente
    select(
      id, natureza, empresa, especie, pavimento, unidade, cliente, situacao,
      data.venda, contrato, contrato.cef, valor.venda,
      parcela.cef.total, parcela.cef.incorrido, checar, soma.meses,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    ) %>%
    arrange(id, natureza)


  # Pró soluto por unidade
  ps.uni <- natureza %>%
    dplyr::filter(natureza == "pro.soluto" | is.na(natureza)) %>%
    # Organizar colunas com meses ordenados cronologicamente
    select(
      id, empresa, especie, pavimento, unidade, cliente, situacao, data.venda,
      contrato, contrato.cef, valor.venda, soma.meses,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    ) %>%
    arrange(id)

if (xlsx) {
  gerar_xlsx(
    data = list(
      # Outputs
      # cef.uni = cef.uni,
      natureza.uni = natureza,
      ps.uni = ps.uni,
      # tx.uni = tx.uni,
      # Inputs
      ## Inputs originais
      in.car = in.car,
      in.cmfcns = e_cef_cmfcns(),
      in.cmfcns.mensal = in.cmfcns.mensal,
      in.contr = in.contr,
      in.cr = in.cr,
      in.ecns = in.ecns,
      in.unis = in.unis,
      in.xcef = in.xcef,
      ## Inputs combinados
      in.cef = in.cef,
      in.cmfcn_xcef = in.cmfcn_xcef,
      in.rec = in.rec
    ),
    tab_colours = c(
      natureza.uni = "darkblue",
      ps.uni = "blue",
      in.car = "white",
      in.cmfcns = "white",
      in.cmfcns.mensal = "white",
      in.contr = "white",
      in.cr = "white",
      in.ecns = "white",
      in.unis = "white",
      in.xcef = "white",
      in.cef = "darkgray",
      in.cmfcn_xcef = "darkgray",
      in.rec = "darkgray"
    ),
    col_dates = c(
      "data", "data.emissao", "data.lancamento", "data.movimentacao",
      "data.movimento", "data.pagamento", "data.venda", "data.vencimento",
      "periodo.inicio", "periodo.fim"
    ),
    col_monetary = c(
      "amortizacao.pj", "desconto", "desconto.subsidio", "encargos", "fgts",
      "financiamento", "juros", "juros.contrato", "juros.mora", "multa",
      "parcela.cef.incorrido", "parcela.cef.total", "principal",
      "recursos.proprios", "reajuste", "remuneracao.terreno",
      "remuneracao.venda", "repasse.cef.obra", "repasse.cef.terreno", "saldo",
      "seguro", "soma.meses", "total", "valor", "valor.c.d", "valor.imovel",
      "valor.liberado.obra", "valor.liberado.terreno", "valor.venda"
    ),
    col_width_auto = c(
      "cliente", "conta.sidec/nsgd", "corretor", "descricao", "edificacao",
      "imobiliaria", "lancamentos", "nome.razao", "obs.situacao", "pavimento",
      "setor"
    ),
    col_width_spec = c(
      empreendimento = 30,
      id = 22
    ),
    save = list(
      nome_arquivo = sprintf("soares-%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")),
      caminho_destino = normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads"), winslash = "\\", mustWork = FALSE)
    )
  )
}

  list(
    # Outputs
    ps.uni = ps.uni,
    # Inputs
    # Inputs originais
    in.car = in.car,
    in.cmfcns = e_cef_cmfcns(),
    in.cmfcns.mensal = in.cmfcns.mensal,
    in.contr = in.contr,
    in.cr = in.cr,
    in.ecns = in.ecns,
    in.unis = in.unis,
    in.xcef = in.xcef,
    # Inputs combinados
    in.cef = in.cef,
    in.cmfcn_xcef = in.cmfcn_xcef,
    in.rec = in.rec,
    natureza = natureza
  )
}

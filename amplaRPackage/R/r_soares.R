r_soares <- function(xlsx = FALSE) {
  # CMF_CN
  in.cmfcns <- e_cef_cmfcns()
  in.cmfcns.mensal <- in.cmfcns %>%
    dplyr::filter(!is.na(valor)) %>%
    mutate(mes = floor_date(data.movimento, "month")) %>%
    group_by(empresa, contrato, natureza, arquivo, mes) %>%
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
    mutate(contrato.cef.5 = str_sub(contrato, -5)) %>%
    select(
      empresa, contrato.cef.5, natureza, arquivo, total,
      any_of(sort(names(.)[!names(.) %in% c("empresa", "contrato.cef.5", "natureza", "arquivo", "total")]))
    )
  # ECNs
  in.ecns <- e_cef_ecns()$ecn_u %>%
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
      contrato.cef.5, repasse.cef.total, repasse.cef.incorrido,
      repasse.cef.a.incorrer, repasse.cef.fin, repasse.cef.desc.subs,
      repasse.cef.fgts, repasse.cef.rec.prop, repasse.cef.terreno.acum,
      repasse.cef.obra.acum, arquivo
    )
  # cef: cmfcns + ecns
  in.cef <- in.ecns %>%
    left_join(
      in.cmfcns.mensal %>%
        select(contrato.cef.5, natureza, total, arquivo) %>%
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
      repasse.cef.a.incorrer, repasse.cef.fin, repasse.cef.desc.subs, repasse.cef.fgts,
      repasse.cef.rec.prop, repasse.cef.terreno.acum, repasse.cef.terreno,
      cef.terreno, repasse.cef.obra.acum, repasse.cef.obra, cef.obra,
      amortizacao.pj, remuneracao.terreno, remuneracao.venda, arquivo.ecns,
      arquivo.cmfcns
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
      data = as.Date(data),
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
  # Extratos da CEF mensalizados por contrato
  in.xcef.mensal <- in.xcef %>%
    dplyr::filter(!is.na(valor)) %>%
    mutate(mes = floor_date(data.movimentacao, "month")) %>%
    group_by(empresa, contrato.5, natureza, mes) %>%
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
    rename(contrato.cef.5 = contrato.5) %>%
    select(
      empresa, contrato.cef.5, natureza, total,
      any_of(sort(names(.)[!names(.) %in% c("empresa", "contrato.cef.5", "natureza", "total")]))
    )
  # Extratos CEF cruzados com CMF_CN
  in.cmfcn.xcef <- r_xcef()
  # Extratos CEF cruzados com CMF_CN mensalizados por contrato
  in.cmfcn.xcef.mensal <- in.cmfcn.xcef %>%
    mutate(mes = floor_date(data.movimentacao, "month")) %>%
    group_by(empresa, contrato.5, natureza.cmfcn, mes) %>%
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
    rename(contrato.cef.5 = contrato.5) %>%
    select(
      empresa, contrato.cef.5, natureza.cmfcn, total,
      any_of(sort(names(.)[!names(.) %in% c("empresa", "contrato.cef.5", "natureza.cmfcn", "total")]))
    )
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
      ),
      contrato.cef.5 = str_sub(contrato.cef, -5, -1)
    ) %>%
    # Adicionar dados ECN
    left_join(in.ecns, by = "contrato.cef.5") %>%
    mutate(
      checar = !is.na(contrato.cef.5) & is.na(repasse.cef.total),
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
      id, empresa, especie, unidade, cliente, contrato, contrato.cef, pavimento,
      situacao, data.venda, valor.venda, repasse.cef.total, checar, natureza,
      soma.meses,
      any_of(sort(names(.)[str_detect(names(.), "^\\d{4}-\\d{2}-\\d{2}$")]))
    ) %>%
    arrange(id, natureza)

if (xlsx) {
  gerar_xlsx(
    data = list(
      # Outputs
      rec.uni = rec.uni,
      # Inputs
      ## Inputs combinados
      in.cef = in.cef,
      in.cmfcn.xcef = in.cmfcn.xcef,
      in.cmfcn.xcef.mensal = in.cmfcn.xcef.mensal,
      in.rec = in.rec,
      ## Inputs originais
      in.car = in.car,
      in.cmfcns = e_cef_cmfcns(),
      in.cmfcns.mensal = in.cmfcns.mensal,
      in.contr = in.contr,
      in.cr = in.cr,
      in.ecns = in.ecns,
      in.unis = in.unis,
      in.xcef = in.xcef,
      in.xcef.mensal = in.xcef.mensal
    ),
    tab_colours = c(
      rec.uni = "darkblue",
      in.cef = "darkgray",
      in.cmfcn.xcef = "darkgray",
      in.cmfcn.xcef.mensal = "darkgray",
      in.rec = "darkgray",
      in.car = "white",
      in.cmfcns = "white",
      in.cmfcns.mensal = "white",
      in.contr = "white",
      in.cr = "white",
      in.ecns = "white",
      in.unis = "white",
      in.xcef = "white",
      in.xcef.mensal = "white"
    ),
    col_headers = list(
      rec.uni = list(
        checar = list(colour = "yellow"),
        repasse.cef.total = list(colour = "blue", font_colour = "white")
      ),
      in.cef = list(
        # ECNs (blue)
        arquivo.ecns = list(colour = "blue", font_colour = "white"),
        repasse.cef.desc.subs = list(colour = "blue", font_colour = "white"),
        repasse.cef.fgts = list(colour = "blue", font_colour = "white"),
        repasse.cef.fin = list(colour = "blue", font_colour = "white"),
        repasse.cef.a.incorrer = list(colour = "white"),
        repasse.cef.incorrido = list(colour = "blue", font_colour = "white"),
        repasse.cef.total = list(colour = "blue", font_colour = "white"),
        repasse.cef.rec.prop = list(colour = "blue", font_colour = "white"),
        repasse.cef.obra.acum = list(colour = "blue", font_colour = "white"),
        repasse.cef.terreno.acum = list(colour = "blue", font_colour = "white"),
        # CMF_CNs (lightblue)
        arquivo.cmfcns = list(colour = "lightblue"),
        amortizacao.pj = list(colour = "lightblue"),
        remuneracao.terreno = list(colour = "lightblue"),
        remuneracao.venda = list(colour = "lightblue"),
        repasse.cef.obra = list(colour = "lightblue"),
        repasse.cef.terreno = list(colour = "lightblue"),
        # Checagem de contratos recentimente registrados na CEF
        cef.obra = list(colour = "yellow"),
        cef.terreno = list(colour = "yellow")
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
      rec.uni = "situacao",
      in.cef = "contrato.cef"
    ),
    col_monetary = c(
      "amortizacao.pj", "desconto", "encargos", "juros", "juros.contrato",
      "juros.mora", "multa", "principal", "reajuste", "remuneracao.venda",
      "repasse.cef.a.incorrer", "repasse.cef.desc.subs", "repasse.cef.fgts",
      "repasse.cef.fin", "repasse.cef.incorrido", "repasse.cef.obra",
      "repasse.cef.obra.acum", "repasse.cef.rec.prop", "repasse.cef.terreno",
      "repasse.cef.terreno.acum", "repasse.cef.total", "saldo", "seguro",
      "soma.meses", "total", "valor", "valor.c.d", "valor.imovel", "valor.venda",
      # Colunas de meses (YYYY-MM-DD)
      names(rec.uni)[str_detect(names(rec.uni), "^\\d{4}-\\d{2}-\\d{2}$")]
    ),
    col_width_auto = c(
      "cliente", "conta.sidec/nsgd", "corretor", "descricao", "edificacao",
      "imobiliaria", "lancamentos", "nome.razao", "obs.situacao", "pavimento",
      "setor"
    ),
    col_width_spec = c(
      cef.obra = 15,
      cef.terreno = 15,
      empreendimento = 30,
      id = 22,
      repasse.cef.a.incorrer = 22,
      repasse.cef.incorrido = 22,
      repasse.cef.terreno.acum = 22,
      remuneracao.terreno = 22
    ),
    save = list(
      nome_arquivo = sprintf("soares-%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")),
      caminho_destino = normalizePath(
        file.path(Sys.getenv("USERPROFILE"), "Downloads"),
        winslash = "\\", mustWork = FALSE
      )
    )
  )
}

  list(
    # Outputs
    rec.uni = rec.uni,
    # Inputs
    # Inputs combinados
    in.cef = in.cef,
    in.cmfcn.xcef = in.cmfcn.xcef,
    in.cmfcn.xcef.mensal = in.cmfcn.xcef.mensal,
    in.rec = in.rec,
    # Inputs originais
    in.car = in.car,
    in.cmfcns = in.cmfcns,
    in.cmfcns.mensal = in.cmfcns.mensal,
    in.contr = in.contr,
    in.cr = in.cr,
    in.ecns = in.ecns,
    in.unis = in.unis,
    in.xcef = in.xcef,
    in.xcef.mensal = in.xcef.mensal
  )
}

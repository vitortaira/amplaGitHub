r_soares <- function() {
  # CMF_CN
  in.cmfcn <- e_cef_cmfcns()
  # ECNs
  in.ecns <- e_cef_ecns()$ecn_u %>%
    rename(contrato.cef = contrato) %>%
    mutate(
      contrato.cef = str_remove_all(contrato.cef, "-") %>%
        if_else(str_length(.) == 13, str_sub(., 1, 12), .),
      parcela.cef.total = financiamento + desconto.subsidio + fgts + recursos.proprios,
      parcela.cef.incorrido = valor.liberado.terreno + valor.liberado.obra
    ) %>%
    dplyr::select(contrato.cef, parcela.cef.total, parcela.cef.incorrido)
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
        TRUE ~ "Pro soluto"
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

  # Fran: integração unis + rec com agregação mensal
  rec.uni <- in.unis %>%
    left_join(
      in.rec %>% dplyr::filter(is.na(natureza) | natureza == "Pro soluto"),
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
    # Agregar por id e mês
    group_by(id, data.mes) %>%
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
      natureza = first(natureza),
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
    # Adicionar totais de natureza
    left_join(totais, by = "id") %>%
    mutate(
      across(
        c(parcela.cef.total.ik, parcela.cef.assinar, taxa.extra),
        ~ tidyr::replace_na(.x, 0)
      )
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
    group_by(id, contrato.comeco) %>%
    slice_max(contrato.fim, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Priorizar contrato com maior soma mensal
    rowwise() %>%
    mutate(soma.meses = sum(c_across(matches("^\\d{4}-\\d{2}-\\d{2}$")), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(id) %>%
    slice_max(soma.meses, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Organizar colunas
    select(
      id, empresa, especie, pavimento, unidade, cliente, situacao, data.venda,
      contrato, contrato.cef, valor.venda,
      parcela.cef.total.ik, parcela.cef.assinar, taxa.extra,
      natureza, parcela.cef.total, parcela.cef.incorrido, checar, soma.meses,
      matches("^\\d{4}-\\d{2}-\\d{2}$")
    ) %>%
    arrange(id)


  list(
    # Outputs
    rec.uni  = rec.uni,
    # Inputs
    in.car   = in.car,
    in.cmfcn = in.cmfcn,
    in.contr = in.contr,
    in.cr    = in.cr,
    in.ecns  = in.ecns,
    in.rec   = in.rec,
    in.unis  = in.unis
  )
}

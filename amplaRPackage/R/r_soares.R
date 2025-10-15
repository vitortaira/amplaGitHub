r_soares <- function() {
  # CMF_CN
  in.cmfcn <- e_cef_cmfcns()
  # ECNs
  in.ecns <- e_cef_ecns()$ecn_u %>%
    rename(contrato.cef = contrato) %>%
    mutate(
      contrato.cef = str_remove_all(contrato.cef, "-") %>%
        if_else(str_length(.) == 13, str_sub(., 1, 12), .),
      fin.cef = financiamento + desconto.subsidio + fgts + recursos.proprios,
      creditado = valor.liberado.terreno + valor.liberado.obra
    ) %>%
    dplyr::select(contrato.cef, fin.cef, creditado)
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
        ele == "TAX" ~ "Taxa extra",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Não" ~ "Parcela CEF a repassar",
        ele %in% c("CEF", "FGT", "FIB", "FIN") &
          !empresa %in% c("POM", "SAU") &
          repassado == "Sim" ~ "Parcela CEF",
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
    group_by(id, natureza) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(natureza %in% c("Parcela CEF", "Parcela CEF a repassar", "Taxa extra")) %>%
    tidyr::pivot_wider(
      names_from = natureza,
      values_from = total,
      values_fill = 0,
      names_repair = "minimal"
    )
  # Fran: in.unis + in.rec
  rec.uni <- in.unis %>%
    left_join(in.rec, by = "id", suffix = c(".in.unis", ".in.rec")) %>%
    # Juntar in.contr para obter contrato.cef válido (usar contrato do in.rec diretamente)
    left_join(
      in.contr %>% select(empresa, contrato, contrato.cef),
      by = c("empresa.in.unis" = "empresa", "contrato" = "contrato"),
      suffix = c("", ".in.contr")
    ) %>%
    mutate(
      cliente = cliente.in.unis,
      data.venda = data.in.unis,
      situacao = situacao,
      valor.venda = valor.venda,
      contrato.cef = coalesce(contrato.cef.in.contr, contrato.cef),
      cruzada = case_when(
        id %in% anti_join(in.unis, in.rec, by = "id")$id ~ "in.unis",
        id %in% anti_join(in.rec, in.unis, by = "id")$id ~ "in.rec",
        TRUE ~ "ambos"
      )
    ) %>%
    # Manter Pro soluto e também ids sem receita (natureza NA) para não perder in.unis
    dplyr::filter(is.na(natureza) | natureza == "Pro soluto") %>%
    group_by(id,
      data.mes = floor_date(data.in.rec, "month")
    ) %>%
    summarise(
      empresa = first(coalesce(empresa.in.unis, empresa.in.rec)),
      especie = first(coalesce(especie.in.unis, especie.in.rec)),
      pavimento = first(coalesce(pavimento.in.unis, pavimento.in.rec)),
      unidade = first(coalesce(unidade.in.unis, unidade.in.rec)),
      cliente = first(cliente),
      data.venda = first(data.venda),
      situacao = first(situacao),
      valor.venda = first(valor.venda),
      contrato = first(contrato),
      contrato.cef = first(contrato.cef),
      total = sum(total, na.rm = TRUE),
      natureza = dplyr::first(natureza),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = data.mes,
      values_from = total,
      values_fill = 0,
      values_fn = sum
    ) %>%
    # Adicionar colunas de CEF e CEF a repassar
    left_join(totais, by = "id") %>%
    mutate(
      `Parcela CEF` = if ("Parcela CEF" %in% names(pick(everything()))) {
        tidyr::replace_na(`Parcela CEF`, 0)
      } else {
        0
      },
      `Parcela CEF a repassar` = if ("Parcela CEF a repassar" %in% names(pick(everything()))) {
        tidyr::replace_na(`Parcela CEF a repassar`, 0)
      } else {
        0
      },
      `Taxa extra` = if ("Taxa extra" %in% names(pick(everything()))) {
        tidyr::replace_na(`Taxa extra`, 0)
      } else {
        0
      }
    ) %>%
    arrange(id, natureza) %>%
    select(
      id, empresa, especie, pavimento, unidade, cliente, situacao, data.venda,
      contrato, contrato.cef, valor.venda, `Parcela CEF`, `Parcela CEF a repassar`, `Taxa extra`, natureza,
      sort(names(select(., -id, -natureza)))
    ) %>%
    # Colapsar linhas duplicadas por id (quebra por mês) mantendo atributos e
    # somando apenas as colunas mensais (nomes no formato YYYY-MM-DD)
    dplyr::group_by(id) %>%
    mutate(contrato.cef = if_else(
      str_length(contrato.cef) == 13,
      str_sub(contrato.cef, 1, 12),
      contrato.cef
    )) %>%
    left_join(in.ecns, by = "contrato.cef") %>%
    mutate(
      checar = if_else(
        !is.na(contrato.cef) & is.na(fin.cef),
        TRUE,
        FALSE
      ),
      contrato.comeco = str_sub(contrato, 1, 4),
      contrato.fim = str_sub(contrato, -1) %>% as.integer()
    ) %>%
    group_by(id, contrato.comeco) %>%
    slice_max(contrato.fim, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(id) %>%
    # Calcular soma das colunas mensais (formato YYYY-MM-DD) para cada contrato
    rowwise() %>%
    mutate(
      soma_meses = sum(c_across(matches("^\\d{4}-\\d{2}-\\d{2}$")), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(id) %>%
    # Manter apenas o contrato com maior soma das parcelas mensais
    slice_max(soma_meses, n = 1, with_ties = FALSE) %>%
    ungroup()


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

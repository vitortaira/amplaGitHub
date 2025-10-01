e_vgv <- function() {
  car <- e_ik_car()$car %>%
    mutate(
      pavimento = stringr::str_replace_all(
        pavimento,
        c(
          "oitav\\s+o" = "oitavo",
          "Oitav\\s+o" = "Oitavo",
          "pav\\s+imento" = "pavimento"
        )
      ),
      id = str_c(empresa, especie, pavimento, unidade, sep = "-"),
      cruzada = TRUE
    ) %>%
    dplyr::filter(
      empresa %in% c("AMP", "AVS", "GRA", "LUC", "POM", "SN2", "SN4")
    )
  unis <- e_ik_unis() %>%
    rename(unidade = numero) %>%
    mutate(
      empresa = str_sub(empreendimento, 1, 3),
      especie = if_else(
        str_detect(unidade, "(?i)moto"),
        "Garagens_motos",
        especie
      ),
      unidade = str_remove_all(unidade, "[^\\d]*") %>% as.integer(),
      pavimento = str_squish(pavimento),
      id = str_c(empresa, especie, pavimento, unidade, sep = "-")
    ) %>%
    dplyr::filter(
      empresa %in% c("AMP", "AVS", "GRA", "LUC", "POM", "SN2", "SN4") &
        !str_detect(empreendimento, "Sicília")
    )
  vgv <- unis %>%
    full_join(car, by = "id") %>%
    mutate(
      cruzada = case_when(
        id %in% anti_join(unis, car, by = "id")$id ~ "unis",
        id %in% anti_join(car, unis, by = "id")$id ~ "car",
        TRUE ~ "ambos"
      )
    ) %>%
    group_by(id) %>%
    summarise(
      empresa = first(empresa.x),
      empreendimento = first(empreendimento),
      especie = first(especie.x),
      pavimento = first(pavimento.x),
      unidade = first(unidade.x),
      area = first(area),
      valor.imovel = first(valor.imovel),
      valor.atualizado.ANU = sum(valor.atualizado[ele == "ANU"], na.rm = TRUE),
      valor.atualizado.CEF = sum(valor.atualizado[ele == "CEF"], na.rm = TRUE),
      valor.atualizado.CHA = sum(valor.atualizado[ele == "CHA"], na.rm = TRUE),
      valor.atualizado.FIB = sum(valor.atualizado[ele == "FIB"], na.rm = TRUE),
      valor.atualizado.FIN = sum(valor.atualizado[ele == "FIN"], na.rm = TRUE),
      valor.atualizado.INT = sum(valor.atualizado[ele == "INT"], na.rm = TRUE),
      valor.atualizado.MEN = sum(valor.atualizado[ele == "MEN"], na.rm = TRUE),
      valor.atualizado.PER = sum(valor.atualizado[ele == "PER"], na.rm = TRUE),
      valor.atualizado.REN = sum(valor.atualizado[ele == "REN"], na.rm = TRUE),
      valor.atualizado.RN1 = sum(valor.atualizado[ele == "RN1"], na.rm = TRUE),
      valor.atualizado.RNR = sum(valor.atualizado[ele == "RNR"], na.rm = TRUE),
      valor.atualizado.SEM = sum(valor.atualizado[ele == "SEM"], na.rm = TRUE),
      valor.atualizado.SIN = sum(valor.atualizado[ele == "SIN"], na.rm = TRUE),
      valor.atualizado.UNI = sum(valor.atualizado[ele == "UNI"], na.rm = TRUE),
      cruzada = first(cruzada),
      .groups = "drop"
    )
}

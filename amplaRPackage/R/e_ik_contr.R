e_ik_contr <-
  function(caminho.contr_c) {
    contr_t <- suppressMessages(readxl::read_excel(
      caminho.contr_c,
      sheet = 1,
      col_names = TRUE,
      col_types = NULL,
      na = c("", "NA"),
      skip = 0
    )) %>%
      as_tibble()
    empreendimento_c <- contr_t[1, 1] %>%
      str_remove("^.*\\:\\s?") %>%
      str_sub(1, 3)
    contr_t %<>%
      rename(
        Contrato_Ampla = "Nº Contrato",
        Contrato_CEF = "Nº Contrato Financiamento"
      ) %>%
      dplyr::filter(str_detect(Contrato_Ampla, "\\d{4}-\\d{1}")) %>%
      mutate(
        Sit = as.factor(Sit),
        `Cotista ?` = as.factor(`Cotista ?`),
        Autorizado = as.factor(Autorizado),
        Moeda = as.factor(Moeda),
        Esp = as.factor(Esp),
        `Tipo Contrato` = as.factor(`Tipo Contrato`),
        `Criado por` = as.factor(`Criado por`),
        `Alterado por` = as.factor(`Alterado por`),
        `Usuário Autorização` = as.factor(`Usuário Autorização`),
        Repassado = if_else(
          is.na(Contrato_CEF), "Não repassado", "Repassado",
        ) %>%
          as.factor(),
        Empreendimento = as.factor(empreendimento_c),
        Arquivo = caminho.contr_c
      ) %>%
      select(
        Empreendimento, Contrato_Ampla, Contrato_CEF, Repassado,
        `Tipo Contrato`, everything(), -`...1`
      )
    return(contr_t)
  }

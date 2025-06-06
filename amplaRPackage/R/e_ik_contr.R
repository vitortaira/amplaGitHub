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
        contrato.ampla = "Nº Contrato",
        contrato.alternativo = "Nº Con. Alternativo",
        contrato.cef = "Nº Contrato Financiamento",
        sit = "Sit",
        cliente = "Cliente",
        cotista = "Cotista ?",
        autorizado = "Autorizado",
        data.contrato = "Data Contrato",
        `cpf/cnpj` = "CPF/CNPJ",
        identificacao.imovel = "Identificação do Imóvel",
        id.cartao = "ID.Cartão",
        moeda = "Moeda",
        esp = "Esp",
        tipo.contrato = "Tipo Contrato",
        data.rescisao = "Data Rescisão",
        notificacao = "Notif.",
        data.notificacao = "Data Notificação",
        empreendimento = "Empreendimento",
        criado.por = "Criado por",
        criado.em = "Criado em",
        alterado.por = "Alterado por",
        alterado.em = "Alterado em",
        notificado.por = "Notificado por",
        notificado.em = "Notificado em",
        observacao = "Observação",
        data.autorizacao = "Data Autorização",
        usuario.autorizacao = "Usuário Autorização"
      ) %>%
      dplyr::filter(str_detect(contrato.ampla, "\\d{4}-\\d{1}")) %>%
      mutate(across(any_of(c(
        "sit", "cotista", "autorizado", "moeda", "esp", "tipo.contrato",
        "criado.por", "alterado.por", "usuario.autorizacao"
      )), as.factor)) %>%
      mutate(
        repassado = if_else(
          is.na(contrato.cef), "Não repassado", "repassado"
        ) %>% as.factor(),
        empreendimento = as.factor(empreendimento_c),
        arquivo = caminho.contr_c
      ) %>%
      select(
        empreendimento, contrato.ampla, contrato.cef, repassado,
        tipo.contrato, everything(), -`...1`
      )
    return(contr_t)
  }

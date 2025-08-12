c_extratos <- function(arquivo.subtipo = "melhores", arquivo.tipo = "todos") {
  # Validar parâmetros
  arquivo.subtipo <- match.arg(arquivo.subtipo, c("todos", "melhores"))
  arquivo.tipo <- match.arg(arquivo.tipo, c("todos", "xcef", "xita"))

  # Tentar carregar mapeamento de contas bancárias
  contasBancarias <- tryCatch(
    {
      # Assumindo que existe uma função ou dados para mapeamento
      # Se não existir, retorna NULL
      NULL
    },
    error = function(e) {
      NULL
    }
  )

  extratosMetadados <- bind_rows(
    e_metadados("xcef") %>%
      mutate(
        arquivo.subtipo = c_cef_xcef(caminho),
        arquivo.tipo = "xcef"
      ),
    e_metadados("xita") %>%
      mutate(
        arquivo.subtipo = "xita1",
        arquivo.tipo = "xita"
      )
  ) %>%
    mutate(
      arquivo.extensao = fs::path_ext(caminho),
      banco = fs::path_file(caminho) %>% str_extract("(?<=-).*?(?=_)"),
      conta = fs::path_file(caminho) %>% str_extract("(?<=_)[0-9]+(?=-)"),
      mes = fs::path_file(caminho) %>% str_extract("\\d{4}_\\d{2}"),
      id.corrente = stringr::str_c(
        empresa,
        "-",
        banco,
        "_",
        stringr::str_remove_all(conta, "-") %>%
          stringr::str_sub(-4, -1)
      ),
      id = {
        # Safe ID assignment with proper error handling
        if (is.null(contasBancarias) || nrow(contasBancarias) == 0) {
          message(
            "Mapeamento de contas bancárias não está disponível. ",
            "Usando id.corrente como fallback."
          )
          id.corrente
        } else if (!all(c("id.antigo", "id.atual", "id.continuo") %in%
          names(contasBancarias))) {
          message(
            "Mapeamento de contas bancárias não possui as colunas ",
            "necessárias. Usando id.corrente como fallback."
          )
          id.corrente
        } else {
          message(sprintf(
            "Mapeamento de contas bancárias carregado com %d registros.",
            nrow(contasBancarias)
          ))

          # Safe case_when with id.corrente as fallback
          dplyr::case_when(
            id.corrente %in% contasBancarias$id.antigo
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.antigo)
              ],
            id.corrente %in% contasBancarias$id.atual
            ~ contasBancarias$id.continuo[
                match(id.corrente, contasBancarias$id.atual)
              ],
            TRUE ~ id.corrente
          )
        }
      }
    )

  # Filtrar por arquivo.tipo se não for "todos"
  if (arquivo.tipo != "todos") {
    extratosMetadados <- extratosMetadados %>%
      filter(arquivo.tipo == !!arquivo.tipo)
  }

  # Aplicar filtragem por arquivo.subtipo
  if (arquivo.subtipo == "todos") {
    return(extratosMetadados)
  } else if (arquivo.subtipo == "melhores") {
    # Debug: mostrar quantos registros temos antes da filtragem
    message(sprintf("Total de registros antes da filtragem 'melhores': %d", nrow(extratosMetadados)))

    # Filtrar apenas registros que têm todas as informações necessárias
    extratosMetadados_completos <- extratosMetadados %>%
      filter(
        !is.na(empresa) & empresa != "",
        !is.na(banco) & banco != "",
        !is.na(conta) & conta != "",
        !is.na(mes) & mes != "",
        !is.na(arquivo.subtipo) & arquivo.subtipo != ""
      )

    # Debug: mostrar quantos registros temos após filtrar registros completos
    message(sprintf("Registros com dados completos: %d", nrow(extratosMetadados_completos)))

    # Debug: contar registros por arquivo.tipo
    if (nrow(extratosMetadados_completos) > 0) {
      tipo_counts <- extratosMetadados_completos %>%
        count(arquivo.tipo, name = "n_files") %>%
        arrange(desc(n_files))

      for (i in 1:nrow(tipo_counts)) {
        message(sprintf("  %s: %d arquivos", tipo_counts$arquivo.tipo[i], tipo_counts$n_files[i]))
      }

      # Debug específico para CEF
      cef_data <- extratosMetadados_completos %>%
        filter(arquivo.tipo == "xcef")

      if (nrow(cef_data) > 0) {
        message(sprintf("CEF: %d arquivos encontrados", nrow(cef_data)))

        # Contar contas únicas
        contas_unicas <- cef_data %>%
          filter(!is.na(conta)) %>%
          distinct(empresa, banco, conta) %>%
          nrow()
        message(sprintf("CEF: %d combinações únicas (empresa-banco-conta)", contas_unicas))

        # Mostrar algumas amostras
        cef_sample <- cef_data %>%
          arrange(empresa, banco, conta, mes) %>%
          slice_head(n = 10)

        message("CEF: Amostras dos arquivos:")
        for (i in 1:min(nrow(cef_sample), 5)) {
          message(sprintf(
            "  %s - %s-%s_%s (%s) - %s",
            basename(cef_sample$caminho[i]),
            cef_sample$empresa[i],
            cef_sample$banco[i],
            cef_sample$conta[i],
            cef_sample$mes[i],
            cef_sample$arquivo.subtipo[i]
          ))
        }
      }
    }

    extratosMetadados_melhores <- extratosMetadados_completos %>%
      group_by(empresa, banco, conta, mes) %>%
      mutate(
        # Definir prioridade customizada por arquivo.tipo
        prioridade_subtipo = case_when(
          # Prioridades para arquivos CEF (xcef): 3>4>5>6>7>8>1>2
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef3" ~ 1,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef4" ~ 2,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef5" ~ 3,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef6" ~ 4,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef7" ~ 5,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef8" ~ 6,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef1" ~ 7,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef2" ~ 8,
          arquivo.tipo == "xcef" & arquivo.subtipo == "xcef9" ~ 9,
          # Prioridades para arquivos ITAÚ (xita): manter lógica simples
          arquivo.tipo == "xita" & arquivo.subtipo == "xita1" ~ 1,
          # Default para subtipos desconhecidos
          TRUE ~ 99
        )
      ) %>%
      # Selecionar apenas o arquivo de maior prioridade para cada grupo
      slice_min(prioridade_subtipo, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-prioridade_subtipo)

    # Debug: mostrar quantos registros temos após aplicar lógica de prioridade
    message(sprintf("Registros após filtragem 'melhores': %d", nrow(extratosMetadados_melhores)))

    # Debug final: contar por arquivo.tipo
    if (nrow(extratosMetadados_melhores) > 0) {
      tipo_counts_final <- extratosMetadados_melhores %>%
        count(arquivo.tipo, name = "n_files") %>%
        arrange(desc(n_files))

      message("Resultado final por tipo:")
      for (i in 1:nrow(tipo_counts_final)) {
        message(sprintf("  %s: %d arquivos", tipo_counts_final$arquivo.tipo[i], tipo_counts_final$n_files[i]))
      }

      # Debug final específico para CEF
      cef_final <- extratosMetadados_melhores %>%
        filter(arquivo.tipo == "xcef")

      if (nrow(cef_final) > 0) {
        contas_unicas_final <- cef_final %>%
          filter(!is.na(conta)) %>%
          distinct(empresa, banco, conta) %>%
          nrow()
        message(sprintf("CEF final: %d combinações únicas (empresa-banco-conta)", contas_unicas_final))

        contas_lista <- cef_final %>%
          filter(!is.na(conta)) %>%
          distinct(empresa, banco, conta) %>%
          arrange(empresa, banco, conta)

        if (nrow(contas_lista) > 0) {
          contas_str <- contas_lista %>%
            mutate(combo = paste(empresa, banco, conta, sep = "|")) %>%
            pull(combo) %>%
            paste(collapse = ", ")
          message(sprintf("CEF contas: %s", contas_str))
        }
      }
    }

    return(extratosMetadados_melhores)
  }

  return(extratosMetadados)
}

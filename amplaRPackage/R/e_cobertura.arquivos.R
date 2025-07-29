e_cobertura.arquivos <- function() {
  # Obter dados CEF
  cef_dados <- tryCatch(
    {
      e_cef_xcefs()
    },
    error = function(e) {
      message("Erro ao extrair dados CEF: ", e$message)
      return(tibble(
        arquivo = character(), arquivo.subtipo = character(),
        empresa = character(), conta = character(),
        periodo.inicio = as.Date(character()), periodo.fim = as.Date(character())
      ))
    }
  )

  # Obter dados ITAÚ
  ita_dados <- tryCatch(
    {
      e_ita_xitas()$xita_l
    },
    error = function(e) {
      message("Erro ao extrair dados ITAÚ: ", e$message)
      return(tibble(
        arquivo = character(), arquivo.subtipo = character(),
        empresa = character(), conta = character(),
        periodo.inicio = as.Date(character()), periodo.fim = as.Date(character())
      ))
    }
  )

  # Garantir que as colunas necessárias existam
  required_cols <- c("arquivo", "arquivo.subtipo", "empresa", "conta", "periodo.inicio", "periodo.fim")

  for (col in required_cols) {
    if (!col %in% names(cef_dados)) {
      if (col %in% c("periodo.inicio", "periodo.fim")) {
        cef_dados[[col]] <- as.Date(NA)
      } else {
        cef_dados[[col]] <- NA_character_
      }
    }
    if (!col %in% names(ita_dados)) {
      if (col %in% c("periodo.inicio", "periodo.fim")) {
        ita_dados[[col]] <- as.Date(NA)
      } else {
        ita_dados[[col]] <- NA_character_
      }
    }
  }

  xcef_t <- left_join(
    e_metadados("xcef") %>%
      rename(arquivo = "caminho"),
    cef_dados %>%
      dplyr::select(all_of(required_cols)),
    by = "arquivo"
  )

  extita_t <- left_join(
    e_metadados("xita") %>%
      rename(arquivo = "caminho"),
    ita_dados %>%
      dplyr::select(all_of(required_cols)),
    by = "arquivo"
  )

  cobertura_t <- bind_rows(
    xcef_t,
    extita_t
  ) %>%
    distinct()

  message(sprintf("Total de registros antes da filtragem: %d", nrow(cobertura_t)))

  # Garantir que as colunas necessárias existam após o join
  if (!"empresa" %in% names(cobertura_t)) {
    cobertura_t$empresa <- NA_character_
    message("Coluna 'empresa' criada com valores NA")
  }
  if (!"arquivo.tipo" %in% names(cobertura_t)) {
    cobertura_t$arquivo.tipo <- NA_character_
    message("Coluna 'arquivo.tipo' criada com valores NA")
  }
  if (!"conta" %in% names(cobertura_t)) {
    cobertura_t$conta <- NA_character_
    message("Coluna 'conta' criada com valores NA")
  }

  # Verificar quantos registros têm empresa não-NA
  registros_com_empresa <- sum(!is.na(cobertura_t$empresa) & cobertura_t$empresa != "", na.rm = TRUE)
  message(sprintf("Registros com empresa válida: %d", registros_com_empresa))

  # Se não há empresas válidas, vamos manter todos os dados e usar arquivo.tipo
  if (registros_com_empresa == 0) {
    message("Nenhum registro com empresa válida encontrado. Mantendo todos os dados.")
    # Não filtrar por empresa, apenas garantir que arquivo.tipo existe
  } else {
    # Filtrar apenas registros com empresa não-NA
    cobertura_t <- cobertura_t %>%
      filter(!is.na(empresa) & empresa != "")
  }

  # Verificar se a coluna conta existe antes de modificá-la
  if ("conta" %in% names(cobertura_t)) {
    cobertura_t <- cobertura_t %>%
      mutate(conta = str_remove(conta, "-") %>% str_sub(-4, -1))
  }

  message(sprintf("Total de registros retornados: %d", nrow(cobertura_t)))
  if (nrow(cobertura_t) > 0) {
    message(sprintf("Colunas disponíveis: %s", paste(names(cobertura_t), collapse = ", ")))
  }

  return(cobertura_t)
}

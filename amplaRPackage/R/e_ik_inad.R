# Descrição ---------------------------------------------------------------

# Condições para o funcionamento:
# arquivos em ".xlsx":
# (1) Estejam na pasta ".../Controladoria - Docmentos/AmplaR/dados/cef
# /inadimplentes
# (2) Sejam nomeados somente com um breve nome do empreendimento
# (eg "sonia1", "pompeia", etc.)

e_ik_inad <-
  function(caminho_arquivo_inadimplentes.c) {
    # Extraindo o vetor de linhas
    linhas_vc <-
      read_excel(caminho_arquivo_inadimplentes.c, col_names = F) %>%
      unite("linhas", everything(), sep = " ", remove = T) %>%
      unlist() %>%
      str_remove_all("NA") %>%
      str_trim() %>%
      suppressMessages()
    linhas_vc <-
      linhas_vc[linhas_vc != ""] %>%
      sapply(function(x) str_replace_all(x, "\\s+", " ")) %>%
      unlist() %>%
      str_trim() %>%
      unname()
    # Fora da tabela
    empreendimento_c <-
      linhas_vc %>%
      keep(~ str_starts(.x, "(?i)empreendimento\\:\\s?")) %>%
      str_remove("^.*\\:\\s?") %>%
      str_sub(1, 3)
    indice.data.impressao_vn <-
      linhas_vc %>%
      str_which("Impresso em:")
    data.impressao_p <-
      linhas_vc[indice.data.impressao_vn[1]] %>%
      str_remove(".* Impresso em: ") %>%
      str_trim() %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M:%S")
    indice.linhas.remover_vn <-
      linhas_vc %>%
      str_which("^Folha|^Relatório de Inadimplência|^Título|^Esp ")
    linhas_vc <-
      linhas_vc[-indice.linhas.remover_vn] %>%
      str_remove("Imobiliaria/Corretor:") %>%
      keep(~ .x != "")
    indice.clientes_vn <-
      linhas_vc %>%
      str_which("^(?i)cliente: ")
    clientes_vc <-
      linhas_vc[indice.clientes_vn] %>%
      str_remove(".*(?i)cliente: ") %>%
      str_remove("(?i)\\s?contrato:\\s?.*") %>%
      str_trim()
    contratos_vc <-
      linhas_vc[indice.clientes_vn] %>%
      str_remove(".* (?i)contrato: ")
    telefones_vc <-
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove("^(?i)telefones: ") %>%
      str_remove(" (?i)unidade:.*") %>%
      str_remove(" ") %>%
      str_remove("0xx") %>%
      str_trim()
    unidades_vc <-
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove(".*(?i)unidade: ") %>%
      str_trim()
    indice.fim.clientes_vn <-
      linhas_vc %>%
      str_which("Parcela")
    abertas_vi <-
      linhas_vc[indice.fim.clientes_vn] %>%
      str_remove(".*: ") %>%
      str_remove(" Parcelas") %>%
      str_remove(" Parcela") %>%
      str_extract("^[^ ]+")
    indice.parcelas_l <- list()
    for (i in seq_along(clientes_vc)) {
      # Índice das linhas de parcelas com todos os clientes
      indice.parcelas_l[[i]] <-
        (indice.clientes_vn[i] + 2):(indice.fim.clientes_vn[i] - 1)
    }
    parcelas.clientes_vn <- c()
    for (i in seq_along(clientes_vc)) {
      parcelas.clientes_vn[i] <-
        (indice.fim.clientes_vn[i] - 1) - (indice.clientes_vn[i] + 2) + 1
    }
    parcelas.clientes_vc <-
      rep(clientes_vc, parcelas.clientes_vn)
    parcelas.contratos_vc <-
      rep(contratos_vc, parcelas.clientes_vn)
    parcelas.unidades_vc <-
      rep(unidades_vc, parcelas.clientes_vn)
    parcelas.telefones_vc <-
      rep(telefones_vc, parcelas.clientes_vn)
    parcelas.abertas_vi <-
      rep(abertas_vi, parcelas.clientes_vn)
    indice.parcelas_vn <- indice.parcelas_l %>% unlist()
    parcelas_df <-
      linhas_vc[indice.parcelas_vn] %>%
      # str_remove("Imobiliaria/Corretor:") %>%
      # keep(~ .x != "") %>%
      str_trim() %>%
      as_tibble() %>%
      separate_wider_delim(
        cols = everything(),
        names =
          c(
            "esp", "parcela", "ele", "vencimento", "atraso", "r/f", "principal",
            "juros", "encargos", "juros.mora", "multa", "seguro", "total"
          ),
        delim = " "
      ) %>%
      mutate(
        cliente = parcelas.clientes_vc,
        esp = as.character(esp),
        parcela = as.character(parcela),
        quantidade.parcelas = as.integer(parcelas.abertas_vi),
        ele = as.character(ele),
        vencimento = as.character(vencimento) %>% as.Date(format = "%d/%m/%Y"),
        `r/f` = as.character(`r/f`),
        principal =
          ifelse(
            str_detect(principal, "\\.") &
              !str_detect(principal, ",") &
              str_detect(principal, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(principal),
            principal %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        juros =
          ifelse(
            str_detect(juros, "\\.") &
              !str_detect(juros, ",") &
              str_detect(juros, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(juros),
            juros %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        encargos =
          ifelse(
            str_detect(encargos, "\\.") &
              !str_detect(encargos, ",") &
              str_detect(encargos, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(encargos),
            encargos %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        juros.mora =
          ifelse(
            str_detect(juros.mora, "\\.") &
              !str_detect(juros.mora, ",") &
              str_detect(juros.mora, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(juros.mora),
            juros.mora %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        multa =
          ifelse(
            str_detect(multa, "\\.") &
              !str_detect(multa, ",") &
              str_detect(multa, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(multa),
            multa %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        seguro =
          ifelse(
            str_detect(seguro, "\\.") &
              !str_detect(seguro, ",") &
              str_detect(seguro, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(seguro),
            seguro %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        total =
          ifelse(
            str_detect(total, "\\.") &
              !str_detect(total, ",") &
              str_detect(total, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(total),
            total %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        data.consulta =
          as.POSIXct(data.impressao_p, format = "%Y-%m-%d %H:%M:%S"),
        atraso = as.integer(atraso),
        empreendimento = empreendimento_c,
        contrato = as.character(parcelas.contratos_vc),
        unidade = as.character(parcelas.unidades_vc),
        telefone = as.character(parcelas.telefones_vc),
        arquivo = caminho_arquivo_inadimplentes.c
      ) %>%
      select(
        empreendimento, contrato, unidade, cliente, telefone, esp, parcela,
        quantidade.parcelas, ele, vencimento, atraso, `r/f`, principal, juros,
        encargos, juros.mora, multa, seguro, total, data.consulta,
        arquivo
      )
    return(inad = parcelas_df)
  }

# Teste -------------------------------------------------------------------

# caminho_arquivo_inadimplentes.c <-
#  here::here(
#    "dados", "cef",
#    "inadimplentes", "prudência.xlsx"
#  )
# prudencia_t=e_ik_inad(caminho_arquivo_inadimplentes.c)
# str(e_ik_inad(caminho_arquivo_inadimplentes.c))
# View(e_ik_inad(caminho_arquivo_inadimplentes.c)$Parcelas)
# View(e_ik_inad(caminho_arquivo_inadimplentes.c)$Consolidado)

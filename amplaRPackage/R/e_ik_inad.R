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
      str_which("^Folha|^Relatório de Inadimplência|^Título|^esp ")
    linhas_vc <-
      linhas_vc[-indice.linhas.remover_vn] %>%
      str_remove("Imobiliaria/Corretor:") %>%
      keep(~ .x != "")
    indice.clientes_vn <-
      linhas_vc %>%
      str_which("^Cliente: ")
    clientes_vc <-
      linhas_vc[indice.clientes_vn] %>%
      str_remove(".*Cliente: ") %>%
      str_remove(" Contrato: .*") %>%
      str_trim()
    contratos_vc <-
      linhas_vc[indice.clientes_vn] %>%
      str_remove(".* Contrato: ")
    telefones_vc <-
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove("^Telefones: ") %>%
      str_remove(" Unidade:.*") %>%
      str_remove(" ") %>%
      str_remove("0xx") %>%
      str_trim()
    unidades_vc <-
      linhas_vc[indice.clientes_vn + 1] %>%
      str_remove(".*Unidade: ") %>%
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
            "esp", "Parcela", "Ele", "Vencto", "Atraso", "R/F", "Principal",
            "Juros", "Encargos", "Juros de Mora", "Multa", "Seguro", "Total"
          ),
        delim = " "
      ) %>%
      mutate(
        Cliente = parcelas.clientes_vc,
        esp = as.character(esp),
        Parcela = as.character(Parcela),
        `Quantidade de parcelas` = as.integer(parcelas.abertas_vi),
        Ele = as.character(Ele),
        Vencto = as.character(Vencto) %>% as.Date(format = "%d/%m/%Y"),
        `R/F` = as.character(`R/F`),
        Principal =
          ifelse(
            str_detect(Principal, "\\.") &
              !str_detect(Principal, ",") &
              str_detect(Principal, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Principal),
            Principal %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        Juros =
          ifelse(
            str_detect(Juros, "\\.") &
              !str_detect(Juros, ",") &
              str_detect(Juros, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Juros),
            Juros %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        Encargos =
          ifelse(
            str_detect(Encargos, "\\.") &
              !str_detect(Encargos, ",") &
              str_detect(Encargos, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Encargos),
            Encargos %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        `Juros de Mora` =
          ifelse(
            str_detect(`Juros de Mora`, "\\.") &
              !str_detect(`Juros de Mora`, ",") &
              str_detect(`Juros de Mora`, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(`Juros de Mora`),
            `Juros de Mora` %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        Multa =
          ifelse(
            str_detect(Multa, "\\.") &
              !str_detect(Multa, ",") &
              str_detect(Multa, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Multa),
            Multa %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        Seguro =
          ifelse(
            str_detect(Seguro, "\\.") &
              !str_detect(Seguro, ",") &
              str_detect(Seguro, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Seguro),
            Seguro %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        Total =
          ifelse(
            str_detect(Total, "\\.") &
              !str_detect(Total, ",") &
              str_detect(Total, "\\.\\d{3,}|\\d{3,}\\."),
            as.numeric(Total),
            Total %>%
              str_remove_all("\\.") %>%
              str_replace(",", ".") %>%
              as.numeric()
          ),
        `Data da consulta` =
          as.POSIXct(data.impressao_p, format = "%Y-%m-%d %H:%M:%S"),
        Atraso = as.integer(Atraso),
        empreendimento = empreendimento_c,
        Contrato = as.character(parcelas.contratos_vc),
        Unidade = as.character(parcelas.unidades_vc),
        Telefone = as.character(parcelas.telefones_vc),
        arquivo = caminho_arquivo_inadimplentes.c
      ) %>%
      select(
        empreendimento, Contrato, Unidade, Cliente, Telefone, esp, Parcela,
        `Quantidade de parcelas`, Ele, Vencto, Atraso, `R/F`, Principal, Juros,
        Encargos, `Juros de Mora`, Multa, Seguro, Total, `Data da consulta`,
        arquivo
      )
    return("Parcelas" = parcelas_df)
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

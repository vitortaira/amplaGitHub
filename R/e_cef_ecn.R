# Descrição ---------------------------------------------------------------

### RESUMO ###

# e_cef_ecn() extrai os dados de um arquivo ECN.

### UTILIZAÇÃO ###

# e_cef_ecn(
#   caminho.ecn_c
# )

### ARGUMENTOS ###

# caminho.ecn_c: String do caminho do arquivo ECN.

# Pacotes -----------------------------------------------------------------

library(openxlsx) # Funções para preencher arquivos .xlsx
library(pdftools) # Funções para extração de dados em PDF
library(readxl) # Funções para a importação de arquivos em Excel

# Criando função para extrair os dados dos arquivos ECN em PDF
e_cef_ecn <-
  function(caminho.ecn_c, xlsx = FALSE) {
    # paginas_l e linhas_vc -------------------------------------------------

    ## Extraindo PDF para uma lista contendo suas páginas
    paginas_l <-
      pdf_text(caminho.ecn_c) %>%
      ### Separando o texto por linhas
      strsplit("\n")
    ### Limpando o conteúdo das páginas
    for (i in seq_along(paginas_l)) {
      paginas_l[[i]] <-
        #### Removendo linhas vazias
        paginas_l[[i]][paginas_l[[i]] != ""] %>%
        #### Removendo eventuais espaços em branco no começo ou final da string
        trimws() %>%
        #### Transformando múltiplos espaços em branco em somente um
        sapply(function(x) gsub("\\s+", " ", x))
    }
    ## Criando função para encontrar a página de uma determinada linha
    encontrar_pagina <-
      function(numero_linha_n) {
        ### Criando um vetor com as últimas linhas de cada página
        linhas_pagina_fim_vn <- cumsum(sapply(paginas_l, length))
        ### Identificando a página com base nas últimas linhas de cada página
        numero_pagina_n <- sum(numero_linha_n > linhas_pagina_fim_vn) + 1
        ### Definindo o número da página como aquilo que a função retorna
        numero_pagina_n
      }
    ## Linhas do PDF
    linhas_vc <-
      unlist(paginas_l, use.names = FALSE)

    # Dados fora das tabelas --------------------------------------------------

    ## Data da consulta ao arquivo
    data_consulta_p <-
      linhas_vc %>%
      keep(
        ~ str_starts(.x, "(?i)empreendimentos da constru") &
          str_detect(.x, "\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}:\\d{2}")
      ) %>%
      nth(1) %>%
      str_extract("\\d{2}/\\d{2}/\\d{4}\\s?\\d{2}:\\d{2}:\\d{2}") %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M:%S")

    ## Financiamento
    ### Criando o objeto para armazenar a linha do financiamento
    financiamento.linha_c <-
      ### Extraindo a linha relevante da primeira página
      linhas_vc[grep("Linha de Financiamento:",
        linhas_vc,
        ignore.case = T
      )][1] %>%
      ### Removendo o começo irrelevante
      str_replace_all(".*Linha de Financiamento:", "") %>%
      ### Removendo o final irrelevante
      str_replace_all(" Tipo de Financiamento:.*", "") %>%
      ### Removendo eventuais espaços em branco no começo ou final da string
      trimws()

    ### Criando o objeto para armazenar o tipo do financiamento
    financiamento.tipo_c <-
      ### Extraindo a linha relevante da primeira página
      linhas_vc[grep("Tipo de Financiamento:",
        linhas_vc,
        ignore.case = T
      )][1] %>%
      ### Removendo o começo irrelevante
      str_replace_all(".*Tipo de Financiamento:", "") %>%
      ### Removendo eventuais espaços em branco no começo ou final da string
      trimws()

    ## Nome do empreendimento
    empreendimento_c <-
      ### Extraindo a linha relevante da primeira página
      linhas_vc[grep("^Nome do Empreendimento: .+",
        linhas_vc,
        ignore.case = T
      )][1] %>%
      ### Removendo o começo irrelevante
      str_replace_all(".*Nome do Empreendimento:", "") %>%
      ### Removendo o final irrelevante
      str_replace_all(" Linha .*", "") %>%
      ### Removendo eventuais espaços em branco no começo ou final da string
      trimws()

    # Tabela "Empreendimento" -------------------------------------------------

    # Identificando a linha da primeira aparição da tabela do empreendimento
    indice.empreendimento_n <-
      (str_which(
        linhas_vc,
        "^Empreendimento"
      ) + 2)[1]
    ## Tabela com dados do empreendimento
    empreendimento_df <-
      tibble(linhas_vc[indice.empreendimento_n]) %>%
      separate_wider_delim(
        cols = everything(),
        names = c(
          "Contrato",
          "APF",
          "Valor Aporte",
          "Total de Unidade",
          "Unidades Comercializadas",
          "Unidades Financiadas Construção",
          "Unidades Complementares",
          "Data Término de Obra",
          "Data Ini - Enc Fiador"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        Empreendimento = empreendimento_c,
        `Data de consulta` = data_consulta_p,
        Arquivo = caminho.ecn_c
      ) %>%
      select(Empreendimento, everything(), `Data de consulta`, Arquivo) %>%
      mutate(
        `Valor Aporte` =
          `Valor Aporte` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric()
      )

    # Tabela "Contrato empréstimo PJ" -----------------------------------------

    # Identificando a linha da primeira aparição da tabela de empréstimo
    indice.emprestimo_n <-
      (str_which(
        linhas_vc,
        "Contrato Empréstimo Pessoa Jurídica"
      ) + 2)[1]
    ## Tabela com dados do contrato de empréstimo PJ
    emprestimo_df <-
      tibble(linhas_vc[indice.emprestimo_n]) %>%
      separate_wider_delim(
        cols = everything(),
        names = c(
          "Número",
          "Data da Assinatura",
          "Valor Empréstimo",
          "Valor Reduzido",
          "Valor Utilizado",
          "Saldo Devedor"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        `Valor Empréstimo` =
          `Valor Empréstimo` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `Valor Reduzido` =
          `Valor Reduzido` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `Valor Utilizado` =
          `Valor Utilizado` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `Saldo Devedor` =
          `Saldo Devedor` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `Linha de financiamento` = as.character(financiamento.linha_c),
        `Tipo de financiamento` = as.character(financiamento.tipo_c),
        Empreendimento = empreendimento_c,
        `Data de consulta` = data_consulta_p,
        Arquivo = caminho.ecn_c
      ) %>%
      select(
        Empreendimento,
        everything(),
        `Data de consulta`,
        Arquivo
      )

    # Tabela "Informações consolidadas" ---------------------------------------

    ## Tabela com dados consolidados
    ### Identificando as primeiras linhas das partes da tabela de dados consolidados
    indice.consolidado.comeco_vn <- str_which(
      linhas_vc,
      "INFORMAÇÕES CONSOLIDADAS"
    ) + 2
    ### Identificando as últimas linhas das partes da tabela de dados consolidados
    indice.consolidado.fim_vn <- c()
    for (i in 1:length(indice.consolidado.comeco_vn)) {
      indice.consolidado.fim_vn[i] <-
        cumsum(sapply(paginas_l, length))[encontrar_pagina(indice.consolidado.comeco_vn[i])]
    }
    indice.consolidado_vn <- list()
    for (i in 1:length(indice.consolidado.comeco_vn)) {
      indice.consolidado_vn[[i]] <-
        indice.consolidado.comeco_vn[i]:indice.consolidado.fim_vn[i]
    }
    indice.consolidado_vn <-
      unlist(indice.consolidado_vn)[-length(unlist(indice.consolidado_vn))]
    ### Criando a tabela que será preenchida com os dados consolidados
    consolidado_df <-
      ### Criando tabela a partir do texto
      tibble(line = linhas_vc[indice.consolidado_vn]) %>%
      ### Separando as linhas de texto em colunas
      separate_wider_delim(
        cols = everything(),
        names = c(
          "Período",
          "Unidade",
          "Valor Creditado",
          "Valor Desbloqueado",
          "Valor Amortizado",
          "Encargo Quitado do PJ"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        `Período` = format(as.Date(`Período`, format = "%d/%m/%Y"), "%d/%m/%Y"),
        Unidade = as.character(Unidade),
        `Valor Creditado` = str_remove_all(`Valor Creditado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Valor Desbloqueado` = str_remove_all(`Valor Desbloqueado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Valor Amortizado` = str_remove_all(`Valor Amortizado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Encargo Quitado do PJ` = str_remove_all(`Encargo Quitado do PJ`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        Empreendimento = empreendimento_c,
        `Data de consulta` = data_consulta_p,
        Arquivo = caminho.ecn_c
      ) %>%
      select(
        Empreendimento,
        everything(),
        `Data de consulta`,
        Arquivo
      )

    # Tabela "Unidades" -------------------------------------------------------

    ## Tabela com dados das unidades
    ### Identificando as primeiras linhas das partes da tabela de unidades
    indice.unidades.comeco_vn <- grep("UNIDADES DE",
      linhas_vc,
      ignore.case = T
    ) + 4
    ### Identificando as últimas linhas das partes da tabela de unidades
    indice.unidades.fim_vn <- grep("EMPREENDIMENTOS DA CONSTRUCAO",
      linhas_vc,
      ignore.case = T
    )[-1] - 1
    ### Identificando todas as linhas da tabela de unidades
    indice.unidades_vn <- list()
    for (i in 1:length(indice.unidades.comeco_vn)) {
      indice.unidades_vn[[i]] <- indice.unidades.comeco_vn[i]:indice.unidades.fim_vn[i]
    }
    indice.unidades_vn <- unlist(indice.unidades_vn)[-length(unlist(indice.unidades_vn))]
    ### Removendo objetos que não serão mais utilizados
    rm(
      "indice.unidades.comeco_vn",
      "indice.unidades.fim_vn"
    )
    ### Criando tabela com dados das unidades a partir do texto
    unidades_df <-
      tibble(line = linhas_vc[indice.unidades_vn]) %>%
      #### Separando as linhas de texto em colunas
      separate_wider_delim(
        cols = everything(),
        names = c(
          "Contrato",
          "TP",
          "Data de Assinatura",
          "Data de Inclusão",
          "Data de Registro",
          "Financiamento",
          "Desconto Subsídio",
          "FGTS",
          "Recursos Próprios",
          "Compra / Venda",
          "Valor de Avaliação",
          "Valor Liberado Terreno",
          "Valor Liberado Obra",
          "Amortização"
        ),
        delim = " "
      ) %>%
      #### Garantindo que as colunas sejam das classes adequadas
      mutate(
        Contrato = as.character(Contrato),
        TP = as.character(TP),
        `Data de Assinatura` = as.Date(`Data de Assinatura`,
          format = "%d/%m/%Y"
        ),
        `Data de Inclusão` = as.Date(`Data de Inclusão`,
          format = "%d/%m/%Y"
        ),
        `Data de Registro` = as.Date(
          `Data de Registro`,
          "%d/%m/%Y"
        ),
        Financiamento = str_remove_all(Financiamento, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Desconto Subsídio` = str_remove_all(`Desconto Subsídio`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        FGTS = str_remove_all(FGTS, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Recursos Próprios` = str_remove_all(`Recursos Próprios`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Compra / Venda` = str_remove_all(`Compra / Venda`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Valor de Avaliação` = str_remove_all(`Valor de Avaliação`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Valor Liberado Terreno` = str_remove_all(`Valor Liberado Terreno`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `Valor Liberado Obra` = str_remove_all(`Valor Liberado Obra`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        Amortização = str_remove_all(Amortização, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        Empreendimento = empreendimento_c,
        `Data de consulta` = data_consulta_p,
        Arquivo = caminho.ecn_c
      ) %>%
      select(
        Empreendimento,
        everything(),
        `Data de consulta`,
        Arquivo
      )
    ## Criando a lista a ser retornada pela função e_cef_ecn()
    dados.arquivo.ecn_l <- list(
      Empreendimento = empreendimento_df,
      Emprestimo = emprestimo_df,
      Consolidado = consolidado_df,
      Unidades = unidades_df
    )


    #    # Salvando dados extraídos em um xlsx -------------------------------------
    #    if (xlsx == TRUE) {
    #      ## Salvando dados extraídos em um xlsx
    #      ### Criando a planilha xlsx
    #      xlsx <- createWorkbook()
    #      ### Empreendimento
    #      #### Criando a aba "Empreendimento"
    #      addWorksheet(xlsx, "Empreendimento", gridLines = FALSE)
    #      #### Populando a aba
    #      writeData(xlsx, "Empreendimento", dados.arquivo.ecn_l[["Empreendimento"]])
    #      #### Definindo formatação das bordas
    #      addStyle(xlsx,
    #        sheet = "Empreendimento",
    #        style = createStyle(border = "TopBottomLeftRight"),
    #        rows = 1:nrow(dados.arquivo.ecn_l[["Empreendimento"]]) + 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Empreendimento"]]),
    #        gridExpand = T
    #      )
    #      #### Definindo formatação das colunas
    #      setColWidths(xlsx,
    #        "Empreendimento",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Empreendimento"]]),
    #        widths = 18
    #      )
    #      #### Criando um filtro para a planilha
    #      addFilter(xlsx,
    #        "Empreendimento",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Empreendimento"]]),
    #        rows = 1
    #      )
    #      #### Definindo formatação da linha de cabeçalho
    #      addStyle(xlsx,
    #        sheet = "Empreendimento",
    #        style = createStyle(
    #          border = "TopBottomLeftRight",
    #          fontSize = 12,
    #          fontColour = "white",
    #          halign = "center",
    #          valign = "center",
    #          textDecoration = "bold",
    #          fgFill = "darkgray",
    #          wrapText = T
    #        ),
    #        rows = 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Empreendimento"]]),
    #        gridExpand = T
    #      )
    #      ### Emprestimo PJ
    #      #### Criando a aba "Emprestimo PJ"
    #      addWorksheet(xlsx, "Emprestimo PJ", gridLines = FALSE)
    #      #### Populando a aba
    #      writeData(xlsx, "Emprestimo PJ", dados.arquivo.ecn_l[["Emprestimo"]])
    #      #### Definindo formatação das bordas
    #      addStyle(xlsx,
    #        sheet = "Emprestimo PJ",
    #        style = createStyle(border = "TopBottomLeftRight"),
    #        rows = 1:nrow(dados.arquivo.ecn_l[["Emprestimo"]]) + 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Emprestimo"]]),
    #        gridExpand = T
    #      )
    #      #### Definindo formatação das colunas
    #      setColWidths(xlsx,
    #        "Emprestimo PJ",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Emprestimo"]]),
    #        widths = 18
    #      )
    #      #### Criando um filtro para a planilha
    #      addFilter(xlsx,
    #        "Emprestimo PJ",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Emprestimo"]]),
    #        rows = 1
    #      )
    #      #### Definindo formatação da linha de cabeçalho
    #      addStyle(xlsx,
    #        sheet = "Emprestimo PJ",
    #        style = createStyle(
    #          border = "TopBottomLeftRight",
    #          fontSize = 12,
    #          fontColour = "white",
    #          halign = "center",
    #          valign = "center",
    #          textDecoration = "bold",
    #          fgFill = "darkgray",
    #          wrapText = T
    #        ),
    #        rows = 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Emprestimo"]]),
    #        gridExpand = T
    #      )
    #      ### Consolidado
    #      #### Criando a aba "Consolidado"
    #      addWorksheet(xlsx, "Consolidado", gridLines = FALSE)
    #      #### Populando a aba
    #      writeData(xlsx, "Consolidado", dados.arquivo.ecn_l[["Consolidado"]])
    #      #### Definindo formatação das bordas
    #      addStyle(xlsx,
    #        sheet = "Consolidado",
    #        style = createStyle(border = "TopBottomLeftRight"),
    #        rows = 1:nrow(dados.arquivo.ecn_l[["Consolidado"]]) + 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Consolidado"]]),
    #        gridExpand = T
    #      )
    #      #### Definindo formatação das colunas
    #      setColWidths(xlsx,
    #        "Consolidado",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Consolidado"]]),
    #        widths = 18
    #      )
    #      #### Criando um filtro para a planilha
    #      addFilter(xlsx,
    #        "Consolidado",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Consolidado"]]),
    #        rows = 1
    #      )
    #      #### Definindo formatação da linha de cabeçalho
    #      addStyle(xlsx,
    #        sheet = "Consolidado",
    #        style = createStyle(
    #          border = "TopBottomLeftRight",
    #          fontSize = 12,
    #          fontColour = "white",
    #          halign = "center",
    #          valign = "center",
    #          textDecoration = "bold",
    #          fgFill = "darkgray",
    #          wrapText = T
    #        ),
    #        rows = 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Consolidado"]]),
    #        gridExpand = T
    #      )
    #      ### Unidades
    #      #### Criando a aba "Unidades"
    #      addWorksheet(xlsx, "Unidades", gridLines = FALSE)
    #      #### Populando a aba
    #      writeData(xlsx, "Unidades", dados.arquivo.ecn_l[["Unidades"]])
    #      #### Definindo formatação das bordas
    #      addStyle(xlsx,
    #        sheet = "Unidades",
    #        style = createStyle(border = "TopBottomLeftRight"),
    #        rows = 1:nrow(dados.arquivo.ecn_l[["Unidades"]]) + 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Unidades"]]),
    #        gridExpand = T
    #      )
    #      #### Definindo formatação das colunas
    #      setColWidths(xlsx,
    #        "Unidades",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Unidades"]]),
    #        widths = 18
    #      )
    #      #### Criando um filtro para a planilha
    #      addFilter(xlsx,
    #        "Unidades",
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Unidades"]]),
    #        rows = 1
    #      )
    #      #### Definindo formatação da linha de cabeçalho
    #      addStyle(xlsx,
    #        sheet = "Unidades",
    #        style = createStyle(
    #          border = "TopBottomLeftRight",
    #          fontSize = 12,
    #          fontColour = "white",
    #          halign = "center",
    #          valign = "center",
    #          textDecoration = "bold",
    #          fgFill = "darkgray",
    #          wrapText = T
    #        ),
    #        rows = 1,
    #        cols = 1:ncol(dados.arquivo.ecn_l[["Unidades"]]),
    #        gridExpand = T
    #      )
    #      #### Congelar a primeira linha
    #      freezePane(xlsx, "Unidades", firstRow = TRUE)
    #      ### Nomeando o arquivo
    #      nome.arquivo <-
    #        paste0(
    #          dirname(caminho.ecn_c),
    #          "/",
    #          empreendimento_c,
    #          " ECN ",
    #          format(data_consulta_p, "%Y_%m_%d %H_%M_%S"),
    #          ".xlsx"
    #        )
    #      ### Salvando o arquivo .xlsx
    #      saveWorkbook(xlsx, nome.arquivo, overwrite = T)
    #    }
    ### Reportando erros comuns
    if (is.null(dados.arquivo.ecn_l)) {
      stop(paste0(
        "Erro ao tentar extrair dados de ",
        caminho.ecn_c
      ))
    }
    ### Definindo a lista como o retorno da função e_cef_ecn()
    return(dados.arquivo.ecn_l)
  }
# Teste -------------------------------------------------------------------

# caminho.ecn_c <- caminhos.ecn_c[14]
#  paste0(
#    "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/",
#    "Relatorios - CIWEB/3. UP Estação Vila Sonia/02.01.25/ECN/",
#    "20250102_121821_000_PP_177770020232_RELATORIO_EMPREENDIMENTO_CONSTRUCAO.PDF"
#  )
# str(e_cef_ecn(caminho.ecn_c))

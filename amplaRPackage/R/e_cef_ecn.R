#' @title Extração dos dados de um arquivo ECN
#'
#' @description
#' A função **e_cef_ecn** extrai e organiza dados dos arquivos ECN em PDF,
#' consolidando informações sobre empreendimentos, empréstimos, dados
#' consolidados e unidades.
#'
#' @param f_caminho.arquivo_c Caminho para o arquivo PDF do ECN.
#' @param xlsx Lógico. Se `TRUE`, salva o resultado em um arquivo .xlsx.
#'
#' @details
#' A função lê o arquivo PDF, modela as informações e retorna uma lista
#' com dados de Empreendimento, Empréstimo, Consolidados e Unidades, baseada
#' no layout do extrato da CEF.
#'
#' @return Uma lista contendo tibbles de dados segmentados em diversos tópicos.
#'
#' @examples
#' \dontrun{
#' f_caminho.arquivo_c <- "meu_arquivo_ecn.pdf"
#' resultado <- e_cef_ecn(f_caminho.arquivo_c, xlsx = TRUE)
#' print(resultado)
#' }
#'
#' @importFrom dplyr mutate nth select
#' @importFrom pdftools pdf_text
#' @importFrom purrr keep
#' @importFrom stringr str_split str_remove_all str_replace str_replace_all
#' @importFrom stringr str_detect str_which str_starts
#' @importFrom tidyr separate_wider_delim
#'
#' @export

# Criando função para extrair os dados dos arquivos ECN em PDF
e_cef_ecn <-
  function(f_caminho.arquivo_c, xlsx = FALSE) {
    # paginas_l e linhas_vc -------------------------------------------------

    ## Extraindo PDF para uma lista contendo suas páginas
    paginas_l <-
      pdf_text(f_caminho.arquivo_c) %>%
      ### Separando o texto por linhas
      str_split("\n")
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
          "contrato",
          "apf",
          "valor.aporte",
          "unidades.total",
          "unidades.comercializadas",
          "unidades.financiadas.construcao",
          "unidades.complementares",
          "data.termino.obra",
          "data.ini.enc.fiador"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        empreendimento = empreendimento_c,
        data.consulta = data_consulta_p,
        arquivo = f_caminho.arquivo_c
      ) %>%
      select(empreendimento, everything(), data.consulta, arquivo) %>%
      mutate(
        valor.aporte =
          valor.aporte %>%
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
          "numero",
          "data.assinatura",
          "valor.emprestimo",
          "valor.reduzido",
          "valor.utilizado",
          "saldo.devedor"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        `valor.emprestimo` =
          `valor.emprestimo` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `valor.reduzido` =
          `valor.reduzido` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `valor.utilizado` =
          `valor.utilizado` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `saldo.devedor` =
          `saldo.devedor` %>%
            str_remove_all("\\.") %>%
            str_replace("\\,", "\\.") %>%
            as.numeric(),
        `linha.financiamento` = as.character(financiamento.linha_c),
        `tipo.financiamento` = as.character(financiamento.tipo_c),
        empreendimento = empreendimento_c,
        data.consulta = data_consulta_p,
        arquivo = f_caminho.arquivo_c
      ) %>%
      select(
        empreendimento,
        everything(),
        data.consulta,
        arquivo
      )

    # Tabela "Informações consolidadas" ---------------------------------------

    ## Tabela com dados consolidados
    ### Identificando as primeiras linhas das partes da tabela de dados consolidados
    # Identificar índices de início das seções "INFORMAÇÕES CONSOLIDADAS" (pular duas linhas de cabeçalho)
    indice.consolidado.comeco_vn <- str_which(linhas_vc, "INFORMAÇÕES CONSOLIDADAS") + 2

    # Calcular a soma cumulativa das contagens de linhas para cada página
    paginas_cum <- cumsum(sapply(paginas_l, length))

    # Determinar o índice final para cada seção consolidada utilizando as informações da página
    indice.consolidado.fim_vn <- sapply(indice.consolidado.comeco_vn, function(indice_inicio) {
      pagina_atual <- encontrar_pagina(indice_inicio)
      paginas_cum[pagina_atual]
    })

    # Para cada seção, criar uma sequência do índice inicial até o índice final correspondente
    lista_indices_consolidado <- mapply(function(inicio, fim) {
      seq(inicio, fim)
    }, inicio = indice.consolidado.comeco_vn, fim = indice.consolidado.fim_vn, SIMPLIFY = FALSE)

    # Combinar todos os índices; remover o último elemento caso seja uma linha de separação indesejada
    indice.consolidado_vn <- unlist(lista_indices_consolidado)[-length(unlist(lista_indices_consolidado))]
    ### Criando a tabela que será preenchida com os dados consolidados
    consolidado_df <-
      ### Criando tabela a partir do texto
      tibble(line = linhas_vc[indice.consolidado_vn]) %>%
      ### Separando as linhas de texto em colunas
      separate_wider_delim(
        cols = everything(),
        names = c(
          "periodo",
          "unidade",
          "valor.creditado",
          "valor.desbloqueado",
          "valor.amortizado",
          "encargo.quitado.pj"
        ),
        delim = " "
      ) %>%
      ### Garantindo que as colunas sejam da classe adequada
      mutate(
        `periodo` = format(as.Date(`periodo`, format = "%d/%m/%Y"), "%d/%m/%Y"),
        unidade = as.character(unidade),
        `valor.creditado` = str_remove_all(`valor.creditado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `valor.desbloqueado` = str_remove_all(`valor.desbloqueado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `valor.amortizado` = str_remove_all(`valor.amortizado`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `encargo.quitado.pj` = str_remove_all(`encargo.quitado.pj`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        empreendimento = empreendimento_c,
        data.consulta = data_consulta_p,
        arquivo = f_caminho.arquivo_c
      ) %>%
      select(
        empreendimento,
        everything(),
        data.consulta,
        arquivo
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
          "contrato",
          "tp",
          "data.assinatura",
          "data.inclusao",
          "data.registro",
          "financiamento",
          "desconto.subsidio",
          "fgts",
          "recursos.proprios",
          "compra/venda",
          "valor.avaliacao",
          "valor.liberado.terreno",
          "valor.liberado.obra",
          "amortizacao"
        ),
        delim = " "
      ) %>%
      #### Garantindo que as colunas sejam das classes adequadas
      mutate(
        contrato = as.character(contrato),
        tp = as.character(tp),
        `data.assinatura` = as.Date(`data.assinatura`,
          format = "%d/%m/%Y"
        ),
        `data.inclusao` = as.Date(`data.inclusao`,
          format = "%d/%m/%Y"
        ),
        `data.registro` = as.Date(
          `data.registro`,
          "%d/%m/%Y"
        ),
        financiamento = str_remove_all(financiamento, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `desconto.subsidio` = str_remove_all(`desconto.subsidio`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        fgts = str_remove_all(fgts, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `recursos.proprios` = str_remove_all(`recursos.proprios`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `compra/venda` = str_remove_all(`compra/venda`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `valor.avaliacao` = str_remove_all(`valor.avaliacao`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `valor.liberado.terreno` = str_remove_all(`valor.liberado.terreno`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        `valor.liberado.obra` = str_remove_all(`valor.liberado.obra`, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        amortizacao = str_remove_all(amortizacao, "\\.") %>%
          str_replace_all("\\,", "\\.") %>%
          as.numeric(),
        empreendimento = empreendimento_c,
        data.consulta = data_consulta_p,
        arquivo = f_caminho.arquivo_c
      ) %>%
      select(
        empreendimento,
        everything(),
        data.consulta,
        arquivo
      )
    ## Criando a lista a ser retornada pela função e_cef_ecn()
    dados.arquivo.ecn_l <- list(
      ecn_e = empreendimento_df,
      ecn_pj = emprestimo_df,
      ecn_c = consolidado_df,
      ecn_u = unidades_df
    )

    ### Reportando erros comuns
    if (is.null(dados.arquivo.ecn_l)) {
      stop(paste0(
        "Erro ao tentar extrair dados de ",
        f_caminho.arquivo_c
      ))
    }
    ### Definindo a lista como o retorno da função e_cef_ecn()
    return(dados.arquivo.ecn_l)
  }

# Teste -------------------------------------------------------------------

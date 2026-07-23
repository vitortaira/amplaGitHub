# Funções auxiliares para cruzamento flexível ---------------------------------

#' Encontra subconjunto de valores que soma ao alvo
#'
#' @param alvo Valor numérico alvo.
#' @param valores Vetor numérico de valores candidatos.
#' @return Vetor de índices cuja soma corresponde ao alvo,
#'   ou \code{integer(0)} se nenhuma combinação for encontrada.
#' @keywords internal
encontrar_combinacao <- function(alvo, valores) {
  if (is.na(alvo)) {
    return(integer(0))
  }
  validos <- which(!is.na(valores))
  n <- length(validos)
  if (n == 0) {
    return(integer(0))
  }
  for (k in seq_len(n)) {
    combos <- utils::combn(n, k, simplify = FALSE)
    for (combo in combos) {
      if (abs(sum(valores[validos[combo]]) - alvo) < 0.005) {
        return(validos[combo])
      }
    }
  }
  integer(0)
}

#' Cruza valores de extratos com cmfcns permitindo 1:N (subset-sum)
#'
#' Primeiro tenta correspondências exatas 1:1. Para valores restantes,
#' busca subconjuntos de cmfcns cuja soma corresponde ao valor do extrato.
#' As datas dos lançamentos podem divergir em até \code{tolerancia_dias}
#' (padrão 3) para acomodar defasagens entre as duas fontes.
#'
#' @param ext_vals Vetor numérico de valores dos extratos.
#' @param cmf_vals Vetor numérico de valores dos CMF_CNs.
#' @param ext_datas Vetor de datas (Date) dos extratos.
#' @param cmf_datas Vetor de datas (Date) dos CMF_CNs.
#' @param tolerancia_dias Inteiro. Diferença máxima permitida em dias entre
#'   as datas dos extratos e dos CMF_CNs. Padrão 3.
#' @return Lista de listas com campos \code{ext} (índice do extrato),
#'   \code{cmf} (índice do cmfcn) e \code{tipo} ("exato" ou "combinado").
#' @keywords internal
cruzar_grupo <- function(ext_vals, cmf_vals,
                         ext_datas, cmf_datas,
                         tolerancia_dias = 3) {
  n_ext <- length(ext_vals)
  n_cmf <- length(cmf_vals)
  ext_usado <- logical(n_ext)
  cmf_usado <- logical(n_cmf)
  pares <- list()

  # Verifica se a diferença em dias está dentro da tolerância
  datas_proximas <- function(d1, d2) {
    if (is.na(d1) || is.na(d2)) {
      return(FALSE)
    }
    abs(as.numeric(difftime(d1, d2, units = "days"))) <= tolerancia_dias
  }

  # Passo 1: correspondências exatas 1:1 (valor igual + datas próximas)
  for (i in seq_len(n_ext)) {
    for (j in seq_len(n_cmf)) {
      if (!ext_usado[i] && !cmf_usado[j] &&
        !is.na(ext_vals[i]) && !is.na(cmf_vals[j]) &&
        abs(ext_vals[i] - cmf_vals[j]) < 0.005 &&
        datas_proximas(ext_datas[i], cmf_datas[j])) {
        ext_usado[i] <- TRUE
        cmf_usado[j] <- TRUE
        pares <- c(pares, list(list(ext = i, cmf = j, tipo = "exato")))
        break
      }
    }
  }

  # Passo 2: combinações (subset-sum) para restantes; todos os CMFs do
  # subconjunto precisam estar dentro da tolerância da data do extrato
  cmf_livres <- which(!cmf_usado)
  for (i in which(!ext_usado)) {
    if (length(cmf_livres) == 0) break
    proximos <- vapply(
      cmf_livres,
      \(j) datas_proximas(ext_datas[i], cmf_datas[j]),
      logical(1)
    )
    cmf_candidatos <- cmf_livres[proximos]
    if (length(cmf_candidatos) == 0) next
    idx <- encontrar_combinacao(ext_vals[i], cmf_vals[cmf_candidatos])
    if (length(idx) > 0) {
      cmf_sel <- cmf_candidatos[idx]
      for (j in cmf_sel) {
        pares <- c(pares, list(list(ext = i, cmf = j, tipo = "combinado")))
      }
      ext_usado[i] <- TRUE
      cmf_usado[cmf_sel] <- TRUE
      cmf_livres <- setdiff(cmf_livres, cmf_sel)
    }
  }

  pares
}

#' @title Cruzamento de dados dos extratos da CEF e relatórios CMF_CN
#'
#' @description
#' A função **r_xcef** realiza o cruzamento dos dados dos
#' extratos da CEF e dos relatórios CMF_CN, gerando um arquivo consolidado
#' em formato `.xlsx`.
#'
#' @param f_caminho.pasta.extratos_c Caminho para a pasta "Relatorios - Extratos".
#' @param f_caminho.pasta.ciweb_c Caminho para a pasta "Relatorios - CIWEB".
#' @param xlsx Lógico. Se TRUE, gera um arquivo Excel (.xlsx) com os dados cruzados.
#'   Valor padrão: FALSE.
#'
#' @details
#' A função executa as seguintes etapas:
#' 1. Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos".
#' 2. Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB".
#' 3. Cruza os dados consolidados com base em campos comuns.
#' 4. Se `xlsx = TRUE`, gera um arquivo `.xlsx` com os dados cruzados na pasta "Extratos conciliados".
#'
#' @return
#' Retorna um tibble com os dados cruzados dos extratos da CEF e relatórios CMF_CN.
#'
#' @examples
#' \dontrun{
#' f_caminho.pasta.extratos_c <- "caminho/para/a/pasta/Relatorios - Extratos"
#' f_caminho.pasta.ciweb_c <- "caminho/para/a/pasta/Relatorios - CIWEB"
#'
#' # Apenas retornar os dados cruzados
#' resultado <- r_xcef(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c)
#'
#' # Retornar os dados e gerar arquivo Excel
#' resultado <- r_xcef(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c, xlsx = TRUE)
#' print(resultado)
#' }
#'
#' @importFrom fs dir_ls
#' @importFrom here here
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rename bind_rows bind_cols filter group_by
#'   summarise left_join select
#' @importFrom stringr str_detect str_ends str_pad str_sub
#'
#' @export

r_xcef <-
  function(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c, xlsx = FALSE) {
    # EPRs para enriquecer extratos com nome do mutuário
    eprs_t <- e_cef_eprs()
    eprs_join_t <- eprs_t %>%
      mutate(contrato = str_sub(contrato, -5, -1)) %>%
      rename(contrato.5 = contrato, arquivo.epr = arquivo) %>%
      dplyr::select(empresa, contrato.5, nome.mutuario, arquivo.epr) %>%
      distinct()

    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    extratos_t <- e_cef_xcefs() %>%
      left_join(eprs_join_t, by = c("empresa", "contrato.5")) %>%
      mutate(
        # Corrigir conversões de data problemáticas
        data.lancamento = tryCatch(
          if (is.numeric(data.lancamento)) {
            as.Date(as.integer(data.lancamento), origin = "1899-12-30")
          } else {
            as.Date(data.lancamento)
          },
          error = function(e) as.Date(NA),
          warning = function(w) as.Date(NA)
        ),
        data.movimentacao = tryCatch(
          if (is.numeric(data.movimentacao)) {
            as.Date(as.integer(data.movimentacao), origin = "1899-12-30")
          } else {
            as.Date(data.movimentacao)
          },
          error = function(e) as.Date(NA),
          warning = function(w) as.Date(NA)
        )
      )

    # Consolida os dados dos relatórios CMF_CN na pasta "Relatorios - CIWEB"
    cmfcns_t <- e_cef_cmfcns() %>%
      mutate(
        contrato.5 = str_sub(contrato.6, start = -5, end = -1)
      )
    # Cruzamento flexível (permite 1:N via subset-sum) ----------------------
    extratos_t$.id_ext <- seq_len(nrow(extratos_t))
    cmfcns_t$.id_cmf <- seq_len(nrow(cmfcns_t))

    # Chave de agrupamento (empresa + contrato). A data é tratada com
    # tolerância de 3 dias dentro de cruzar_grupo() para acomodar
    # defasagens entre as fontes (ex.: lançamento em 31/12 no extrato
    # vs. 03/01 no CMF_CN).
    chave_ext <- ifelse(
      is.na(extratos_t$empresa) | is.na(extratos_t$contrato.5),
      NA_character_,
      paste(extratos_t$empresa, extratos_t$contrato.5, sep = "|")
    )
    chave_cmf <- ifelse(
      is.na(cmfcns_t$empresa) | is.na(cmfcns_t$contrato.5),
      NA_character_,
      paste(cmfcns_t$empresa, cmfcns_t$contrato.5, sep = "|")
    )

    # Encontrar pares por grupo
    grupos_comuns <- intersect(
      unique(na.omit(chave_ext)),
      unique(na.omit(chave_cmf))
    )
    pares_t <- lapply(grupos_comuns, function(g) {
      ext_idx <- which(chave_ext == g)
      cmf_idx <- which(chave_cmf == g)
      matches <- cruzar_grupo(
        ext_vals = extratos_t$valor[ext_idx],
        cmf_vals = cmfcns_t$valor[cmf_idx],
        ext_datas = extratos_t$data.movimentacao[ext_idx],
        cmf_datas = cmfcns_t$data.movimento[cmf_idx],
        tolerancia_dias = 3
      )
      if (length(matches) == 0) {
        return(NULL)
      }
      tibble(
        .id_ext = vapply(matches, \(m) ext_idx[m$ext], integer(1)),
        .id_cmf = vapply(matches, \(m) cmf_idx[m$cmf], integer(1)),
        tipo.cruzamento = vapply(matches, \(m) m$tipo, character(1))
      )
    }) %>% bind_rows()

    # Montar tabela cruzada
    if (nrow(pares_t) > 0) {
      cols_sufixo <- c("natureza", "data.lancamento", "arquivo", "valor")
      ext_parte <- extratos_t[pares_t$.id_ext, ]
      cmf_parte <- cmfcns_t[pares_t$.id_cmf, ] %>%
        select(-empresa, -contrato.5, -.id_cmf)
      names(ext_parte) <- ifelse(
        names(ext_parte) %in% cols_sufixo,
        paste0(names(ext_parte), ".xcef"),
        names(ext_parte)
      )
      names(cmf_parte) <- ifelse(
        names(cmf_parte) %in% cols_sufixo,
        paste0(names(cmf_parte), ".cmfcn"),
        names(cmf_parte)
      )
      extratos.cruzados_t <- bind_cols(
        pares_t %>% select(tipo.cruzamento),
        ext_parte,
        cmf_parte
      ) %>%
        select(
          data.movimentacao, empresa, contrato.5,
          valor.xcef, valor.cmfcn, tipo.cruzamento,
          everything(), -.id_ext
        )
    } else {
      extratos.cruzados_t <- tibble()
    }
    # Somas de repasse do ano corrente por (contrato, empresa)
    ano_atual <- format(Sys.Date(), "%Y")
    col_soma_xcef <- paste0("soma.repasse.", ano_atual, ".xcef")
    col_soma_cmfcn <- paste0("soma.repasse.", ano_atual, ".cmfcn")

    soma_repasse_xcef <- extratos_t %>%
      filter(
        natureza == "repasse.cef",
        !is.na(data.movimentacao),
        format(data.movimentacao, "%Y") == ano_atual
      ) %>%
      group_by(contrato.5, empresa) %>%
      summarise(!!col_soma_xcef := sum(valor, na.rm = TRUE),
        .groups = "drop"
      )
    soma_repasse_cmfcn <- cmfcns_t %>%
      filter(
        natureza %in% c(
          "repasse.cef.obra",
          "repasse.cef.terreno",
          "remuneracao.terreno",
          "remuneracao.venda"
        ),
        !is.na(data.movimento),
        format(data.movimento, "%Y") == ano_atual
      ) %>%
      group_by(contrato.5, empresa) %>%
      summarise(!!col_soma_cmfcn := sum(valor, na.rm = TRUE),
        .groups = "drop"
      )

    # Marcar linhas cruzadas em extratos_t e cmfcns_t
    extratos_t %<>% mutate(
      cruzada = if_else(.id_ext %in% pares_t$.id_ext, "sim", "não")
    ) %>%
      select(
        contrato.5, data.movimentacao, valor, empresa, natureza, conta.interno,
        cruzada, data.lancamento, documento, descricao, saldo, conta, agencia,
        produto, cnpj, cpf.cnpj, nome.razao, periodo.inicio, periodo.fim,
        data.consulta, arquivo
      )
    cmfcns_t %<>% mutate(
      cruzada = if_else(.id_cmf %in% pares_t$.id_cmf, "sim", "não")
    ) %>%
      select(
        contrato.5, data.movimento, valor, empresa, natureza, cruzada,
        data.lancamento, contrato, lancamentos, np, `conta.sidec/nsgd`,
        situacao, mot, arquivo
      )
    extratos.cruzados_t %<>%
      rename(
        valor.extrato = valor.xcef,
        arquivo.extrato = arquivo.xcef,
        data.lancamento.extrato = data.lancamento.xcef,
        natureza.extrato = natureza.xcef
      ) %>%
      left_join(soma_repasse_xcef, by = c("contrato.5", "empresa")) %>%
      left_join(soma_repasse_cmfcn, by = c("contrato.5", "empresa")) %>%
      mutate(
        across(
          all_of(c(col_soma_xcef, col_soma_cmfcn)),
          \(x) replace(x, is.na(x), 0)
        ),
        checar = {
          v1 <- .data[[col_soma_xcef]]
          v2 <- .data[[col_soma_cmfcn]]
          contratos_pj <- c(
            e_cef_nplpjs()$contrato.6.ultimo,
            e_cef_nplpjs()$contrato.6.penultimo
          )
          contratos_pj5 <- str_sub(contratos_pj, start = -5, end = -1)
          denom <- pmax(abs(v1), abs(v2), 1e-10)
          proximo <- abs(v1 - v2) / denom < 0.001
          eh_pj <- contrato.5 %in% contratos_pj5
          if_else(proximo | eh_pj, "ok", "diverge")
        }
      ) %>%
      select(
        contrato.5,
        data.movimentacao.extrato = data.movimentacao,
        data.movimentacao.cmfcn = data.movimento,
        valor.extrato,
        valor.cmfcn, empresa,
        nome.mutuario, tipo.cruzamento, natureza.extrato, conta.interno,
        all_of(c(col_soma_xcef, col_soma_cmfcn)), checar,
        data.lancamento.extrato, documento, descricao, saldo, conta, agencia,
        produto, periodo.inicio, periodo.fim, data.consulta, arquivo.extrato,
        natureza.cmfcn, contrato, data.lancamento.cmfcn, lancamentos, np,
        `conta.sidec/nsgd`, situacao, mot, arquivo.cmfcn, arquivo.epr
      )
    # Salvando num xlsx -------------------------------------------------------

    if (xlsx) {
      # Definindo o nome do arquivo dinamicamente
      nome.xlsx_c <-
        paste0(
          "extratos_cruzados-",
          format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
          ".xlsx"
        )

      # Caminho da pasta de destino
      caminho_pasta_extratos_cruzados <- file.path(
        "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Relatorios - Extratos/Extratos cruzados"
      )

      # Preparar dados para as três abas
      dados_xlsx <- list(
        Cruzados = extratos.cruzados_t,
        Extratos = extratos_t,
        CMF_CNs = cmfcns_t,
        EPRs = eprs_t %>%
          select(-arquivo.tabela.tipo, -arquivo.tipo, -arquivo.fonte)
      )

      # Cores das abas
      cores_abas <- c(
        Cruzados = "purple",
        Extratos = "red",
        CMF_CNs = "darkblue",
        EPRs = "lightblue"
      )

      # Configuração de larguras específicas por coluna
      # Todas as abas têm Cliente com 45
      larguras_spec <- c(
        "Cliente" = 45,
        "Histórico" = 25,
        "nome.mutuario" = 30
      )

      # Colunas com largura automática ajustada ao conteúdo
      colunas_auto <- c(
        "data.movimentacao",
        "data.movimento",
        "data.movimentacao.extrato",
        "data.movimentacao.cmfcn",
        "data.lancamento.extrato",
        "descricao",
        "data.lancamento.cmfcn",
        "lancamentos",
        "conta.sidec/nsgd"
      )

      # Configuração de alinhamento específico por coluna
      # Cliente, LANCAMENTOS e CONTA SIDEC/NSGD são alinhados à esquerda
      alinhas_spec <- c(
        "Cliente" = "left",
        "LANCAMENTOS" = "left",
        "CONTA SIDEC/NSGD" = "left",
        "Histórico" = "left"
      )

      # Configuração de colunas monetárias
      colunas_monetarias <- c(
        "valor", "valor.extrato", "valor.cmfcn", "saldo",
        col_soma_xcef, col_soma_cmfcn
      )

      # Configuração de colunas de data
      colunas_datas <- c(
        "data.lancamento",
        "data.lancamento.cmfcn",
        "data.lancamento.extrato",
        "data.movimentacao",
        "data.movimento",
        "data.movimentacao.extrato",
        "data.movimentacao.cmfcn",
        "periodo.fim",
        "periodo.inicio"
      )

      # Configuração de cabeçalhos customizados por aba com cores específicas por coluna
      # Aba "Cruzados": purple para algumas colunas, red para outras, blue para outras
      col_headers_config <- list(
        Cruzados = list(
          # Purple headers with white font
          "contrato.5" = list(colour = "purple", font_colour = "white", font_size = 12),
          "data.movimentacao.extrato" = list(colour = "red", font_colour = "white", font_size = 12),
          "data.movimentacao.cmfcn" = list(colour = "darkblue", font_colour = "white", font_size = 12),
          "valor.extrato" = list(colour = "purple", font_colour = "white", font_size = 12),
          "valor.cmfcn" = list(colour = "purple", font_colour = "white", font_size = 12),
          # Gray headers
          "empresa" = list(colour = "lightgray", font_size = 12),
          "nome.mutuario" = list(colour = "lightblue", font_size = 12),
          "natureza.extrato" = list(colour = "lightgray", font_size = 12),
          "conta.interno" = list(colour = "lightgray", font_size = 12),
          "tipo.cruzamento" = list(colour = "lightgray", font_size = 12),
          "arquivo.extrato" = list(colour = "lightgray", font_size = 12),
          "arquivo.cmfcn" = list(colour = "lightgray", font_size = 12),
          "arquivo.epr" = list(colour = "lightgray", font_size = 12),
          "natureza.cmfcn" = list(colour = "lightgray", font_size = 12),
          # Red headers with white font
          "data.lancamento.extrato" = list(colour = "red", font_colour = "white", font_size = 12),
          "documento" = list(colour = "red", font_colour = "white", font_size = 12),
          "descricao" = list(colour = "red", font_colour = "white", font_size = 12),
          "saldo" = list(colour = "red", font_colour = "white", font_size = 12),
          "conta" = list(colour = "red", font_colour = "white", font_size = 12),
          "agencia" = list(colour = "red", font_colour = "white", font_size = 12),
          "produto" = list(colour = "red", font_colour = "white", font_size = 12),
          "periodo.inicio" = list(colour = "red", font_colour = "white", font_size = 12),
          "periodo.fim" = list(colour = "red", font_colour = "white", font_size = 12),
          "data.consulta" = list(colour = "red", font_colour = "white", font_size = 12),
          # Blue headers with white font
          "contrato" = list(colour = "blue", font_colour = "white", font_size = 12),
          "data.lancamento.cmfcn" = list(colour = "blue", font_colour = "white", font_size = 12),
          "lancamentos" = list(colour = "blue", font_colour = "white", font_size = 12),
          "np" = list(colour = "blue", font_colour = "white", font_size = 12),
          "conta.sidec/nsgd" = list(colour = "blue", font_colour = "white", font_size = 12),
          "situacao" = list(colour = "blue", font_colour = "white", font_size = 12),
          "mot" = list(colour = "blue", font_colour = "white", font_size = 12)
        ),
        Extratos = list(
          # Purple headers with white font
          "contrato.5" = list(colour = "purple", font_colour = "white", font_size = 12),
          "data.movimentacao" = list(colour = "purple", font_colour = "white", font_size = 12),
          "valor" = list(colour = "purple", font_colour = "white", font_size = 12),
          # Gray headers
          "empresa" = list(colour = "lightgray", font_size = 12),
          "natureza" = list(colour = "lightgray", font_size = 12),
          "conta.interno" = list(colour = "lightgray", font_size = 12),
          "cruzada" = list(colour = "lightgray", font_size = 12),
          "arquivo" = list(colour = "lightgray", font_size = 12),
          # Red headers with white font (default for other columns)
          all = list(colour = "red", font_colour = "white", font_size = 12)
        ),
        CMF_CNs = list(
          # Purple headers with white font
          "contrato.5" = list(colour = "purple", font_colour = "white", font_size = 12),
          "data.movimento" = list(colour = "purple", font_colour = "white", font_size = 12),
          "valor" = list(colour = "purple", font_colour = "white", font_size = 12),
          # Gray headers
          "empresa" = list(colour = "lightgray", font_size = 12),
          "natureza" = list(colour = "lightgray", font_size = 12),
          "cruzada" = list(colour = "lightgray", font_size = 12),
          "arquivo" = list(colour = "lightgray", font_size = 12),
          # Blue headers with white font (default for other columns)
          all = list(colour = "blue", font_colour = "white", font_size = 12)
        )
      )
      # Headers dinâmicos das somas de repasse (nome depende do ano)
      col_headers_config$Cruzados[[col_soma_xcef]] <-
        list(colour = "lightgray", font_size = 12)
      col_headers_config$Cruzados[[col_soma_cmfcn]] <-
        list(colour = "lightgray", font_size = 12)
      col_headers_config$Cruzados[["checar"]] <-
        list(colour = "lightgray", font_size = 12)

      # EPRs headers: lightgray para empresa e arquivo, lightblue para demais
      col_headers_config$EPRs <- list(
        "empresa" = list(colour = "lightgray", font_size = 12),
        "arquivo" = list(colour = "lightgray", font_size = 12),
        all = list(colour = "lightblue", font_size = 12)
      )

      # Aplicar a configuração de cores para todos os cabeçalhos de cada aba
      # Para abas com 'all', expandir a cor padrão a todas as colunas
      for (aba in c("Extratos", "CMF_CNs", "EPRs")) {
        if (aba %in% names(col_headers_config)) {
          df_aba <- dados_xlsx[[aba]]
          config_aba <- col_headers_config[[aba]]
          cor_padrao <- config_aba[["all"]]
          col_headers_config[[aba]] <- list()
          for (col in colnames(df_aba)) {
            # Se a coluna tem configuração específica, usar; senão usar a padrão
            if (col %in% names(config_aba) && col != "all") {
              col_headers_config[[aba]][[col]] <- config_aba[[col]]
            } else {
              col_headers_config[[aba]][[col]] <- cor_padrao
            }
          }
        }
      }

      # Gerar a planilha usando gerar_xlsx
      gerar_xlsx(
        data = dados_xlsx,
        tab_names = names(dados_xlsx),
        tab_colours = cores_abas,
        col_width_def = 18,
        col_width_spec = larguras_spec,
        col_width_auto = colunas_auto,
        col_headers = col_headers_config,
        col_monetary = colunas_monetarias,
        col_dates = colunas_datas,
        col_align = alinhas_spec,
        save = list(nome.xlsx_c, caminho_pasta_extratos_cruzados)
      )
    }
    return(extratos.cruzados_t)
  }

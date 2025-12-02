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
#' @importFrom dplyr mutate rename bind_rows inner_join
#' @importFrom stringr str_detect str_ends str_pad str_sub
#'
#' @export

r_xcef <-
  function(f_caminho.pasta.extratos_c, f_caminho.pasta.ciweb_c, xlsx = FALSE) {
    # Consolida os dados dos extratos da CEF na pasta "Relatorios - Extratos"
    extratos_t <- e_cef_xcefs() %>%
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
    # Cruza os dados consolidados
    extratos.cruzados_t <-
      inner_join(
        extratos_t,
        cmfcns_t,
        by = c(
          "data.movimentacao" = "data.movimento", "empresa", "contrato.5",
          "valor"
        ),
        suffix = c(".xcef", ".cmfcn")
      ) %>%
      select(
        # Interseção
        data.movimentacao, empresa, contrato.5, valor,
        # Incluir todas as outras colunas
        everything()
      ) %>%
      mutate(
        id_xcef = paste0(
          # Interseção
          contrato.5, valor
        ),
        id_cmfcn = paste0(
          # Interseção
          contrato.5, valor
        )
      )
    # Colunas que identificam linhas cruzadas em extratos_t e cmfcns_t
    extratos_t %<>% mutate(
      cruzada = if_else(
        paste0(contrato.5, valor) %in% paste0(extratos.cruzados_t$contrato.5, extratos.cruzados_t$valor),
        "sim",
        "não"
      )
    )
    cmfcns_t %<>% mutate(
      cruzada = if_else(
        paste0(contrato.5, valor) %in% paste0(extratos.cruzados_t$contrato.5, extratos.cruzados_t$valor),
        "sim",
        "não"
      )
    )
    extratos.cruzados_t %<>%
      rename(
        arquivo.extrato = arquivo.xcef,
        data.lancamento.extrato = data.lancamento.xcef,
        natureza.extrato = natureza.xcef
      ) %>%
      select(
        contrato.5, data.movimentacao, valor, empresa, natureza.extrato,
        conta.interno, data.lancamento.extrato, documento, descricao, saldo,
        conta, agencia, produto, periodo.inicio, periodo.fim, data.consulta,
        arquivo.extrato, contrato, data.lancamento.cmfcn, lancamentos,
        natureza.cmfcn, np, `conta.sidec/nsgd`, situacao, mot, arquivo.cmfcn
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
        CMF_CNs = cmfcns_t
      )

      # Cores das abas
      cores_abas <- c(
        Cruzados = "purple",
        Extratos = "red",
        CMF_CNs = "blue"
      )

      # Configuração de larguras específicas por coluna
      # Todas as abas têm CONTA SIDEC/NSGD com 25, Cliente/LANCAMENTOS com 45
      larguras_spec <- c(
        "CONTA SIDEC/NSGD" = 25,
        "Cliente" = 45,
        "LANCAMENTOS" = 45,
        "Histórico" = 25
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
      colunas_monetarias <- c("valor", "saldo")

      # Configuração de colunas de data
      colunas_datas <- c(
        "data.movimentacao",
        "data.lancamento.extrato",
        "periodo.inicio",
        "periodo.fim",
        "data.lancamento.cmfcn",
        "data.movimento"
      )

      # Configuração de cabeçalhos customizados por aba com cores específicas por coluna
      # Aba "Cruzados": purple para algumas colunas, red para outras, blue para outras
      col_headers_config <- list(
        Cruzados = list(
          # Purple headers (sem quebra de texto)
          "contrato.5" = list(colour = "purple", font_size = 12, wrapText = FALSE),
          "data.movimentacao" = list(colour = "purple", font_size = 12, wrapText = FALSE),
          "valor" = list(colour = "purple", font_size = 12, wrapText = FALSE),
          # Red headers (sem quebra de texto)
          "data.lancamento.extrato" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "documento" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "descricao" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "saldo" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "conta" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "agencia" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "produto" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "periodo.inicio" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "periodo.fim" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "data.consulta" = list(colour = "red", font_size = 12, wrapText = FALSE),
          "arquivo.xcef" = list(colour = "red", font_size = 12, wrapText = FALSE),
          # Blue headers (sem quebra de texto)
          "contrato" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "data.lancamento.cmfcn" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "lancamentos" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "np" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "conta.sidec/nsgd" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "situacao" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "mot" = list(colour = "blue", font_size = 12, wrapText = FALSE),
          "arquivo.cmfcn" = list(colour = "blue", font_size = 12, wrapText = FALSE)
        ),
        Extratos = list(
          all = list(colour = "red", font_size = 12)
        ),
        CMF_CNs = list(
          all = list(colour = "blue", font_size = 12)
        )
      )

      # Aplicar a configuração de cores para todos os cabeçalhos de cada aba
      # Para Extratos e CMF_CNs, aplicar a cor padrão a todas as colunas
      for (aba in c("Extratos", "CMF_CNs")) {
        if (aba %in% names(col_headers_config)) {
          df_aba <- dados_xlsx[[aba]]
          cor_padrao <- col_headers_config[[aba]][["all"]]
          col_headers_config[[aba]] <- list()
          for (col in colnames(df_aba)) {
            col_headers_config[[aba]][[col]] <- cor_padrao
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
        col_headers = col_headers_config,
        col_monetary = colunas_monetarias,
        col_dates = colunas_datas,
        col_align = alinhas_spec,
        save = list(nome.xlsx_c, caminho_pasta_extratos_cruzados)
      )
    }

    #  # Caminho da planilha na pasta local
    #  caminho.xlsx_c <-
    #    paste0("C:/Users/Ampla/Documents/", nome.xlsx_c) %>%
    #    normalizePath(winslash = "/", mustWork = F)
    #  # Comando no PowerShell para clicar em "Atualizar tudo" na planilha
    #  ps_cmd <-
    #    paste0(
    #      "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8;",
    #      "$excel = New-Object -ComObject Excel.Application;",
    #      "Start-Sleep -Seconds 2;",
    #      # Repare que o caminho está entre aspas simples
    #      "$wb = $excel.Workbooks.Open('", caminho.xlsx_c, "');",
    #      "$wb.RefreshAll();",
    #      "Start-Sleep -Seconds 3;",
    #      "$wb.Save();",
    #      "$wb.Close();",
    #      "$excel.Quit();",
    #      "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null;",
    #      "[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null;"
    #    )
    #  # Executar o comando do PowerShell pelo R
    #  system2("powershell", args = c("-Command", ps_cmd))
    #  # Movendo a planilha da pasta local para o OneDrive
    #  file.rename(
    #    caminho.xlsx_c,
    #    here::here("dados", "cef", "inadimplentes", "formatados", nome.xlsx_c)
    #  )
    return(extratos.cruzados_t)
  }

# Teste -------------------------------------------------------------------

# r_xcef()
# f_caminho.arquivo.extrato_cef_c <-
#   here::here(
#     "..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#     "Estação", "Fevereiro 2025", "CAIXA -  2419 - FEVEREIRO.pdf"
#   )
# f_caminho.arquivo.extrato_cef_c <-
#   here::here(
#     "..", "..", "Relatórios - Documentos", "Relatorios - Extratos",
#     "Matriz - Prudencia", "Fevereiro 2025", "EXTRATO 2429 - FEVEREIRO.pdf"
#   )
# View(extrair_dados_arquivo_extrato_cef(caminhos.extratos.cef_c[2])$Dados)
# extrato <- extrair_dados_arquivo_extrato_cef(f_caminho.arquivo.extrato_cef_c)
# teste <- extrair_dados_arquivo_extrato_cef(f_caminho.arquivo.extrato_cef_c)
# shell.exec(f_caminho.arquivo.extrato_cef_c)

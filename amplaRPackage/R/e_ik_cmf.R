#' @title Extração de dados CMF Informakon
#'
#' @description
#' A função e_ik_cmf() extrai os dados de CMF (Conta Movimento Financeiro)
#' dos arquivos na pasta "informakon", preenche-os em uma planilha xlsx
#' (opcional) e os retorna em um data frame.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{caminhos_pastas("informakon")}.
#' @param xlsx Logical. Se \code{TRUE}, cria um arquivo xlsx com os dados extraídos.
#'   Valor padrão: \code{FALSE}.
#' @param suprimir_warnings Logical. Se \code{TRUE}, suprime warnings de conversão de tipos.
#'   Valor padrão: \code{TRUE}.
#'
#' @return Data frame com dados do CMF consolidados.
#'
#' @examples
#' \dontrun{
#' # Chamando a função
#' cmf_df <- e_ik_cmf()
#'
#' # Com geração de xlsx
#' cmf_df <- e_ik_cmf(xlsx = TRUE)
#'
#' # Mostrando warnings de conversão
#' cmf_df <- e_ik_cmf(suprimir_warnings = FALSE)
#' }
#'
#' @importFrom here here
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr mutate select rename bind_rows
#' @importFrom stringr str_sub str_detect str_extract str_remove str_trim str_replace_all
#' @importFrom lubridate floor_date as_date
#' @importFrom fs dir_ls
#' @importFrom tibble tibble
#' @export
e_ik_cmf <- function(
    f_caminho.pasta.ik_c = caminhos_pastas("informakon"),
    xlsx = FALSE,
    suprimir_warnings = TRUE) {
  # Função interna para buscar o arquivo CMF mais recente
  obter_caminho_cmf <- function() {
    if (!dir.exists(f_caminho.pasta.ik_c)) {
      stop("A pasta 'informakon' não foi encontrada.")
    }

    # Busca arquivos que começam com "cmf_ik_"
    caminhos_cmf <- fs::dir_ls(f_caminho.pasta.ik_c, recurse = TRUE, type = "file")
    caminhos_cmf <- caminhos_cmf[
      basename(caminhos_cmf) %>% stringr::str_detect("^cmf_ik_") &
        basename(caminhos_cmf) %>% stringr::str_detect("\\.xlsx$")
    ]

    if (length(caminhos_cmf) == 0) {
      stop("Nenhum arquivo CMF encontrado na pasta informakon.")
    }

    # Se houver múltiplos arquivos, pega o mais recente baseado na data no nome
    if (length(caminhos_cmf) > 1) {
      data_final_por_arquivo <- sapply(caminhos_cmf, function(path) {
        # Extrai a data final do padrão cmf_ik_YYYYMMDD_YYYYMMDD.xlsx
        basename(path) %>%
          stringr::str_extract("_\\d{8}\\.xlsx$") %>%
          stringr::str_remove("\\.xlsx$") %>%
          stringr::str_remove("^_") %>%
          as.Date(format = "%Y%m%d")
      })
      indice_recente <- which.max(data_final_por_arquivo)
      caminhos_cmf[indice_recente]
    } else {
      caminhos_cmf[1]
    }
  }

  # Carrega o arquivo CMF mais recente
  caminho_arquivo_cmf <- obter_caminho_cmf()

  message(sprintf("Processando arquivo CMF: %s", basename(caminho_arquivo_cmf)))

  # Tenta diferentes métodos para ler o arquivo
  dados_cmf_lista <- list()
  abas_processadas <- 0

  # Método 1: Tentar com readxl
  tryCatch(
    {
      abas_disponiveis <- readxl::excel_sheets(caminho_arquivo_cmf)
      message(sprintf("Abas encontradas com readxl: %s", paste(abas_disponiveis, collapse = ", ")))

      # Processa cada aba
      for (aba in abas_disponiveis) {
        message(sprintf("Processando aba: %s", aba))

        tryCatch(
          {
            # Primeiro, tenta ler apenas o cabeçalho para entender a estrutura
            cabecalho <- readxl::read_excel(caminho_arquivo_cmf, sheet = aba, n_max = 0)
            num_colunas <- ncol(cabecalho)

            # Força todas as colunas a serem lidas como texto para evitar conflitos de tipo
            tipos_colunas <- rep("text", num_colunas)

            # Lê a aba com tipos forçados
            if (suprimir_warnings) {
              dados_aba <- suppressWarnings(readxl::read_excel(
                caminho_arquivo_cmf,
                sheet = aba,
                col_types = tipos_colunas
              ))
            } else {
              dados_aba <- readxl::read_excel(
                caminho_arquivo_cmf,
                sheet = aba,
                col_types = tipos_colunas
              )
            }

            # Verifica se a aba tem dados
            if (nrow(dados_aba) == 0) {
              message(sprintf("Aba '%s' está vazia, pulando...", aba))
              next
            }

            # Padroniza nomes das colunas (remove espaços e caracteres especiais)
            names(dados_aba) <- names(dados_aba) %>%
              stringr::str_trim() %>%
              stringr::str_replace_all("\\s+", ".") %>%
              stringr::str_replace_all("[^a-zA-Z0-9.]", "")

            # Adiciona metadados
            dados_aba <- dados_aba %>%
              dplyr::mutate(
                aba.origem = aba,
                arquivo = caminho_arquivo_cmf,
                arquivo.tabela.tipo = "cmf",
                arquivo.tipo = "cmf",
                arquivo.fonte = "ik"
              )

            # Armazena na lista
            dados_cmf_lista[[aba]] <- dados_aba
            abas_processadas <- abas_processadas + 1

            message(sprintf("Aba '%s' processada: %d registros", aba, nrow(dados_aba)))
          },
          error = function(e) {
            message(sprintf("Erro ao processar aba '%s' com readxl: %s", aba, e$message))

            # Tenta sem tipos forçados como fallback
            tryCatch(
              {
                if (suprimir_warnings) {
                  dados_aba_fallback <- suppressWarnings(readxl::read_excel(caminho_arquivo_cmf, sheet = aba))
                } else {
                  dados_aba_fallback <- readxl::read_excel(caminho_arquivo_cmf, sheet = aba)
                }

                if (nrow(dados_aba_fallback) > 0) {
                  # Padroniza nomes das colunas
                  names(dados_aba_fallback) <- names(dados_aba_fallback) %>%
                    stringr::str_trim() %>%
                    stringr::str_replace_all("\\s+", ".") %>%
                    stringr::str_replace_all("[^a-zA-Z0-9.]", "")

                  # Adiciona metadados
                  dados_aba_fallback <- dados_aba_fallback %>%
                    dplyr::mutate(
                      aba.origem = aba,
                      arquivo = caminho_arquivo_cmf,
                      arquivo.tabela.tipo = "cmf",
                      arquivo.tipo = "cmf",
                      arquivo.fonte = "ik"
                    )

                  dados_cmf_lista[[aba]] <- dados_aba_fallback
                  abas_processadas <- abas_processadas + 1

                  message(sprintf("Aba '%s' processada com fallback: %d registros", aba, nrow(dados_aba_fallback)))
                }
              },
              error = function(e2) {
                message(sprintf("Fallback também falhou para aba '%s': %s", aba, e2$message))
              }
            )
          }
        )
      }
    },
    error = function(e) {
      message(sprintf("Erro ao ler arquivo com readxl: %s", e$message))
      message("Tentando métodos alternativos...")
    }
  )

  # Se readxl falhou, tentar com openxlsx
  if (length(dados_cmf_lista) == 0) {
    tryCatch(
      {
        wb <- openxlsx::loadWorkbook(caminho_arquivo_cmf)
        abas_disponiveis <- names(wb)
        message(sprintf("Abas encontradas com openxlsx: %s", paste(abas_disponiveis, collapse = ", ")))

        for (aba in abas_disponiveis) {
          tryCatch(
            {
              dados_aba <- openxlsx::readWorkbook(wb, sheet = aba)

              if (nrow(dados_aba) == 0) {
                message(sprintf("Aba '%s' está vazia, pulando...", aba))
                next
              }

              # Converte para tibble e adiciona metadados
              dados_aba <- tibble::as_tibble(dados_aba) %>%
                dplyr::mutate(
                  aba.origem = aba,
                  arquivo = caminho_arquivo_cmf,
                  arquivo.tabela.tipo = "cmf",
                  arquivo.tipo = "cmf",
                  arquivo.fonte = "ik"
                )

              dados_cmf_lista[[aba]] <- dados_aba
              abas_processadas <- abas_processadas + 1

              message(sprintf("Aba '%s' processada com openxlsx: %d registros", aba, nrow(dados_aba)))
            },
            error = function(e) {
              message(sprintf("Erro ao processar aba '%s' com openxlsx: %s", aba, e$message))
            }
          )
        }
      },
      error = function(e) {
        message(sprintf("Erro ao ler arquivo com openxlsx: %s", e$message))
      }
    )
  }

  # Se nenhum método funcionou, retorna estrutura vazia
  if (length(dados_cmf_lista) == 0) {
    message("Não foi possível ler o arquivo CMF. Retornando estrutura vazia.")

    # Retorna tibble vazio com estrutura padrão baseada nos outros arquivos Informakon
    return(tibble::tibble(
      data.movimento = as.Date(character()),
      descricao = character(),
      valor = numeric(),
      contrato = character(),
      conta = character(),
      empresa = character(),
      mes = as.Date(character()),
      aba.origem = character(),
      arquivo = character(),
      arquivo.tabela.tipo = character(),
      arquivo.tipo = character(),
      arquivo.fonte = character()
    ))
  }

  # Consolida todos os dados
  message(sprintf("Total de abas processadas: %d", abas_processadas))

  # Combina todas as abas em um único dataframe
  cmf_consolidado <- tryCatch(
    {
      dplyr::bind_rows(dados_cmf_lista, .id = "aba.id")
    },
    error = function(e) {
      message(sprintf("Erro ao consolidar dados: %s", e$message))

      # Tenta consolidar aba por aba
      resultado_consolidado <- tibble::tibble()
      for (i in seq_along(dados_cmf_lista)) {
        tryCatch(
          {
            resultado_consolidado <- dplyr::bind_rows(resultado_consolidado, dados_cmf_lista[[i]])
          },
          error = function(e) {
            message(sprintf("Erro ao consolidar aba %d: %s", i, e$message))
          }
        )
      }
      return(resultado_consolidado)
    }
  )

  message(sprintf("Total de registros consolidados: %d", nrow(cmf_consolidado)))

  # Função auxiliar para conversão inteligente de tipos de dados
  converter_tipos_dados <- function(df) {
    # Lista de padrões de nomes de colunas e seus tipos esperados
    padroes_colunas <- list(
      # Colunas de data
      "data" = c(
        "data", "Data", "data.movimento", "Data.Movimento",
        "data.lancamento", "Data.Lancamento", "dt.lancto", "Dt.Lancto",
        "Data.Pagto", "data.pagto", "Data.Doc.Pagto", "data.doc.pagto",
        "Data.Vencimento", "data.vencimento", "Data.Liberacao", "data.liberacao",
        "Conciliacao", "Conciliação", "conciliacao", "conciliação",
        "Conciliao", "conciliao"
      ),

      # Colunas monetárias
      "valor" = c(
        "valor", "Valor", "valor.movimento", "Valor.Movimento",
        "Total.Pago", "total.pago", "Valor.Titulo", "valor.titulo",
        "acrescimos", "Acrescimos", "descontos", "Descontos",
        "encargos", "Encargos", "multa", "Multa"
      ),

      # Colunas numéricas inteiras
      "integer" = c(
        "numero.entrada", "Numero.Entrada", "parcela", "Parcela",
        "numero.conta", "Numero.Conta", "N.Conta", "n.conta"
      ),

      # Colunas de texto (forçar como character)
      "character" = c(
        "credor", "Credor", "documento", "Documento",
        "observacao", "Observacao", "empresa", "Empresa",
        "cod.centro", "Cod.Centro", "centro.negocio", "Centro.Negocio",
        "agente.financeiro", "Agente.Financeiro", "contrato", "Contrato",
        "Cliente", "cliente", "Link.Natureza", "Link Natureza",
        "link.natureza", "link natureza"
      )
    )

    for (nome_col in names(df)) {
      # Converter datas
      if (nome_col %in% padroes_colunas$data) {
        tryCatch(
          {
            # Primeiro verifica se são números de série do Excel
            if (is.character(df[[nome_col]]) &&
              all(stringr::str_detect(df[[nome_col]][!is.na(df[[nome_col]])], "^\\d+$"))) {
              # Converte números de série do Excel para data
              # Excel conta dias desde 1900-01-01, mas há um bug do Excel que considera 1900 bissexto
              df[[nome_col]] <- as.Date(as.numeric(df[[nome_col]]) - 2, origin = "1900-01-01")
            } else {
              # Tenta diferentes formatos de data
              df[[nome_col]] <- lubridate::as_date(df[[nome_col]])
            }
          },
          error = function(e) {
            # Se falhar, tenta formatos específicos
            tryCatch(
              {
                df[[nome_col]] <- as.Date(df[[nome_col]], format = "%d/%m/%Y")
              },
              error = function(e2) {
                # Última tentativa: se são números, tenta como serial do Excel
                tryCatch(
                  {
                    if (is.character(df[[nome_col]])) {
                      df[[nome_col]] <- as.Date(as.numeric(df[[nome_col]]) - 2, origin = "1900-01-01")
                    }
                  },
                  error = function(e3) {
                    message(sprintf("Aviso: Não foi possível converter '%s' para data", nome_col))
                  }
                )
              }
            )
          }
        )
      }

      # Converter valores monetários
      else if (nome_col %in% padroes_colunas$valor) {
        tryCatch(
          {
            # Remove pontos de milhares e substitui vírgula por ponto
            if (is.character(df[[nome_col]])) {
              df[[nome_col]] <- df[[nome_col]] %>%
                stringr::str_remove_all("\\.") %>% # Remove pontos de milhares
                stringr::str_replace(",", ".") %>% # Substitui vírgula por ponto
                as.numeric()
            } else {
              df[[nome_col]] <- as.numeric(df[[nome_col]])
            }
          },
          error = function(e) {
            message(sprintf("Aviso: Não foi possível converter '%s' para numérico", nome_col))
          }
        )
      }

      # Converter inteiros
      else if (nome_col %in% padroes_colunas$integer) {
        tryCatch(
          {
            df[[nome_col]] <- as.integer(df[[nome_col]])
          },
          error = function(e) {
            message(sprintf("Aviso: Não foi possível converter '%s' para inteiro", nome_col))
          }
        )
      }

      # Forçar como character
      else if (nome_col %in% padroes_colunas$character) {
        df[[nome_col]] <- as.character(df[[nome_col]])
      }
    }

    return(df)
  }

  # Aplica conversão de tipos nos dados consolidados
  cmf_consolidado <- converter_tipos_dados(cmf_consolidado)

  # Tenta identificar e padronizar colunas de data comuns (versão melhorada)
  colunas_data_comuns <- c(
    "data", "Data", "data.movimento", "Data.Movimento",
    "data.lancamento", "Data.Lancamento", "dt.lancto", "Dt.Lancto",
    "Data.Pagto", "data.pagto"
  )

  for (col_data in colunas_data_comuns) {
    if (col_data %in% names(cmf_consolidado)) {
      tryCatch(
        {
          cmf_consolidado[[col_data]] <- lubridate::as_date(cmf_consolidado[[col_data]])

          # Criar coluna mes se há dados de data
          if (sum(!is.na(cmf_consolidado[[col_data]])) > 0) {
            cmf_consolidado$mes <- lubridate::floor_date(cmf_consolidado[[col_data]], "month")
          }
        },
        error = function(e) {
          message(sprintf("Aviso: Não foi possível converter coluna '%s' para data: %s", col_data, e$message))
        }
      )
    }
  }

  # Tenta identificar e padronizar colunas de valor monetário
  colunas_valor_comuns <- c(
    "valor", "Valor", "valor.movimento", "Valor.Movimento",
    "Total.Pago", "total.pago", "Valor.Titulo", "valor.titulo"
  )

  for (col_valor in colunas_valor_comuns) {
    if (col_valor %in% names(cmf_consolidado)) {
      tryCatch(
        {
          cmf_consolidado[[col_valor]] <- as.numeric(cmf_consolidado[[col_valor]])
        },
        error = function(e) {
          message(sprintf("Aviso: Não foi possível converter coluna '%s' para numérico: %s", col_valor, e$message))
        }
      )
    }
  }

  # Tenta identificar colunas de empresa baseada nos padrões de outras funções
  colunas_empresa_comuns <- c("empresa", "Empresa", "cod.centro", "Cod.Centro")

  for (col_empresa in colunas_empresa_comuns) {
    if (col_empresa %in% names(cmf_consolidado)) {
      # Se for cod.centro, extrair empresa (primeiros 3 dígitos)
      if (stringr::str_detect(col_empresa, "cod\\.centro|Cod\\.Centro")) {
        tryCatch(
          {
            cmf_consolidado$empresa <- stringr::str_sub(as.character(cmf_consolidado[[col_empresa]]), 1, 3)
          },
          error = function(e) {
            message(sprintf("Aviso: Não foi possível extrair empresa de '%s': %s", col_empresa, e$message))
          }
        )
      }
    }
  }

  # Criar coluna conta.interno com os últimos 4 dígitos de Conta.N
  colunas_conta_comuns <- c("Conta.N", "conta.n", "Conta.Numero", "conta.numero", "conta", "Conta")

  for (col_conta in colunas_conta_comuns) {
    if (col_conta %in% names(cmf_consolidado)) {
      tryCatch(
        {
          # Obter valores da coluna de conta
          valor_conta <- cmf_consolidado[[col_conta]]

          # Criar conta.interno tratando NA explicitamente
          cmf_consolidado$conta.interno <- ifelse(
            is.na(valor_conta),
            NA_character_,
            {
              # Extrair apenas números da coluna
              conta_numeros <- stringr::str_extract_all(as.character(valor_conta), "\\d") %>%
                sapply(function(x) paste(x, collapse = ""))

              # Se não houver dígitos ou for string vazia, retornar NA
              ifelse(nchar(conta_numeros) == 0 | conta_numeros == "",
                NA_character_,
                stringr::str_pad(stringr::str_sub(conta_numeros, -4, -1),
                  4,
                  side = "left", pad = "0"
                )
              )
            }
          )

          message(sprintf("Coluna 'conta.interno' criada baseada em '%s'", col_conta))
          break # Sair do loop após encontrar a primeira coluna válida
        },
        error = function(e) {
          message(sprintf("Aviso: Não foi possível extrair conta.interno de '%s': %s", col_conta, e$message))
        }
      )
    }
  }

  # Se solicitado, salva em xlsx usando gerar_xlsx
  if (xlsx && nrow(cmf_consolidado) > 0) {
    tryCatch(
      {
        # Nome do arquivo de saída
        nome_arquivo <- sprintf("CMF_Consolidado_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S"))

        # Definir colunas monetárias que existem nos dados
        colunas_monetarias_existentes <- colunas_valor_comuns[colunas_valor_comuns %in% names(cmf_consolidado)]

        # Usar gerar_xlsx para criar o arquivo
        gerar_xlsx(
          data = cmf_consolidado,
          wb_load = NULL,
          tab_names = "CMF_Consolidado",
          col_width_def = 18,
          col_monetary = colunas_monetarias_existentes,
          save = list(nome_arquivo, f_caminho.pasta.ik_c)
        )

        message(sprintf("Arquivo Excel criado: %s", nome_arquivo))
      },
      error = function(e) {
        message(sprintf("Erro ao criar arquivo Excel: %s", e$message))
      }
    )
  }

  return(cmf_consolidado)
}

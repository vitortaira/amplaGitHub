#' @title Extração e Consolidação de Dados Informakon
#'
#' @description
#' A função **e_ik()** extrai os dados dos arquivos na pasta "Informakon",
#' preenche-os em uma planilha xlsx (opcional) e os retorna em uma lista.
#'
#' @param f_caminho.pasta.ik_c String do caminho da pasta "informakon".
#'   Valor padrão: \code{c_caminhos_pastas("informakon")}.
#' @param xlsx Logical. Se \code{TRUE}, cria um arquivo xlsx com os dados extraídos.
#'   Valor padrão: \code{FALSE}.
#'
#' @details
#' A extração é feita a partir de arquivos na pasta "informakon". Além disso, o
#' usuário pode optar por criar uma planilha xlsx com os dados processados.
#'
#' @return
#' Retorna uma lista contendo os dados extraídos (despesas, receitas, etc.) em
#' diferentes elementos.
#'
#' @examples
#' \dontrun{
#' # Chamada com valor padrão
#' dados_informakon <- e_ik()
#'
#' # Chamada customizada
#' dados_informakon <- e_ik(
#'   f_caminho.pasta.ik_c = "C:/caminho/personalizado/informakon",
#'   xlsx = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{read_excel}}, \code{\link{floor_date}}
#'
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect str_extract str_remove str_sub
#' @importFrom lubridate floor_date
#' @export

e_ik <-
  function(f_caminho.pasta.ik_c =
             c_caminhos_pastas("informakon"),
           xlsx = FALSE) {
    extrair_caminhos_relatorio_informakon <-
      function() {
        if (!dir.exists(f_caminho.pasta.ik_c)) {
          stop("A pasta 'informakon' não foi encontrada.")
        }
        # Despesas
        caminhos.arquivos.despesas_vc <-
          setdiff(
            dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file")[
              dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file") %>%
                basename() %>%
                str_which("^despesas")
            ],
            c(
              dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file")[
                str_which(
                  dir_ls(c_caminhos_pastas("informakon"), type = "file"),
                  "^Informakon.*"
                )
              ]
            )
          )
        data.final.despesas_vd <- c()
        for (caminho_arquivo_despesas.c in caminhos.arquivos.despesas_vc) {
          data.final.despesas_vd[caminho_arquivo_despesas.c] <-
            basename(caminho_arquivo_despesas.c) %>%
            str_extract("[^_]+$") %>%
            str_remove(".xlsx") %>%
            unname() %>%
            as.Date(format = "%Y%m%d")
        }
        data.final.despesas_vd %<>% unname %>% as.Date()
        data.final.despesas_d <- max(data.final.despesas_vd) %>% format("%Y%m%d")
        # Receitas
        caminhos.arquivos.receitas_vc <-
          setdiff(
            dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file")[
              dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file") %>%
                basename() %>%
                str_which("^receitas")
            ],
            c(
              dir_ls(c_caminhos_pastas("informakon"), recurse = TRUE, type = "file")[
                str_which(
                  dir_ls(c_caminhos_pastas("informakon"), type = "file"),
                  "^Informakon.*"
                )
              ]
            )
          )
        data.final.receitas_vd <- c()
        for (caminho_arquivo_receitas.c in caminhos.arquivos.receitas_vc) {
          data.final.receitas_vd[caminho_arquivo_receitas.c] <-
            basename(caminho_arquivo_receitas.c) %>%
            str_extract("[^_]+$") %>%
            str_remove(".xlsx") %>%
            unname() %>%
            as.Date(format = "%Y%m%d")
        }
        data.final.receitas_vd %<>% unname %>% as.Date()
        data.final.receitas_d <- max(data.final.receitas_vd) %>% format("%Y%m%d")
        # Mais recentes
        caminhos.relatorio.informakon_l <-
          list(
            "Despesas" =
              caminhos.arquivos.despesas_vc[
                str_which(
                  caminhos.arquivos.despesas_vc,
                  data.final.despesas_d
                )
              ],
            "Receitas" =
              caminhos.arquivos.receitas_vc[
                str_which(
                  caminhos.arquivos.receitas_vc,
                  data.final.receitas_d
                )
              ]
          )
        return(caminhos.relatorio.informakon_l)
      }
    caminhos.arquivos.informakon_vc <-
      extrair_caminhos_relatorio_informakon() %>%
      unlist()
    ###############################################################################
    # caminhos.arquivos.informakon_vc <-
    #  dir_ls(
    #    f_caminho.pasta.ik_c,
    #    recurse = TRUE,
    #    type = "file"
    #  )
    # caminhos.arquivos.informakon_vc <-
    #  caminhos.arquivos.informakon_vc[
    #    - str_which(
    #      basename(caminhos.arquivos.informakon_vc),
    #      "^Informakon"
    #      )
    #  ]
    dados.pasta.informakon_l <- list()
    for (caminho_arquivo_informakon.c in caminhos.arquivos.informakon_vc) {
      # Despesas ----------------------------------------------------------------

      if (caminho_arquivo_informakon.c %>% basename() %>% str_detect("^despesas")) {
        dados.pasta.informakon_l[["desp"]] <-
          read_excel(caminho_arquivo_informakon.c) %>%
          mutate(
            `a/c` = as.character(`a/c`),
            `Acréscimos` = as.numeric(`Acréscimos`),
            `Agente Financeiro` = as.character(`Agente Financeiro`),
            `Centro de Negócio` = as.character(`Centro de Negócio`),
            `Cod. Centro` = as.character(`Cod. Centro`),
            Credor = as.character(Credor),
            `Data Doc Pagto` = as.Date(`Data Doc Pagto`),
            `Data Liberação` = as.Date(`Data Liberação`),
            `Data Vencimento` = as.Date(`Data Vencimento`),
            `Data Vencimento Origem` = as.Date(`Data Vencimento Origem`),
            Descontos = as.numeric(Descontos),
            `Descontos Adiant.` = as.numeric(`Descontos Adiant.`),
            Documento = as.character(`Documento`),
            Empresa = `Cod. Centro` %>% str_sub(1, 3),
            Encargos = as.numeric(Encargos),
            `Mês` = floor_date(`Data Doc Pagto`, "month"),
            Multa = as.numeric(Multa),
            `N° Conta` = as.character(`N° Conta`),
            `Nº Entrada` = as.integer(`Nº Entrada`),
            `Observação` = as.character(`Observação`),
            Parcela = as.character(Parcela),
            `Total Pago` = as.numeric(`Total Pago`),
            `Valor Titulo` = as.numeric(`Valor Titulo`),
            Arquivo = caminho_arquivo_informakon.c,
            tabela.tipo = "desp",
            Arquivo_tipo = "desp",
            Arquivo_fonte = "ik"
          ) %>%
          select(
            `Data Doc Pagto`, `Mês`, `Data Liberação`, Credor, Empresa,
            `Cod. Centro`, `Centro de Negócio`, `Agente Financeiro`, `N° Conta`,
            `Nº Entrada`, Documento, Parcela, `Data Vencimento`,
            `Data Vencimento Origem`, `Valor Titulo`, `Acréscimos`, Descontos,
            Encargos, `Descontos Adiant.`, Multa, `Total Pago`, `a/c`,
            `Observação`, Arquivo, tabela.tipo, Arquivo_tipo,
            Arquivo_fonte
          )
      }

      # Receitas ----------------------------------------------------------------

      if (caminho_arquivo_informakon.c %>% basename() %>% str_detect("^receitas")) {
        dados.pasta.informakon_l[["rec"]] <-
          read_excel(caminho_arquivo_informakon.c, skip = 3) %>%
          mutate(
            Agente = as.character(Agente),
            Apto = as.integer(Apto),
            `Cart.` = as.factor(`Cart.`),
            Cliente = as.character(Cliente),
            Contrato = as.character(Contrato),
            `Data Pagto` = as.Date(`Data Pagto`, format = "%d/%m/%Y"),
            Desconto = as.numeric(Desconto),
            Elemento = as.character(Elemento),
            Empreendimento = as.character(Empreendimento),
            Empresa = Empreendimento %>% str_sub(1, 3),
            Encargos = as.numeric(Encargos),
            Esp = as.character(Esp),
            Juros = as.numeric(Juros),
            `Juros de Mora` = as.numeric(`Juros de Mora`),
            `Mês` = floor_date(`Data Pagto`, "month"),
            Multa = as.numeric(Multa),
            Parcela = as.character(Parcela),
            Principal = as.numeric(Principal),
            `R/F` = as.factor(`R/F`),
            Reajuste = as.numeric(Reajuste),
            Seguro = as.numeric(Seguro),
            Torre = as.character(Torre),
            Total = as.numeric(Total),
            Vencimento = as.Date(Vencimento),
            Arquivo = caminho_arquivo_informakon.c,
            tabela.tipo = "rec",
            Arquivo_tipo = "rec",
            Arquivo_fonte = "ik"
          ) %>%
          select(
            Empreendimento, Empresa, Cliente, Contrato, Torre, Apto, Esp,
            Parcela, Elemento, Vencimento, `Data Pagto`, `Mês`, `R/F`, Agente,
            Principal, Juros, Reajuste, Encargos, `Juros de Mora`, Multa,
            Seguro, Desconto, `Cart.`, Total, Arquivo, tabela.tipo,
            Arquivo_tipo, Arquivo_fonte
          )
      }
    }

    # Balanço patrimonial -----------------------------------------------------
    #
    #    # Despesas
    #    dados.despesas.bp.informakon_df <-
    #      dados.pasta.informakon_l[["Despesas"]] %>%
    #      group_by(`Centro de Negócio`, `Mês`) %>%
    #      summarise(
    #        `Total Pago no Mês` = sum(`Total Pago`, na.rm = T),
    #        .groups = "drop"
    #      ) %>%
    #      ungroup()
    #    # Receitas
    #    dados.receitas.bp.informakon_df <-
    #      dados.pasta.informakon_l[["Receitas"]] %>%
    #      group_by(Empreendimento, `Mês`) %>%
    #      summarise(
    #        Total = sum(Total, na.rm = T),
    #        .groups = "drop"
    #      ) %>%
    #      ungroup()
    #    # Adicionando a dados.pasta.informakon_l
    #    dados.pasta.informakon.bp_l <-
    #      c(
    #        dados.pasta.informakon_l,
    #        list(Despesas.BP = dados.despesas.bp.informakon_df),
    #        list(Receitas.BP = dados.receitas.bp.informakon_df)
    #      )

    return(dados.pasta.informakon_l)
  }

# Teste -------------------------------------------------------------------

# caminho_arquivo_informakon.c <-
#  here::here("informakon", "receitas_informakon_20180101_20250131.xlsx")
# dados.ik_l <- e_ik()
# str(e_ik())
# View(e_ik()$Despesas)
# View(e_ik()$Receitas)
# dados.pasta.informakon.despesas <- e_ik()$Despesas

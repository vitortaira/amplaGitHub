# Descrição ---------------------------------------------------------------

### RESUMO ###

# dados_ik() extrai os dados dos arquivos na pasta
# "Informakon", os preenche numa planilha xlsx, e os retorna numa lista.

### UTILIZAÇÃO ###

# dados_ik(
#   f_caminho.pasta.ik_c
# )

### ARGUMENTOS ###

# f_caminho.pasta.ik_c: String do caminho da pasta "informakon".

# Pacotes -----------------------------------------------------------------

library(here) # Facilita a identificação de caminhos
library(lubridate)
library(magrittr) # Ferramentas sintáticas ao dplyr, e.g. %<>%
library(openxlsx) # Funções para preencher arquivos .xlsx
library(purrr)
library(readxl) # Funções para a importação de arquivos em Excel, e.g. read_excel()
library(styler) # Funções para formatar códigos, e.g. style_file()
library(tidyverse) # Pacotes úteis para a análise de dados, e.g. dplyr e ggplot2

dados_ik <-
  function(f_caminho.pasta.ik_c =
             here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"),
           xlsx = FALSE) {
    extrair_caminhos_relatorio_informakon <-
      function(f_caminho.pasta.ik_c = here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon")) {
        if (!dir.exists(f_caminho.pasta.ik_c)) {
          stop("A pasta 'informakon' não foi encontrada.")
        }
        # Despesas
        caminhos.arquivos.despesas_vc <-
          setdiff(
            list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"), full.names = T)[
              list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"), full.names = T) %>%
                basename() %>%
                str_which("^despesas")
            ],
            c(
              list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"),
                full.names = T
              )[str_which(
                list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon")),
                "^Informakon.*"
              )]
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
            list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"), full.names = T)[
              list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"), full.names = T) %>%
                basename() %>%
                str_which("^receitas")
            ],
            c(
              list.files(
                here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon"),
                full.names = T
              )[str_which(
                list.files(here("Controladoria - Documentos", "Ampla_Github", "dados", "informakon")),
                "^Informakon.*"
              )]
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
    #  list.files(
    #    f_caminho.pasta.ik_c,
    #    full.names = T,
    #    recursive = T
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
        dados.pasta.informakon_l[["Despesas"]] <-
          read_excel(caminho_arquivo_informakon.c) %>%
          mutate(
            `a/c`                    = as.character(`a/c`),
            Acréscimos               = as.numeric(Acréscimos),
            `Agente Financeiro`      = as.character(`Agente Financeiro`),
            `Centro de Negócio`      = as.character(`Centro de Negócio`),
            `Cod. Centro`            = as.character(`Cod. Centro`),
            Credor                   = as.character(Credor),
            `Data Doc Pagto`         = as.Date(`Data Doc Pagto`),
            `Data Liberação`         = as.Date(`Data Liberação`),
            `Data Vencimento`        = as.Date(`Data Vencimento`),
            `Data Vencimento Origem` = as.Date(`Data Vencimento Origem`),
            Descontos                = as.numeric(Descontos),
            `Descontos Adiant.`      = as.numeric(`Descontos Adiant.`),
            Documento                = as.character(`Documento`),
            Empresa                  = `Cod. Centro` %>% str_sub(1, 3),
            Encargos                 = as.numeric(Encargos),
            Mês                      = floor_date(`Data Doc Pagto`, "month"),
            Multa                    = as.numeric(Multa),
            `N° Conta`               = as.character(`N° Conta`),
            `Nº Entrada`             = as.integer(`Nº Entrada`),
            Observação               = as.character(Observação),
            Parcela                  = as.character(Parcela),
            `Total Pago`             = as.numeric(`Total Pago`),
            `Valor Titulo`           = as.numeric(`Valor Titulo`)
          )
      }

      # Receitas ----------------------------------------------------------------

      if (caminho_arquivo_informakon.c %>% basename() %>% str_detect("^receitas")) {
        dados.pasta.informakon_l[["Receitas"]] <-
          read_excel(caminho_arquivo_informakon.c, skip = 3) %>%
          mutate(
            Agente          = as.character(Agente),
            Apto            = as.integer(Apto),
            `Cart.`         = as.factor(`Cart.`),
            Cliente         = as.character(Cliente),
            Contrato        = as.character(Contrato),
            `Data Pagto`    = as.Date(`Data Pagto`, format = "%d/%m/%Y"),
            Desconto        = as.numeric(Desconto),
            Elemento        = as.character(Elemento),
            Empreendimento  = as.character(Empreendimento),
            Encargos        = as.numeric(Encargos),
            Esp             = as.character(Esp),
            Juros           = as.numeric(Juros),
            `Juros de Mora` = as.numeric(`Juros de Mora`),
            Mês             = floor_date(`Data Pagto`, "month"),
            Multa           = as.numeric(Multa),
            Parcela         = as.character(Parcela),
            Principal       = as.numeric(Principal),
            `R/F`           = as.factor(`R/F`),
            Reajuste        = as.numeric(Reajuste),
            Seguro          = as.numeric(Seguro),
            Torre           = as.character(Torre),
            Total           = as.numeric(Total),
            Vencimento      = as.Date(Vencimento)
          )
      }
    }

    # Balanço patrimonial -----------------------------------------------------
    #
    #    # Despesas
    #    dados.despesas.bp.informakon_df <-
    #      dados.pasta.informakon_l[["Despesas"]] %>%
    #      group_by(`Centro de Negócio`, Mês) %>%
    #      summarise(
    #        `Total Pago no Mês` = sum(`Total Pago`, na.rm = T),
    #        .groups = "drop"
    #      ) %>%
    #      ungroup()
    #    # Receitas
    #    dados.receitas.bp.informakon_df <-
    #      dados.pasta.informakon_l[["Receitas"]] %>%
    #      group_by(Empreendimento, Mês) %>%
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
#  here("informakon", "receitas_informakon_20180101_20250131.xlsx")
# dados.ik_l <- dados_ik()
# str(dados_ik())
# View(dados_ik()$Despesas)
# View(dados_ik()$Receitas)
# dados.pasta.informakon.despesas <- dados_ik()$Despesas

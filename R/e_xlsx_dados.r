### RESUMO ###

# xlsx_dados() transforma os dados de dados() em XLSX.

### UTILIZAÇÃO ###

# xlsx_dados(file_path, sheet_name, ...)

### ARGUMENTOS ###
# file_path: Caminho para o arquivo Excel.
# sheet_name: Nome da aba a ser lida.

source(
  here::here(
    "Controladoria - Documentos", "AmplaR", "R", "e_dados.R"
  )
)

library(openxlsx)

xlsx_dados <-
  function(f_caminho.pasta.dados_c =
             here::here(
               "Relatórios - Documentos", "Dados", "Dados originais"
             )) {
    # Valida existência dos pacotes necessários
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("O pacote 'openxlsx' é necessário, mas não está instalado. Por favor, instale-o primeiro.")
    }
    # Valida argumentos
    if (!dir.exists(f_caminho.pasta.dados_c)) {
      stop(
        str_c(
          "A pasta '.../Relatórios - Documentos/Dados/Dados originais' ",
          "não foi encontrada."
        )
      )
    }
    # Carregando os dados
    dados_l <- e_dados()
    # Criando o arquivo xlsx
    wb_x <- createWorkbook()
    # Define nomes e cores das abas
    wb.abas_l <-
      list(
        Aba =
          c(
            # Abas da CEF
            c("cmfcn", "ecnC", "ecnE", "ecnPJ", "ecnU", "epr", "extCEF"),
            # Abas do Informakon
            c("desp", "recPS")
          ),
        Fonte =
          c(
            # Abas da CEF
            c(rep("cef", 7)),
            # Abas do Informakon
            c(rep("ik", 2))
          ),
        Cor =
          c(
            # Abas da CEF
            c(rep("darkblue", 7)),
            # Abas do Informakon
            c(rep("darkgreen", 2))
          )
      )
    # Adicionando e populando abas dinamicamente
    pwalk(
      wb.abas_l,
      function(Aba, Fonte, Cor) {
        addWorksheet(
          wb_x,
          sheet = Aba,
          gridLines = FALSE,
          tabColour = Cor
        )
        writeDataTable(
          wb_x,
          sheet = Aba,
          as.data.frame(dados_l[[Fonte]][[Aba]]),
          tableName = Aba
        )
      }
    )
    nome.xlsx_c <-
      str_c(
        here::here("Relatórios - Documentos", "Dados", "Dados originais"),
        "/Dados_",
        format(Sys.time(), format = "%Y_%m_%d-%H_%M_%S"),
        ".xlsx"
      )
    saveWorkbook(wb_x, nome.xlsx_c, overwrite = FALSE)
  }

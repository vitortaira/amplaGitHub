### RESUMO ###

# xlsx_dados() transforma os dados de dados() em XLSX.

### UTILIZAÇÃO ###

# xlsx_dados(file_path, sheet_name, ...)

### ARGUMENTOS ###
# file_path: Caminho para o arquivo Excel.
# sheet_name: Nome da aba a ser lida.

source(
  here(
    "Controladoria - Documentos", "Ampla_Github", "dados", "funcoes", "dados.R"
  )
)

library(openxlsx)

xlsx_dados <-
  function(f_caminho.pasta.dados_c =
             here(
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
    dados_l <- dados()
    # Criando o arquivo xlsx
    wb_x <- createWorkbook()
    # Define nomes e cores das abas
    wb.abas_t <- tibble(
      Aba =
        c("desp", "rec_ps"),
      Fonte =
        c(rep("ik", 2)),
      Cor =
        c(rep("darkgreen", 2))
    )
    # Adicionando e populando abas dinamicamente
    pwalk(
      wb.abas_t,
      ~ {
        addWorksheet(
          wb_x,
          sheet = ..1,
          gridLines = FALSE,
          tabColour = ..3
        )
        writeDataTable(
          wb_x,
          sheet = ..1,
          dados_l[[..2]][[..1]],
          tableName = ..1,
        )
      }
    )
    nome.xlsx_c <-
      str_c(
        here("Relatórios - Documentos", "Dados", "Dados originais"),
        "/Dados_",
        format(Sys.time(), format = "%Y_%m_%d-%H_%M_%S"),
        ".xlsx"
      )
    saveWorkbook(wb_x, nome.xlsx_c, overwrite = FALSE)
  }

#' Lado servidor da aplicação
#'
#' @param input,output,session Parâmetros internos para {shiny}.
#'     NÃO REMOVA.
#' @import shiny
#' @export
app_server <- function(input, output, session) {
  # Carregar bibliotecas necessárias
  library(plotly)
  library(dplyr)
  library(lubridate)
  library(fs) # Para manipulação de arquivos e diretórios
  library(here) # Para gerenciamento de caminhos relativos ao projeto

  # Carregar dados reais dos arquivos RDS (mesmo padrão do amplaShiny)
  # Carrega todos os arquivos RDS do diretório de dados
  dadosLista <- readRDS(
    dir_ls("inst/dados", type = "file")
  )

  # Inicializar módulos principais
  m_inicio_server("modulo_inicio")

  m_despesas_server(
    "modulo_despesas",
    dados_despesas = dadosLista$ik$desp
  )

  m_receitas_server(
    "modulo_receitas",
    dados_receitas = dadosLista$ik$rec
  )
}

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

  # Inicializar módulos para todas as URLs (eles só renderizarão quando a UI os chamar)
  g_barras.empilhadas.mes_server(
    "grafico_despesas",
    dados = dadosLista$ik$desp,
    choices = list("Empresa" = "empresa", "Centro" = "centro.negocio", "Credor" = "credor", "Agente Financeiro" = "agente.financeiro"),
    total = "total.pago",
    data = "data.doc.pagto",
    comecoTitulo = "Despesas por"
  )

  g_barras.empilhadas.mes_server(
    "grafico_receitas",
    dados = dadosLista$ik$rec,
    choices = list("Empresa" = "empresa", "Empreendimento" = "empreendimento", "Agente" = "agente", "Elemento" = "elemento"),
    total = "total",
    data = "data.pagamento",
    comecoTitulo = "Receitas por"
  )
}

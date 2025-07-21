#' Executar aplicação Shiny
#'
#' Esta função inicia a aplicação Shiny do pacote amplaApp.
#' Ela carrega automaticamente todos os dados necessários e
#' configura o ambiente para execução local ou deploy.
#'
#' @param host Endereço IP do host (padrão: '127.0.0.1')
#' @param port Porta para execução (padrão: 3838)
#' @param launch.browser Se deve abrir o navegador automaticamente (padrão: TRUE)
#' @param ... Argumentos adicionais passados para shiny::runApp()
#'
#' @return Não retorna valor (executa a aplicação Shiny)
#' @export
#'
#' @examples
#' \dontrun{
#' # Executar aplicação localmente
#' run_app()
#'
#' # Executar em porta específica
#' run_app(port = 4000)
#'
#' # Executar sem abrir navegador
#' run_app(launch.browser = FALSE)
#' }
run_app <- function(host = "127.0.0.1", port = 3838, launch.browser = TRUE, ...) {
  # Configurações iniciais do aplicativo
  options(scipen = 999) # Prevenir notação científica

  # Carregar pacotes necessários
  suppressMessages({
    library(shiny)
    library(dplyr)
    library(plotly)
    library(here)
    library(fs)
    library(lubridate)
  })

  # Executar a aplicação diretamente usando as funções já carregadas
  shiny::runApp(
    shinyApp(ui = app_ui, server = app_server),
    host = host,
    port = port,
    launch.browser = launch.browser,
    ...
  )
}

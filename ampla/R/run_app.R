#' Executar aplicação Shiny
#'
#' Esta função inicia a aplicação Shiny do pacote amplaApp.
#' Ela automaticamente encontra uma porta disponível se a porta padrão estiver ocupada.
#'
#' @param host Endereço IP do host (padrão: '127.0.0.1')
#' @param port Porta inicial para tentar (padrão: 3838)
#' @param launch.browser Se deve abrir o navegador automaticamente (padrão: TRUE)
#' @param ... Argumentos adicionais passados para shiny::runApp()
#'
#' @return Não retorna valor (executa a aplicação Shiny)
#' @export
#'
#' @examples
#' \dontrun{
#' # Executar aplicação localmente (tenta porta 3838, depois 3839, etc.)
#' run_app()
#'
#' # Executar começando em porta específica
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

  # Função para tentar várias portas automaticamente
  tentarExecutarApp <- function(porta_inicial) {
    for (p in porta_inicial:(porta_inicial + 50)) {
      resultado <- tryCatch(
        {
          message("Tentando iniciar na porta ", p)
          shiny::runApp(
            shinyApp(ui = app_ui, server = app_server),
            host = host,
            port = p,
            launch.browser = launch.browser,
            ...
          )
          "sucesso" # Se chegou aqui, funcionou
        },
        error = function(e) {
          if (grepl("address already in use|Failed to create server", e$message)) {
            # Porta ocupada, tentar próxima
            message("Porta ", p, " ocupada, tentando próxima...")
            "porta_ocupada"
          } else {
            # Outro erro, repassar
            stop(e)
          }
        }
      )

      # Se foi sucesso, sair da função
      if (resultado == "sucesso") {
        return()
      }
      # Se foi porta ocupada, continuar no loop (não precisa fazer nada)
    }
    stop("Não foi possível encontrar uma porta disponível")
  }

  # Tentar executar a aplicação
  tentarExecutarApp(port)
}

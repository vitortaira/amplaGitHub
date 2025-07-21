# Deployment app.R for shinyapps.io
# This file loads the package and runs the Shiny application

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
  library(DT)
  library(RColorBrewer)
  library(pkgload)
})

# Carregar o pacote local em modo desenvolvimento
# Para deployment, carrega todas as funções necessárias
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Verificar se as funções estão disponíveis
if (!exists("app_ui") || !exists("app_server")) {
  stop("Funções app_ui e app_server não encontradas. Verifique o carregamento do pacote.")
}

# Executar a aplicação diretamente
shinyApp(ui = app_ui, server = app_server)

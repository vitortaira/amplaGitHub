# Deployment app.R for shinyapps.io
# This file loads the package and runs the Shiny application

# Configurações iniciais do aplicativo
options(scipen = 999) # Prevenir notação científica

# Carregar pacotes necessários
library(shiny)
library(dplyr)
library(plotly)
library(here)
library(fs)
library(lubridate)
library(DT)
library(RColorBrewer)

# Carregar funções da aplicação
source("R/notion_mcp.R")
source("R/app_ui.R")
source("R/m_inicio.R")
source("R/sm_filtro_periodo.R")
source("R/sm_grafico_barras_empilhadas.R")
source("R/m_despesas.R")
source("R/m_receitas.R")
source("R/app_server.R")

# Verificar se as funções estão disponíveis
if (!exists("app_ui") || !exists("app_server")) {
  stop("Funções app_ui e app_server não encontradas. Verifique o carregamento do pacote.")
}

# Executar a aplicação diretamente
shinyApp(ui = app_ui, server = app_server)

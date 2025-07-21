# Script para executar a aplicação Shiny localmente
# Execute este arquivo no RStudio ou R Console

# Carregar bibliotecas necessárias
library(here)
library(pkgload)

# Carregar o pacote local em modo desenvolvimento
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Executar a aplicação usando a função do pacote
cat("Iniciando aplicação Shiny...\n")
cat("A aplicação será aberta em seu navegador padrão.\n")
cat("Para parar a aplicação, pressione Ctrl+C ou Esc no console do R.\n\n")

# Executar usando a função run_app()
run_app()

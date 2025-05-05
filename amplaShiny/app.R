# app.R (or server.R + ui.R if you prefer)
library(fs)
library(here)
library(lubridate)
library(plotly)
library(readxl)
library(shiny)
library(tidyverse)

# setwd("C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaShiny")

# Reference "amplaShiny" explicitly:
source(here("R", "g_desp.traj_i.r")) # provides g_desp.traj_i
source(here("R", "g_desp.traj_o.r")) # provides g_desp.traj_o
source(here("R", "g_rec.traj_i.r")) # provides g_rec.traj_i
source(here("R", "g_rec.traj_o.r")) # provides g_rec.traj_o

# Demo user table.  In production use a real auth method.
login_t <- data.frame(
  usuario = "ampler",
  senha = "251200",
  stringsAsFactors = FALSE
)

# Load data (make sure "dados_l[['ik']]$desp" and "$rec" exist!)
dados_l <- readRDS(
  dir_ls(here("inst", "dados"), type = "file")
)

# --- UI ---
ui <- fluidPage(
  # JS to handle Enter-to-login and show/hide password
  tags$script(HTML("
    $(document).on('keypress', function(e) {
      if(e.which==13) { $('#loginBtn').click(); }
    });
    Shiny.addCustomMessageHandler('togglePassword', function(show){
      $('#passwd').attr('type', show? 'text':'password');
    });
  ")),
  uiOutput("loginUI"),
  uiOutput("mainAppUI")
)

# --- SERVER ---
server <- function(input, output, session) {
  credentials <- reactiveValues(logged_in = FALSE, login_failed = FALSE)

  # Render the login UI if not logged in
  output$loginUI <- renderUI({
    if (!credentials$logged_in) {
      fluidPage(
        titlePanel("Dashboard Ampla"),
        br(),
        fluidRow(
          column(
            width = 4, offset = 4,
            wellPanel(
              textInput("userName", "Usuário:", ""),
              div(
                style = "position: relative;",
                passwordInput("passwd", "Senha:", ""),
                checkboxInput("showPassword", "Mostrar senha", value = FALSE)
              ),
              actionButton("loginBtn", "Entrar"),
              uiOutput("loginHelp")
            )
          )
        )
      )
    }
  })

  # Toggle password visibility
  observeEvent(input$showPassword, {
    session$sendCustomMessage("togglePassword", isTRUE(input$showPassword))
  })

  # Check login
  observeEvent(input$loginBtn, {
    req(input$userName, input$passwd)
    valid <- login_t$usuario == input$userName & login_t$senha == input$passwd
    if (any(valid, na.rm = TRUE)) {
      credentials$logged_in <- TRUE
      credentials$login_failed <- FALSE
      updateTabsetPanel(session, "pagePanels", selected = "Panorama")
    } else {
      credentials$login_failed <- TRUE
    }
  })

  # Show error if login failed
  output$loginHelp <- renderUI({
    if (credentials$login_failed) {
      div(
        style = "color: red; margin-top: 10px;",
        "O login falhou. Usuário e/ou senha incorretos."
      )
    }
  })

  # Main UI if logged in
  output$mainAppUI <- renderUI({
    if (credentials$logged_in) {
      fluidPage(
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = "ampla_header.jpg", alt = "ampla_header",
            width = "300px", height = "100px"
          )
        ),
        br(),
        tabsetPanel(
          id = "pagePanels",
          tabPanel("Panorama",
            value = "Panorama",
            fluidPage(
              tabsetPanel(
                id = "panoramaSubTabs",
                tabPanel("Financeiro",
                  value = "Financeiro",
                  fluidPage(
                    # Shiny modules for Despesas (inputs + outputs)
                    g_desp.traj_i("desp", names(dados_l[["ik"]]$desp)),
                    # Shiny modules for Receitas (inputs + outputs)
                    g_rec.traj_i("rec", names(dados_l[["ik"]]$rec))
                  )
                ),
                tabPanel("Comercial",
                  value = "Comercial",
                  h2("Comercial"),
                  "Placeholder for commercial content..."
                ),
                tabPanel("Obras",
                  value = "Obras",
                  h2("Obras"),
                  "Placeholder for obras content..."
                )
              )
            )
          ),
          tabPanel("Dados",
            value = "Dados",
            fluidPage(
              h2("Geral"),
              tags$a(
                href = paste0(
                  "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/Dados/Originais/",
                  str_remove(path_file(dir_ls(here("inst", "dados"), type = "file")), "\\..*$"),
                  ".xlsx"
                ),
                "Link para a planilha com os dados mais recentes",
                target = "_blank"
              )
            )
          ),
          tabPanel("Relatórios",
            value = "Relatórios",
            fluidPage(
              h2("Identificação de lançamentos dos extratos da CEF"),
              "Links para as planilhas."
            )
          )
        )
      )
    }
  })

  # Invoke the server part of each module
  # (Despesas)
  g_desp.traj_o("desp", dados_l[["ik"]])
  # (Receitas)
  g_rec.traj_o("rec", dados_l[["ik"]])
}
shinyApp(ui, server)

# app.R (or server.R + ui.R as you prefer)
library(fs)
library(here)
library(plotly)
library(readxl)
library(shiny)
library(tidyverse)
i_am(here::here()) # Set the working directory to the project root

# A simple user database (for demo only; use a secure approach in production)
login_t <- data.frame(
  usuario = "usuario",
  senha = "251200",
  stringsAsFactors = FALSE
)

# Load latest data
dados_l <-
  readRDS(
    dir_ls(here("amplaShiny", "inst", "dados"),
      type = "file"
    )
  )

# --- UI ---
ui <- fluidPage(
  # JS: fire login on Enter and toggle password visibility
  tags$script(HTML("
    // trigger login on Enter
    $(document).on('keypress', function(e) {
      if(e.which==13) { $('#loginBtn').click(); }
    });
    // handler to toggle the 'type' attribute of #passwd
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

  # --- Render login UI ---
  output$loginUI <- renderUI({
    if (!credentials$logged_in) {
      fluidPage(
        titlePanel("Dashboard Ampla"),
        br(),
        fluidRow(
          column(
            width = 4,
            offset = 4,
            wellPanel(
              textInput("userName", "Usuário:", ""),
              # Password input with show/hide functionality
              div(
                style = "position: relative;",
                passwordInput("passwd", "Senha:", ""),
                checkboxInput("showPassword", "Mostrar senha", value = FALSE)
              ),
              actionButton("loginBtn", "Entrar"),
              # Help message for failed login
              uiOutput("loginHelp")
            )
          )
        )
      )
    }
  })

  # --- Password show/hide: send a custom message to toggle the field ---
  observeEvent(input$showPassword, {
    session$sendCustomMessage("togglePassword", isTRUE(input$showPassword))
  })

  # --- Login button (or ENTER) ---
  observeEvent(input$loginBtn, {
    req(input$userName, input$passwd) # Ensure inputs are not NULL
    valid <- login_t$usuario == input$userName & login_t$senha == input$passwd
    if (length(valid) && any(valid, na.rm = TRUE)) { # valid login check
      credentials$logged_in <- TRUE
      credentials$login_failed <- FALSE
      # Automatically set "Panorama" after logging in:
      updateTabsetPanel(session, "pagePanels", selected = "Panorama")
    } else {
      credentials$login_failed <- TRUE
    }
  })

  # --- Display help message for failed login ---
  output$loginHelp <- renderUI({
    if (credentials$login_failed) {
      div(
        style = "color: red; margin-top: 10px;",
        "O login falhou. Usuário e/ou senha incorretos."
      )
    }
  })

  # --- Main app UI if logged in ---
  output$mainAppUI <- renderUI({
    if (credentials$logged_in) {
      fluidPage(
        # 1) Company logo from the "www" folder
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = "ampla_header.jpg",
            alt = "ampla_header",
            width = "300px",
            height = "100px"
          )
        ),
        br(),
        # 2) Tabset with three tabs
        tabsetPanel(
          id = "pagePanels",
          # Panorama tab now contains the variable selector and the Plotly chart
          tabPanel(
            title = "Panorama",
            value = "Panorama",
            fluidPage(
              h2("Despesas"),
              selectInput(
                inputId = "variavel_despesas",
                label = "Empilhar barras por:",
                choices = names(dados_l[["ik"]]$desp) %>%
                  discard(~ .x %in% c("Data Doc Pagto", "Data Liberação", "Mês")),
                selected = names(dados_l[["ik"]]$desp)[1]
              ),
              plotlyOutput("stackedPlot", height = "600px"),
              # h2("Receitas"),
              # selectInput(
              #  inputId = "variavel_receitas",
              #  label = "Empilhar barras por:",
              #  choices = names(dados_l[["ik"]]$desp) %>%
              #    discard(~ .x %in% c("Data Doc Pagto", "Data Liberação", "Mês")),
              #  selected = names(dados_l[["ik"]]$desp)[1]
              # ),
              # plotlyOutput("stackedPlot", height = "600px")
            )
          ),
          tabPanel(
            title = "Dados",
            value = "Dados",
            fluidPage(
              h2("Página: Dados"),
              "Exiba dados tabulares ou relatórios específicos..."
            )
          ),
          tabPanel(
            title = "Relatórios",
            value = "Relatórios",
            fluidPage(
              h2("Página: Relatórios"),
              "Links ou visualizações de relatórios em PDF/Excel..."
            )
          )
        )
      )
    }
  })

  # --- Example stacked plot ---
  output$stackedPlot <- renderPlotly({
    req(input$variavel_despesas)
    df_summary <- dados_l[["ik"]]$desp %>%
      group_by(`Mês`, Var = .data[[input$variavel_despesas]]) %>%
      summarise(Total = sum(.data[["Total Pago"]], na.rm = TRUE), .groups = "drop")
    plot_ly(
      data   = df_summary,
      x      = ~Mês,
      y      = ~Total,
      color  = ~Var,
      type   = "bar"
    ) %>%
      layout(
        title = "Trajetória das despesas",
        barmode = "stack",
        separators = ".,",
        xaxis = list(
          title      = "Data",
          tickformat = "%b-%y",
          tickangle  = 45
        ),
        yaxis = list(
          title     = "Valor (em R$)",
          autorange = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.05, y = -0.15
        ),
        width = 1200,
        height = 900,
        autosize = FALSE
      )
  })
}
shinyApp(ui, server)

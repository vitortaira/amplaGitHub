# app.R (or server.R + ui.R as you prefer)
library(fs)
library(here)
library(lubridate)
library(plotly)
library(readxl)
library(shiny)
library(tidyverse)
library(forcats)

# A simple user database (for demo only; use a secure approach in production)
login_t <- data.frame(
  usuario = "ampler",
  senha = "251200",
  stringsAsFactors = FALSE
)

# Load latest data
dados_l <-
  readRDS(
    dir_ls(here("inst", "dados"),
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
          # Panorama tab now contains sub-tabs
          tabPanel(
            title = "Panorama",
            value = "Panorama",
            fluidPage(
              tabsetPanel(
                id = "panoramaSubTabs",
                tabPanel(
                  title = "Financeiro",
                  value = "Financeiro",
                  fluidPage(
                    h2("Despesas"),
                    selectInput(
                      inputId = "variavel_despesas",
                      label = "Empilhar barras por:",
                      choices = names(dados_l[["ik"]]$desp) %>%
                        discard(
                          ~ .x %in%
                            c(
                              "a/c", "Acréscimos", "Arquivo",
                              "Arquivo_tipo_tabela", "Arquivo_tipo",
                              "Arquivo_fonte", "Cod. Centro",
                              "Data Doc Pagto", "Data Liberação",
                              "Data Vencimento", "Data Vencimento Origem",
                              "Descontos", "Descontos Adiant.", "Documento",
                              "Encargos", "Mês", "Multa",
                              "Nº Entrada", "Observação",
                              "Total Pago", "Valor Titulo"
                            )
                        ),
                      selected = "Empresa"
                    ),
                    # --- Periodo Despesas ---
                    radioButtons(
                      inputId = "filtro_periodo_desp",
                      label = "Seleção de período (Despesas):",
                      choices = c(
                        "Ano corrente"       = "ano_corrente",
                        "Últimos 12 meses"   = "ultimos_12",
                        "Desde o início"     = "desde_inicio",
                        "Selecionar período" = "personalizado"
                      ),
                      selected = "ano_corrente"
                    ),
                    conditionalPanel(
                      condition = "input.filtro_periodo_desp == 'personalizado'",
                      dateInput("data_inicial_desp", "Data inicial (Desp):"),
                      dateInput("data_final_desp", "Data final (Desp):")
                    ),
                    plotlyOutput("g_desp.traj", height = "600px"),
                    h2("Receitas"),
                    selectInput(
                      inputId = "variavel_receitas",
                      label = "Empilhar barras por:",
                      choices = names(dados_l[["ik"]]$rec),
                      selected = "Empreendimento"
                    ),
                    # --- Periodo Receitas ---
                    radioButtons(
                      inputId = "filtro_periodo_rec",
                      label = "Seleção de período (Receitas):",
                      choices = c(
                        "Ano corrente"       = "ano_corrente",
                        "Últimos 12 meses"   = "ultimos_12",
                        "Desde o início"     = "desde_inicio",
                        "Selecionar período" = "personalizado"
                      ),
                      selected = "ano_corrente"
                    ),
                    conditionalPanel(
                      condition = "input.filtro_periodo_rec == 'personalizado'",
                      dateInput("data_inicial_rec", "Data inicial (Rec):"),
                      dateInput("data_final_rec", "Data final (Rec):")
                    ),
                    plotlyOutput("g_rec.traj", height = "600px")
                  )
                ),
                tabPanel(
                  title = "Comercial",
                  value = "Comercial",
                  fluidPage(
                    h2("Comercial"),
                    "Placeholder for commercial content..."
                  )
                ),
                tabPanel(
                  title = "Obras",
                  value = "Obras",
                  fluidPage(
                    h2("Obras"),
                    "Placeholder for obras content..."
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "Dados",
            value = "Dados",
            fluidPage(
              h2("Geral"),
              tags$a(
                href = paste0(
                  "C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Relatórios - Documentos/",
                  "Dados/Originais/",
                  str_remove(
                    path_file(dir_ls(here("inst", "dados"), type = "file")),
                    "\\..*$"
                  ),
                  ".xlsx"
                ),
                "Link para a planilha com os dados mais recentes",
                target = "_blank"
              )
            )
          ),
          tabPanel(
            title = "Relatórios",
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

  # --- separate period range for Despesas (Data Doc Pagto) ---
  period_range_desp <- reactive({
    req(input$filtro_periodo_desp)
    today <- Sys.Date()
    switch(input$filtro_periodo_desp,
      ano_corrente = {
        start <- floor_date(today, "year")
        end <- today
      },
      ultimos_12 = {
        start <- today %m-% months(12)
        end <- today
      },
      desde_inicio = {
        dt <- as.Date(dados_l[["ik"]]$desp$`Data Doc Pagto`)
        start <- min(dt, na.rm = TRUE)
        end <- today
      },
      personalizado = {
        req(input$data_inicial_desp, input$data_final_desp)
        start <- input$data_inicial_desp
        end <- input$data_final_desp
      }
    )
    list(start = start, end = end)
  })

  # --- separate period range for Receitas (Data Pagto) ---
  period_range_rec <- reactive({
    req(input$filtro_periodo_rec)
    today <- Sys.Date()
    switch(input$filtro_periodo_rec,
      ano_corrente = {
        start <- floor_date(today, "year")
        end <- today
      },
      ultimos_12 = {
        start <- today %m-% months(12)
        end <- today
      },
      desde_inicio = {
        dt <- as.Date(dados_l[["ik"]]$rec$`Data Pagto`)
        start <- min(dt, na.rm = TRUE)
        end <- today
      },
      personalizado = {
        req(input$data_inicial_rec, input$data_final_rec)
        start <- input$data_inicial_rec
        end <- input$data_final_rec
      }
    )
    list(start = start, end = end)
  })

  # --- update renderPlotly for Despesas ---
  output$g_desp.traj <- renderPlotly({
    req(input$variavel_despesas)
    pr <- period_range_desp()
    df <- dados_l[["ik"]]$desp %>%
      mutate(.dt = as.Date(`Data Doc Pagto`)) %>%
      filter(.dt >= pr$start, .dt <= pr$end) %>%
      group_by(
        Mês = floor_date(.dt, "month"),
        Var = .data[[input$variavel_despesas]]
      ) %>%
      summarise(Total = sum(`Total Pago`, na.rm = TRUE), .groups = "drop") %>%
      mutate(Var = fct_reorder(Var, Total, .fun = sum))

    plot_ly(df, x = ~Mês, y = ~Total, color = ~Var, type = "bar") %>%
      layout(
        barmode   = "stack",
        title     = "Trajetória das despesas",
        xaxis     = list(tickformat = "%b‑%y", tickangle = 45),
        yaxis     = list(title = "Valor (em R$)"),
        legend    = list(orientation = "h", x = 0.05, y = -0.15),
        autosize  = TRUE
      ) %>%
      config(displaylogo = FALSE)
  })

  # --- update renderPlotly for Receitas ---
  output$g_rec.traj <- renderPlotly({
    req(input$variavel_receitas)
    pr <- period_range_rec()
    df <- dados_l[["ik"]]$rec %>%
      mutate(.dt = as.Date(`Data Pagto`)) %>%
      filter(.dt >= pr$start, .dt <= pr$end) %>%
      group_by(
        Mês = floor_date(.dt, "month"),
        Var = .data[[input$variavel_receitas]]
      ) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Var = fct_reorder(Var, Total, .fun = sum))

    plot_ly(df, x = ~Mês, y = ~Total, color = ~Var, type = "bar") %>%
      layout(barmode = "stack") %>%
      config(displayModeBar = FALSE)
  })
}
shinyApp(ui, server)

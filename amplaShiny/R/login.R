# Login UI Module
login_ui <- function(id) {
  ns <- NS(id)

  # JS scripts for password handling and enter key
  tags$head(
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if(e.which==13) { $('#loginBtn').click(); }
      });
      Shiny.addCustomMessageHandler('togglePassword', function(show){
        $('#passwd').attr('type', show? 'text':'password');
      });
    "))
  )

  uiOutput(ns("loginUI"))
}

# Login Server Module
login_server <- function(id, login_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    credentials <- reactiveValues(logged_in = FALSE, login_failed = FALSE)

    # UI de login
    output$loginUI <- renderUI({
      if (!credentials$logged_in) {
        fluidPage(
          tags$div(
            style = "text-align: center;",
            tags$img(
              src = "ampla_header.jpg", alt = "Cabeçalho Ampla",
              width = "450px", height = "150px"
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4, offset = 4,
              wellPanel(
                textInput(ns("userName"), "Usuário:", ""),
                div(
                  style = "position: relative;",
                  passwordInput(ns("passwd"), "Senha:", ""),
                  checkboxInput(ns("showPassword"), "Mostrar senha", value = FALSE)
                ),
                actionButton(ns("loginBtn"), "Entrar"),
                uiOutput(ns("loginHelp"))
              )
            )
          )
        )
      }
    })

    observeEvent(input$showPassword, {
      session$sendCustomMessage("togglePassword", isTRUE(input$showPassword))
    })

    observeEvent(input$loginBtn, {
      req(input$userName, input$passwd)
      valid <- login_data$usuario == input$userName & login_data$senha == input$passwd
      if (any(valid, na.rm = TRUE)) {
        credentials$logged_in <- TRUE
        credentials$login_failed <- FALSE
      } else {
        credentials$login_failed <- TRUE
      }
    })

    output$loginHelp <- renderUI({
      if (credentials$login_failed) {
        div(
          style = "color: red; margin-top: 10px;",
          "O login falhou. Usuário e/ou senha incorretos."
        )
      }
    })

    # Return reactive values to main app
    return(credentials)
  })
}

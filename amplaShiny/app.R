# app.R (ou server.R + ui.R, se preferir)
# Carregando as bibliotecas necessárias
library(fs) # Manipulação de arquivos e diretórios
library(here) # Gerenciamento de caminhos relativos
library(lubridate) # Manipulação de datas
library(plotly) # Criação de gráficos interativos
library(readxl) # Leitura de arquivos Excel
library(shiny) # Aplicações web interativas
library(tidyverse) # Conjunto de pacotes para manipulação de dados

# Defina o diretório de trabalho, se necessário (comentado)
# setwd("C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Documents/amplaGitHub/amplaShiny")

# Carrega os módulos e o script de filtro de período
source(here("R", "filtro_periodo.R"))
source(here("R", "g_desp.traj_i.R"))
source(here("R", "g_desp.traj_o.R"))
source(here("R", "g_rec.traj_i.R"))
source(here("R", "g_rec.traj_o.R"))
source(here("R", "g_metadados.hist_i.R"))
source(here("R", "g_metadados.hist_o.R"))

# Tabela de login para demonstração
login_t <- data.frame(
  usuario = "ampler",
  senha = "251200",
  stringsAsFactors = FALSE
)

# Carrega os dados
dados_l <- readRDS(
  dir_ls(here("inst", "dados"), type = "file")
)

# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  title = "Dashboard Ampla",
  tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg")
  ),

  # Script JavaScript para tratar Enter no teclado e para alternar a visibilidade da senha
  tags$script(HTML("
    $(document).on('keypress', function(e) {
      if(e.which==13) { $('#loginBtn').click(); }
    });
    Shiny.addCustomMessageHandler('togglePassword', function(show){
      $('#passwd').attr('type', show? 'text':'password');
    });
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
      navigator.clipboard.writeText(message).then(function() {
        console.log('Path copied to clipboard');
      }, function() {
        console.error('Failed to copy path');
      });
    });
  ")),
  uiOutput("loginUI"),
  uiOutput("mainAppUI")
)

# --- Lógica do Servidor ---
server <- function(input, output, session) {
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

  observeEvent(input$showPassword, {
    session$sendCustomMessage("togglePassword", isTRUE(input$showPassword))
  })

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

  output$loginHelp <- renderUI({
    if (credentials$login_failed) {
      div(
        style = "color: red; margin-top: 10px;",
        "O login falhou. Usuário e/ou senha incorretos."
      )
    }
  })

  # UI principal
  output$mainAppUI <- renderUI({
    if (credentials$logged_in) {
      fluidPage(
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = "ampla_header.jpg", alt = "Cabeçalho Ampla",
            width = "450px", height = "150px"
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
                    # Substitui o filtro_periodo local pelos módulos do filtro
                    filtro_periodo_module_ui("myFiltro"),
                    g_desp.traj_i(
                      "desp",
                      c(
                        "Agente Financeiro", "Credor", "Centro de Negócio",
                        "Empresa", "N° Conta", "Parcela"
                      )
                    ),
                    g_rec.traj_i("rec", names(dados_l[["ik"]]$rec))
                  )
                ),
                tabPanel("Comercial",
                  value = "Comercial",
                  h2("Comercial"),
                  "Conteúdo placeholder para a área comercial..."
                ),
                tabPanel("Obras",
                  value = "Obras",
                  h2("Obras"),
                  "Conteúdo placeholder para a área de obras..."
                )
              )
            )
          ),
          tabPanel(
            "Dados",
            value = "Dados",
            fluidPage(
              h2("Geral"),
              actionButton("copyPath", "Copiar caminho do arquivo para área de transferência"),
              textOutput("copyConfirmation"),
              hr(),
              # chamada à UI do seu módulo de histograma
              g_metadados.hist_i("metaHist", choices = names(dados_l[["metadados"]]$metadados)),
            )
          ),
          tabPanel("Relatórios",
            value = "Relatórios",
            fluidPage(
              h2("Conciliação de extratos bancários"),
              "Links para as planilhas.",
              h2("Demonstrativo de fluxo de caixa"),
              "Links para as planilhas.",
              h2("Orçado x Realizado"),
              "Links para as planilhas."
            )
          )
        )
      )
    }
  })

  # Chama a parte do servidor dos filtros e dos módulos
  filtroVals <- filtro_periodo_module_server("myFiltro")

  g_desp.traj_o(
    "desp",
    dados_l[["ik"]],
    filtroVals$filtro_periodo,
    filtroVals$data_inicial,
    filtroVals$data_final
  )
  g_rec.traj_o(
    "rec",
    dados_l[["ik"]],
    filtroVals$filtro_periodo,
    filtroVals$data_inicial,
    filtroVals$data_final
  )

  # módulo de saída do histograma de metadados
  g_metadados.hist_o(
    "metaHist",
    dados_l[["metadados"]],
    filtroVals$filtro_periodo,
    filtroVals$data_inicial,
    filtroVals$data_final
  )

  # Clipboard operation
  observeEvent(input$copyPath, {
    file_path <- file.path(
      "C:/Users/Ampla/AMPLA INCORPORADORA LTDA",
      "Relatórios - Documentos",
      "Dados",
      "Originais",
      paste0(
        str_remove(
          path_file(dir_ls(here("inst", "dados"), type = "file")[1]),
          "\\..*$"
        ),
        ".xlsx"
      )
    )

    session$sendCustomMessage("copyToClipboard", file_path)
    output$copyConfirmation <- renderText("Caminho copiado para área de transferência!")
  })
}

# Inicializa a aplicação Shiny
shinyApp(ui, server)

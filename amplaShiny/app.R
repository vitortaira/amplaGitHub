# app.R (ou server.R + ui.R, se preferir)
# Carregando as bibliotecas necessárias
library(fs) # Manipulação de arquivos e diretórios
library(here) # Gerenciamento de caminhos relativos
library(lubridate) # Manipulação de datas
library(plotly) # Criação de gráficos interativos
library(readxl) # Leitura de arquivos Excel
library(shiny) # Aplicações web interativas
library(tidyverse) # Conjunto de pacotes para manipulação de dados

# Define o diretório de trabalho, se necessário (comentado)
# setwd("C:/Users/Ampla/AMPLA INCORPORADORA LTDA/Controladoria - Documentos/amplaGitHub/amplaShiny")

# Carrega as funções específicas dos módulos (referenciando explicitamente a pasta "amplaShiny")
source(here("R", "g_desp.traj_i.r")) # fornece a função g_desp.traj_i
source(here("R", "g_desp.traj_o.r")) # fornece a função g_desp.traj_o
source(here("R", "g_rec.traj_i.r")) # fornece a função g_rec.traj_i
source(here("R", "g_rec.traj_o.r")) # fornece a função g_rec.traj_o

# Tabela de login para demonstração. Em produção, utilize um método de autenticação adequado.
login_t <- data.frame(
  usuario = "ampler",
  senha = "251200",
  stringsAsFactors = FALSE
)

# Carrega os dados. Certifique-se de que "dados_l[['ik']]$desp" e "$rec" existam!
dados_l <- readRDS(
  dir_ls(here("inst", "dados"), type = "file")
)

# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  # Script JavaScript para tratar Enter no teclado e para alternar a visibilidade da senha
  tags$script(HTML("
    $(document).on('keypress', function(e) {
      if(e.which==13) { $('#loginBtn').click(); }
    });
    Shiny.addCustomMessageHandler('togglePassword', function(show){
      $('#passwd').attr('type', show? 'text':'password');
    });
  ")),
  uiOutput("loginUI"), # Área de login
  uiOutput("mainAppUI") # Interface principal exibida após o login
)

# --- Lógica do Servidor ---
server <- function(input, output, session) {
  # Variáveis reativas para armazenar o estado de autenticação
  credentials <- reactiveValues(logged_in = FALSE, login_failed = FALSE)

  # Renderiza a interface de login caso o usuário ainda não tenha feito login
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
              uiOutput("loginHelp") # Mensagem de erro caso o login falhe
            )
          )
        )
      )
    }
  })

  # Observa a alteração do checkbox para mostrar/ocultar a senha
  observeEvent(input$showPassword, {
    session$sendCustomMessage("togglePassword", isTRUE(input$showPassword))
  })

  # Verifica as credenciais quando o botão de login é clicado
  observeEvent(input$loginBtn, {
    req(input$userName, input$passwd)
    valid <- login_t$usuario == input$userName & login_t$senha == input$passwd
    if (any(valid, na.rm = TRUE)) {
      credentials$logged_in <- TRUE # Login realizado com sucesso
      credentials$login_failed <- FALSE
      updateTabsetPanel(session, "pagePanels", selected = "Panorama")
    } else {
      credentials$login_failed <- TRUE # Login falhou
    }
  })

  # Renderiza a mensagem de erro de login, se necessário
  output$loginHelp <- renderUI({
    if (credentials$login_failed) {
      div(
        style = "color: red; margin-top: 10px;",
        "O login falhou. Usuário e/ou senha incorretos."
      )
    }
  })

  # Renderiza a interface principal da aplicação, após o sucesso do login
  output$mainAppUI <- renderUI({
    if (credentials$logged_in) {
      fluidPage(
        # Cabeçalho com a imagem (logo) centralizada
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = "ampla_header.jpg", alt = "Cabeçalho Ampla",
            width = "450px", height = "150px"
          )
        ),
        br(),
        # Aba principal contendo vários submenus
        tabsetPanel(
          id = "pagePanels",
          tabPanel("Panorama",
            value = "Panorama",
            fluidPage(
              # Sub-abas dentro da aba "Panorama"
              tabsetPanel(
                id = "panoramaSubTabs",
                tabPanel("Financeiro",
                  value = "Financeiro",
                  fluidPage(
                    # Módulo para Despesas: inputs e outputs
                    g_desp.traj_i(
                      "desp",
                      c(
                        "Agente Financeiro", "Credor", "Centro de Negócio",
                        "Empresa", "N° Conta", "Parcela"
                      )
                    ),
                    # Módulo para Receitas: inputs e outputs
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
          tabPanel("Dados",
            value = "Dados",
            fluidPage(
              h2("Geral"),
              # Botão para baixar a planilha com os dados mais recentes.
              downloadButton(
                outputId = "downloadData",
                label = "Baixar planilha com os dados mais recentes"
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

  # Chama a parte do servidor de cada módulo após a autenticação
  # Módulo de Despesas
  g_desp.traj_o("desp", dados_l[["ik"]])
  # Módulo de Receitas
  g_rec.traj_o("rec", dados_l[["ik"]])

  # Download handler para o botão de download
  output$downloadData <- downloadHandler(
    filename = function() {
      # Get the filename without extension
      base_name <- str_remove(
        path_file(
          dir_ls(
            here("inst", "dados"),
            type = "file"
          )
        ),
        "\\..*$"
      )
      paste0(base_name, ".xlsx")
    },
    content = function(file) {
      # Find the excel file path
      excel_path <- file.path(
        "C:/Users/Ampla/AMPLA INCORPORADORA LTDA",
        "Relatórios - Documentos",
        "Dados",
        "Originais",
        paste0(
          str_remove(
            path_file(
              dir_ls(
                here("inst", "dados"),
                type = "file"
              )
            ),
            "\\..*$"
          ),
          ".xlsx"
        )
      )

      # Only copy if it exists
      if (file.exists(excel_path)) {
        file.copy(excel_path, file)
      } else {
        # Create a simple Excel file if original doesn't exist
        write.csv(data.frame(message = "Arquivo original não disponível"), file)
      }
    }
  )
}

# Inicializa a aplicação Shiny
shinyApp(ui, server)

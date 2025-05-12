# =============================================================================
# DASHBOARD AMPLA - APLICAÇÃO SHINY
# =============================================================================
# Aplicação para visualização e análise de dados financeiros e comerciais
# da Ampla Incorporadora
# Autor: Equipe Ampla
# Atualizado: 2023

# =============================================================================
# BIBLIOTECAS
# =============================================================================
library(fs) # Para manipulação de arquivos e diretórios
library(here) # Para gerenciamento de caminhos relativos ao projeto
library(lubridate) # Para manipulação e formatação de datas
library(plotly) # Para criação de gráficos interativos
library(readxl) # Para leitura de arquivos Excel (.xlsx, .xls)
library(shiny) # Framework para desenvolvimento de aplicações web interativas
library(tidyverse) # Meta-pacote com dplyr, ggplot2, tidyr, etc. para análise de dados

# =============================================================================
# CARREGAMENTO DE MÓDULOS
# =============================================================================
# Carrega componentes modulares da aplicação
source(here("R", "login.R")) # Sistema de autenticação
source(here("R", "filtro_periodo.R")) # Filtros temporais para os dados
source(here("R", "g_desp.traj.R")) # Visualização de trajetória de despesas
source(here("R", "g_rec.traj.R")) # Visualização de trajetória de receitas
source(here("R", "g_metadados.hist.R")) # Histograma de metadados

# =============================================================================
# DADOS DE AUTENTICAÇÃO
# =============================================================================
# Credenciais de acesso para demonstração (em produção, usar sistema seguro)
login_t <- data.frame(
  usuario = "ampler",
  senha = "251200",
  stringsAsFactors = FALSE
)

# =============================================================================
# CARREGAMENTO DOS DADOS
# =============================================================================
# Carrega todos os arquivos RDS do diretório de dados
dados_l <- readRDS(
  dir_ls(here("inst", "dados"), type = "file")
)

# =============================================================================
# INTERFACE DO USUÁRIO (UI)
# =============================================================================
ui <- fluidPage(
  title = "Dashboard Ampla",
  # Configura o ícone da aplicação na aba do navegador
  tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg")
  ),

  # Script JavaScript para funcionalidade de copiar para área de transferência
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
      navigator.clipboard.writeText(message).then(function() {
        console.log('Path copied to clipboard');
      }, function() {
        console.error('Failed to copy path');
      });
    });
  ")),

  # Componente de login (renderizado antes do acesso ao dashboard)
  login_ui("login"),

  # Container para o conteúdo principal (renderizado após autenticação)
  uiOutput("mainAppUI")
)

# =============================================================================
# LÓGICA DO SERVIDOR
# =============================================================================
server <- function(input, output, session) {
  # Inicializa o módulo de login e obtém o status de autenticação
  credentials <- login_server("login", login_t)

  # Redireciona para a aba "Panorama" após o login bem-sucedido
  observeEvent(credentials$logged_in, {
    if (credentials$logged_in) {
      updateTabsetPanel(session, "pagePanels", selected = "Panorama")
    }
  })

  # Renderiza a interface principal apenas após autenticação
  output$mainAppUI <- renderUI({
    if (credentials$logged_in) {
      fluidPage(
        # Cabeçalho com logo da empresa
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = "ampla_header.jpg", alt = "Cabeçalho Ampla",
            width = "450px", height = "150px"
          )
        ),
        br(),
        # Painéis principais de navegação
        tabsetPanel(
          id = "pagePanels",

          # =============================================================================
          # PAINEL: PANORAMA
          # =============================================================================
          tabPanel("Panorama",
            value = "Panorama",
            fluidPage(
              tabsetPanel(
                id = "panoramaSubTabs",
                # Subpainel: Financeiro - Exibe gráficos de despesas e receitas
                tabPanel("Financeiro",
                  value = "Financeiro",
                  fluidPage(
                    # Filtro temporal para os gráficos financeiros
                    filtro_periodo_module_ui("myFiltro"),
                    # Gráfico de trajetória de despesas com opções de segmentação
                    g_desp.traj_ui(
                      "desp",
                      c(
                        "Agente Financeiro", "Credor", "Centro de Negócio",
                        "Empresa", "N° Conta", "Parcela"
                      )
                    ),
                    # Gráfico de trajetória de receitas com todas as dimensões disponíveis
                    g_rec.traj_ui("rec", names(dados_l[["ik"]]$rec))
                  )
                ),
                # Subpaineis para futuras implementações
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

          # =============================================================================
          # PAINEL: DADOS
          # =============================================================================
          tabPanel(
            "Dados",
            value = "Dados",
            fluidPage(
              h2("Geral"),
              # Botão para copiar caminho do arquivo original para importação/validação
              actionButton("copyPath", "Copiar caminho do arquivo para área de transferência"),
              textOutput("copyConfirmation"),
              hr(),
              # Visualização de histograma para análise de metadados
              g_metadados.hist_i("metaHist", choices = names(dados_l[["metadados"]]$metadados)),
            )
          ),

          # =============================================================================
          # PAINEL: RELATÓRIOS
          # =============================================================================
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

  # =============================================================================
  # INICIALIZAÇÃO DOS MÓDULOS
  # =============================================================================

  # Inicializa o módulo de filtro de período e obtém valores selecionados
  filtroVals <- filtro_periodo_module_server("myFiltro")

  # Inicializa o módulo de visualização de despesas
  g_desp.traj_server(
    "desp",
    dados_l[["ik"]],
    filtroVals$filtro_periodo,
    filtroVals$data_inicial,
    filtroVals$data_final
  )

  # Inicializa o módulo de visualização de receitas
  g_rec.traj_server(
    "rec",
    dados_l[["ik"]],
    filtroVals$filtro_periodo,
    filtroVals$data_inicial,
    filtroVals$data_final
  )

  # Inicializa o módulo de histograma de metadados
  g_metadados.hist_o(
    "metaHist",
    dados_l[["metadados"]],
    function() {
      filtroVals$filtro_periodo()
    }, # Wrap in function to ensure proper reactive handling
    function() {
      filtroVals$data_inicial()
    },
    function() {
      filtroVals$data_final()
    }
  )

  # =============================================================================
  # FUNCIONALIDADE: COPIAR CAMINHO DO ARQUIVO
  # =============================================================================
  observeEvent(input$copyPath, {
    # Constrói o caminho para o arquivo original Excel
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

    # Utiliza o handler JS personalizado para copiar o texto
    session$sendCustomMessage("copyToClipboard", file_path)
    output$copyConfirmation <- renderText("Caminho copiado para área de transferência!")
  })
}

# Inicializa a aplicação Shiny com as configurações definidas
shinyApp(ui, server)

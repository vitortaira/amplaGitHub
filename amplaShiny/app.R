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
library(DT)
library(fs) # Para manipulação de arquivos e diretórios
library(here) # Para gerenciamento de caminhos relativos ao projeto
library(htmlwidgets) # Para exportação de gráficos interativos
library(lubridate) # Para manipulação e formatação de datas
library(plotly) # Para criação de gráficos interativos
library(RColorBrewer) # Para paletas de cores
library(readxl) # Para leitura de arquivos Excel (.xlsx, .xls)
library(shiny) # Framework para desenvolvimento de aplicações web interativas
library(tidyverse) # Meta-pacote com dplyr, ggplot2, tidyr, etc
library(visNetwork) # Para visualização de redes e grafos

# =============================================================================
# CARREGAMENTO DE MÓDULOS
# =============================================================================
# Carrega componentes modulares da aplicação
source(here("R", "b_dados.R")) # Módulo de busca de dados
source(here("R", "filtro_periodo.R")) # Filtros temporais para os dados
source(here("R", "g_barras.empilhadas.mes.R")) # Visualização de trajetória de despesas
source(here("R", "g_cronogramas.cef.R")) # Cronogramas nos contratos com a CEF
source(here("R", "gs_barras.cef.cobra.R"))
source(here("R", "g_gnw.R")) # Rede de grafos
source(here("R", "g_metadados.hist.R")) # Histograma de metadados
source(here("R", "login.R")) # Sistema de autenticação

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

dados_l[["metadados"]]$data_geracao <-
  dir_ls(here("inst", "dados"), type = "file") %>%
  path_file() %>%
  str_remove("\\..*$") %>%
  str_remove("Dados_") %>%
  as_datetime(format = "%Y_%m_%d-%H_%M_%S")

# =============================================================================
# INTERFACE DO USUÁRIO (UI)
# =============================================================================
ui <- fluidPage(
  title = "Dashboard Ampla",
  # Configura o ícone da aplicação na aba do navegador
  tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = "ampla_icon.jpeg"),
    # Load the Select2 CSS/JS
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/css/select2.min.css"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/js/select2.min.js"
    ),
    # Add a small CSS snippet that forces Select2 to appear above other elements
    tags$style(HTML("
      .select2-container, .select2-dropdown, .select2-search {
        z-index: 999999999 !important;
      }
    ")),
    tags$style(HTML("
      /* Make top-level tabs sticky */
      .nav-tabs {
        position: sticky;
        top: 0;
        z-index: 1040;
        background-color: #fff;
        margin-bottom: 0;
        border-bottom: 1px solid #ddd;
      }

      /* Make sub-tabs sticky but without frame/shadow */
      .tab-content .nav-tabs {
        position: sticky;
        top: 42px;
        z-index: 1039;
        background-color: #fff;
        padding-top: 5px;
        padding-bottom: 5px;
        /* Removed: border-bottom and box-shadow */
      }

      /* Add proper spacing after main tabs */
      .tab-content {
        margin-top: 10px;
        padding-top: 40px;
      }

      /* Style for periodo filter container - make it sticky */
      .periodo-filter-container {
        position: sticky;
        top: 90px; /* Position it below the two tab rows */
        z-index: 1038; /* Below the tabs but above other content */
        border: 1px solid #e0e0e0;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
        background-color: #f9f9f9;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
      }

      /* For detail tables: prevent text wrapping, clip if too long */
      .dt-nowrap td, .dt-nowrap th {
        white-space: nowrap;
        text-overflow: ellipsis;
        overflow: hidden;
        max-width: 250px; /* adjust as needed */
      }

      /* Ensure modals (detail tables, etc.) appear above sticky tabs */
      .modal {
        z-index: 9999 !important;
      }
    "))
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

  # Set "Buscar" as default subtab when clicking on "Dados"
  observeEvent(input$pagePanels, {
    if (input$pagePanels == "Dados") {
      updateTabsetPanel(session, "dadosSubtabs", selected = "Buscar")
    }
  })

  # Set default subtabs when clicking on main tabs
  observeEvent(input$pagePanels, {
    if (input$pagePanels == "Panorama") {
      updateTabsetPanel(session, "panoramaSubTabs", selected = "Financeiro")
    } else if (input$pagePanels == "Dados") {
      updateTabsetPanel(session, "dadosSubtabs", selected = "Buscar")
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


          # ===================================================================
          # PAINEL: DADOS
          # ===================================================================
          tabPanel(
            "Dados",
            value = "Dados",
            fluidPage(
              tabsetPanel(
                id = "dadosSubtabs",
                tabPanel(
                  "Buscar",
                  value = "Buscar",
                  fluidPage(
                    b_dados_ui("buscar")
                  )
                ),
                tabPanel(
                  "Mapas",
                  value = "Mapas",
                  fluidPage(
                    tags$div(
                      class = "alert alert-info",
                      style = "margin-bottom: 15px; padding: 10px; border-left: 3px solid #007BFF; background-color: #f8f9fa;",
                      tags$strong("Calloutbox")
                    ),
                    h2("Fluxo dos dados"),
                    g_gnw_ui("gnw")
                  )
                ),
                tabPanel(
                  "Metadados",
                  value = "Metadados",
                  fluidPage(
                    h2("Geral"),
                    tags$strong("Data de geração dos dados:"), " ", dados_l[["metadados"]]$data_geracao,
                    br(),
                    # Botão para copiar caminho do arquivo original para importação/validação
                    actionButton(
                      "copyPath",
                      "Copiar caminho do arquivo para área de transferência"
                    ),
                    textOutput("copyConfirmation"),
                    hr(),
                    h2("Arquivos"),
                    h3("Gráficos"),
                    # Visualização de histograma para análise de metadados
                    g_metadados.hist_i(
                      "metaHist",
                      choices = setdiff(
                        names(dados_l[["metadados"]]$metadados), "Arquivo"
                      )
                    ),
                    h3("Tabela"),
                    # Exibir metadados em uma tabela interativa
                    fluidRow(
                      column(
                        12,
                        DT::datatable(
                          dados_l[["metadados"]]$metadados,
                          options = list(
                            pageLength = 10,
                            autoWidth = TRUE,
                            scrollX = TRUE,
                            searchHighlight = TRUE,
                            language = list(
                              url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json",
                              searchPanes = list(
                                collapse = "Filtros",
                                title = "Filtros"
                              )
                            )
                          ),
                          filter = "top",
                          class = "cell-border stripe",
                          rownames = FALSE
                        ) %>%
                          DT::formatStyle(
                            columns = colnames(dados_l[["metadados"]]$metadados),
                            backgroundColor = "rgba(240, 240, 240, 0.5)",
                            color = "black"
                          )
                      )
                    )
                  )
                )
              )
            )
          ),

          # ===================================================================
          # PAINEL: PANORAMA
          # ===================================================================
          tabPanel("Panorama",
            value = "Panorama",
            fluidPage(
              tabsetPanel(
                id = "panoramaSubTabs",
                tabPanel("Comercial",
                  value = "Comercial",
                  h2("Comercial"),
                  tags$div(
                    h3("Gráficos"),
                    tags$ul(
                      tags$li("Vendas por empreendimento"),
                      tags$li("Desempenho de corretores"),
                      tags$li("Funil de vendas e conversões"),
                      tags$li("Projeção de receitas comerciais")
                    )
                  )
                ),
                # Subpainel: Financeiro - Exibe gráficos de despesas e receitas
                tabPanel("Financeiro",
                  value = "Financeiro",
                  fluidPage(
                    # Filtro temporal para os gráficos financeiros
                    div(
                      class = "periodo-filter-container",
                      h4("Período em análise:", style = "margin-top: 0; margin-bottom: 10px;"),
                      filtro_periodo_module_ui("myFiltro")
                    ),
                    h2("Custo de obras"),
                    # Gráfico de trajetória dos custos de obras
                    gs_barras.cef.cobra_ui("gs_barras.cef.cobra"),
                    h2("Despesas"),
                    # Gráfico de trajetória de despesas com opções de segmentação
                    g_barras.empilhadas.mes_ui(
                      "g_barras.empilhadas.mes.desp",
                      choices = c(
                        "Agente Financeiro", "Credor", "Centro de Negócio",
                        "Empresa", "N° Conta", "Parcela"
                      ),
                      total = "Total Pago", # numeric column
                      data = "Data Doc Pagto", # date column
                      comeco.titulo = "Despesas empilhadas por" # static prefix for chart title
                    ),
                    h2("Extratos"),
                    g_barras.empilhadas.mes_ui(
                      "g_barras.empilhadas.mes.extcef",
                      choices = c(
                        "Cliente", "Conta_interno"
                      ),
                      total = "Valor",
                      data = "Data de movimento",
                      comeco.titulo = "Entradas e saídas empilhadas por"
                    ),
                    h2("Receitas"),
                    # Gráfico de trajetória de receitas com todas as dimensões disponíveis
                    g_barras.empilhadas.mes_ui(
                      "g_barras.empilhadas.mes.rec",
                      choices = c(
                        "Agente", "Cart.", "Cliente", "Elemento",
                        "Empreendimento", "Empresa", "Esp", "Parcela", "R/F",
                        "Torre"
                      ),
                      total = "Total", # numeric column
                      data = "Data Pagto", # date column
                      comeco.titulo = "Receitas empilhadas por" # static prefix for chart title
                    )
                  )
                ),
                tabPanel("Obras",
                  value = "Obras",
                  h2("Cronograma dos contratos com a CEF"),
                  "Linha de crédito: Apoio à Produção",
                  tags$div(
                    g_cronogramas_cef_ui("g_cronogramas.cef"),
                    h2("Pendências"),
                    tags$ul(
                      tags$li("Progresso dos empreendimentos por etapa"),
                    )
                  )
                )
              )
            )
          ),

          # ===================================================================
          # PAINEL: RELATÓRIOS
          # ===================================================================
          tabPanel("Relatórios",
            value = "Relatórios",
            fluidPage(
              h2("Conciliação de extratos"),
              "Links para as planilhas.",
              h2("Demonstrativo de fluxo de caixa"),
              "Links para as planilhas.",
              h2("Orçado x Realizado"),
              "Links para as planilhas.",
              h2("Relatório de inadimplência"),
              "Links para as planilhas."
            )
          )
        )
      )
    }
  })

  # ===========================================================================
  # INICIALIZAÇÃO DOS MÓDULOS
  # ===========================================================================

  # Inicializa o módulo de busca de dados
  b_dados_server("buscar", dados_l)

  # Inicializa o módulo de filtro de período e obtém valores selecionados
  filtroVals <- filtro_periodo_module_server("myFiltro")

  gs_barras.cef.cobra_server(
    "gs_barras.cef.cobra",
    dados = dados_l[["cef"]][["dcd"]],
    filtro_periodo = filtroVals$filtro_periodo,
    data_inicial = filtroVals$data_inicial,
    data_final = filtroVals$data_final,
    positive = c("SALDO MUTUARIO (PJ)", "SALDO MUTUARIO (PF)"),
    negative = c("MAXIMO LIB. ETAPA (PJ)"),
    line = "GARANTIA TERMINO OBRA",
    date = "Data de consulta",
    ref_line_col = "VR CUSTO OBRA"
  )

  # Inicializa o módulo de visualização de despesas
  g_barras.empilhadas.mes_server(
    "g_barras.empilhadas.mes.desp",
    dados = dados_l[["ik"]][["desp"]],
    filtro_periodo = filtroVals$filtro_periodo,
    data_inicial = filtroVals$data_inicial,
    data_final = filtroVals$data_final,
    max_unicos_i = 20,
    total = "Total Pago", # same as the UI param
    data = "Data Doc Pagto", # same as the UI param
    comeco.titulo = "Despesas" # same as the UI param
  )

  # Inicializa o módulo de visualização de receitas
  g_barras.empilhadas.mes_server(
    "g_barras.empilhadas.mes.rec",
    dados = dados_l[["ik"]][["rec"]],
    filtro_periodo = filtroVals$filtro_periodo,
    data_inicial = filtroVals$data_inicial,
    data_final = filtroVals$data_final,
    max_unicos_i = 20,
    total = "Total", # same as the UI param
    data = "Data Pagto", # same as the UI param
    comeco.titulo = "Receitas empilhadas por" # same as the UI param
  )

  # Inicializa o módulo de visualização dos extratos
  g_barras.empilhadas.mes_server(
    "g_barras.empilhadas.mes.extcef",
    dados          = dados_l[["cef"]][["extcef"]],
    filtro_periodo = filtroVals$filtro_periodo,
    data_inicial   = filtroVals$data_inicial,
    data_final     = filtroVals$data_final,
    max_unicos_i   = 20,
    total          = "Valor",
    data           = "Data de movimento",
    comeco.titulo  = "Entradas e saídas empilhadas por"
  )

  # Inicializa o módulo de cronogramas da CEF com o dcd filtrado
  dcd_filtrado <- dados_l[["cef"]][["dcd"]] %>%
    group_by(EMPREENDIMENTO) %>%
    filter(`Data de consulta` == max(`Data de consulta`, na.rm = TRUE)) %>%
    ungroup()
  g_cronogramas_cef_server(
    "g_cronogramas.cef",
    dados = dcd_filtrado
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

  # GNW module (no time filter)
  g_gnw_server("gnw", dados_l[["metadados"]])

  # ===========================================================================
  # FUNCIONALIDADE: COPIAR CAMINHO DO ARQUIVO
  # ===========================================================================
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

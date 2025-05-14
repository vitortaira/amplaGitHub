# filepath: c:\Users\Ampla\AMPLA INCORPORADORA LTDA\Controladoria - Documentos\amplaGitHub\amplaShiny\R\g_gnw.R

# Need both libraries
library(visNetwork)
library(htmlwidgets) # Required for onRender function

g_gnw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    visNetworkOutput(ns("gnw_network"), width = "100%", height = "500px")
  )
}

g_gnw_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    output$gnw_network <- renderVisNetwork({
      # Simplified validation - check just the core data
      req(dados$gnw_nodes, dados$gnw_edges)

      # For the legend nodes, create them on the fly if they don't exist
      legend_nodes <- if (!is.null(dados$gnw_nodes_legends)) {
        dados$gnw_nodes_legends
      } else {
        # Create default legend
        data.frame(
          label = c("Arquivos", "Origens", "Base de dados", "Relatórios", "Decisões"),
          color = c("yellow", "orange", "red", "blue", "purple"),
          shape = "box"
        )
      }

      # Create visNetwork with the data passed in
      visNetwork(
        dados$gnw_nodes,
        dados$gnw_edges,
        width = "100%",
        height = "500px",
        main = "Fluxo dos dados",
        footer = "Da informação à decisão."
      ) %>%
        visNodes(
          shape = "box",
          widthConstraint = list(maximum = 250),
          font = list(multi = TRUE)
        ) %>%
        visEdges(arrows = "to", width = 5) %>%
        visHierarchicalLayout(
          enabled = TRUE,
          direction = "LR",
          levelSeparation = 300,
          nodeSpacing = 200,
          blockShifting = FALSE,
          parentCentralization = FALSE,
          edgeMinimization = TRUE,
          sortMethod = "directed"
        ) %>%
        visPhysics(
          enabled = TRUE,
          solver = "repulsion",
          repulsion = list(
            nodeDistance    = 200,
            centralGravity  = 0.2,
            springLength    = 200,
            springConstant  = 0.05
          ),
          stabilization = list(enabled = TRUE, iterations = 1000)
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
          nodesIdSelection = TRUE
        ) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(
          position  = "left",
          main      = "Legendas:",
          useGroups = FALSE,
          addNodes  = legend_nodes, # Use the legend_nodes variable
          zoom      = FALSE
        ) %>%
        htmlwidgets::onRender("
          function(el, x) {
            setTimeout(function() {
              var graphArea = el.querySelector('div[id^=\"graphhtmlwidget-\"]');
              if (graphArea) {
                graphArea.style.backgroundColor = 'lightgray';
              }
            }, 500);
          }
        ")
    })
  })
}

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
      # Check the proper path
      req(dados$gnw_nodes, dados$gnw_edges)

      # Use correct data paths
      visNetwork(
        dados$gnw_nodes,
        dados$gnw_edges,
        width = "100%",
        height = "500px",
        footer = "Da informação à decisão."
      ) %>%
        visNodes(
          shape = "box",
          widthConstraint = list(maximum = 250),
          font = list(multi = TRUE)
        ) %>%
        visEdges(
          arrows = "to",
          width = 5,
          color = list(color = dados$gnw_edges$color)
        ) %>%
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
          nodesIdSelection = FALSE
        ) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(
          position  = "left",
          main      = "Legendas:",
          useGroups = FALSE,
          addNodes  = dados$gnw_nodes_legends,
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

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
      req(dados$gnw_nodes, dados$gnw_edges)

      visNetwork(
        dados$gnw_nodes,
        dados$gnw_edges,
        width = "100%",
        height = "500px"
      ) %>%
        visNodes(shape = "box", font = list(multi = TRUE)) %>%
        visEdges(
          arrows = "to",
          smooth = list(enabled = TRUE, type = "cubicBezier")
        ) %>%
        visHierarchicalLayout(
          enabled = TRUE,
          direction = "LR",
          levelSeparation = 700,
          nodeSpacing = 350,
          edgeMinimization = TRUE
        ) %>%
        visOptions(
          highlightNearest = list(
            algorithm = "all",
            enabled = TRUE,
            hover = FALSE
          ),
          nodesIdSelection = FALSE
        ) %>%
        visInteraction(navigationButtons = TRUE, dragView = TRUE) %>%
        visLegend(
          position = "left",
          main = "Legendas:",
          useGroups = FALSE,
          addNodes = dados$gnw_nodes_legends,
          zoom = FALSE
        ) %>%
        addFontAwesome() %>%
        visEvents(stabilizationIterationsDone = "function() {
          this.setOptions({nodes: {fixed: {x: true, y: true}}});
        }")
    })
  })
}

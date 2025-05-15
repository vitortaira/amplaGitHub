#' @title Create data flow graph visualization
#'
#' @description
#' Creates a visNetwork visualization of data flow using nodes_df and edges_df
#'
#' @return A visNetwork object representing the data flow
#'
#' @importFrom htmlwidgets onRender
#' @importFrom visNetwork visNetwork visNodes visEdges visHierarchicalLayout visPhysics visOptions visInteraction visLegend
#' @importFrom magrittr %>%
#' @export
m_gnw <- function() {
  # Ensure required libraries are available
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.")
  }
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required.")
  }

  # Get nodes and edges if not available in environment
  if (!exists("nodes_df") || !exists("edges_df") || !exists("nodes.legends_df")) {
    nodes_data <- m_nodes()
    nodes_df <- nodes_data$nodes
    nodes.legends_df <- nodes_data$nodes.legends
    edges_df <- m_edges()
  }

  # Renderização do grafo com visNetwork
  dados_gnw <-
    visNetwork(
      nodes_df,
      edges_df,
      width = "100%",
      height = "500px",
      main = "Fluxo dos dados",
      footer = "Da informação à decisão."
    ) %>%
    # Ajustes globais dos nodes (forma "box", quebra de texto automática)
    visNodes(
      shape = "box",
      widthConstraint = list(maximum = 250),
      font = list(multi = TRUE)
    ) %>%
    # Ajustes globais das edges
    visEdges(
      arrows = "to",
      width = 5
    ) %>%
    # Layout hierárquico com espaçamento maior
    visHierarchicalLayout(
      enabled = TRUE,
      direction = "LR",
      levelSeparation = 300, # Aumenta o espaçamento vertical
      nodeSpacing = 200, # Aumenta o espaçamento horizontal entre nós irmãos
      blockShifting = FALSE,
      parentCentralization = FALSE,
      edgeMinimization = TRUE,
      sortMethod = "directed"
    ) %>%
    # Física (physics) pode ajudar a evitar sobreposições
    visPhysics(
      enabled = TRUE,
      solver = "repulsion",
      repulsion = list(
        nodeDistance = 200,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.05
      ),
      stabilization = list(enabled = TRUE, iterations = 1000)
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
      nodesIdSelection = FALSE
    ) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLegend(
      position = "left",
      main = "Legendas:",
      useGroups = FALSE,
      addNodes = nodes.legends_df,
      zoom = FALSE
    ) %>%
    onRender(
      "
      function(el, x) {
        setTimeout(function() {
          var graphArea = el.querySelector('div[id^=\"graphhtmlwidget-\"]');
          if (graphArea) {
            graphArea.style.backgroundColor = 'lightgray';
          }
        }, 500);
      }
      "
    )

  return(dados_gnw)
}

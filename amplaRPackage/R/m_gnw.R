#' @title Create data flow graph visualization
#'
#' @description
#' Creates a visNetwork visualization of data flow using nodes_df and edges_df
#'
#' @return A shiny app representing the data flow
#'
#' @importFrom shiny shinyApp fluidPage inputPanel selectInput visNetworkOutput
#' @importFrom htmlwidgets onRender
#' @importFrom visNetwork visNetwork visNodes visEdges visHierarchicalLayout visPhysics visOptions visInteraction visLegend
#' @importFrom magrittr %>%
#' @export
m_gnw <- function() {
  library(shiny)
  library(visNetwork)

  nodes_data <- m_nodes()
  nodes_df <- nodes_data$nodes
  nodes.legends_df <- nodes_data$nodes.legends
  edges_df <- m_edges()

  ui <- fluidPage(
    div(
      class = "minimal-frame",
      selectInput("edge_color", "Filtrar cor da aresta:",
        choices = c("Todas", unique(edges_df$color)), selected = "Todas"
      )
    ),
    div(
      class = "minimal-frame chart-frame",
      div(class = "minimal-divider"),
      visNetworkOutput("graph", width = "100%", height = "600px")
    )
  )

  server <- function(input, output, session) {
    # Store original data
    original_nodes <- nodes_df
    original_edges <- edges_df

    # Use reactiveVal for current state
    current_nodes <- reactiveVal(original_nodes)
    current_edges <- reactiveVal(original_edges)

    filtered_edges <- reactive({
      color <- input$edge_color
      if (is.null(color) || color == "Todas") {
        current_edges()
      } else {
        current_edges()[current_edges()$color == color, ]
      }
    })

    filtered_nodes <- reactive({
      if (is.null(input$edge_color) || input$edge_color == "Todas") {
        current_nodes()
      } else {
        edges <- filtered_edges()
        node_ids <- unique(c(edges$from, edges$to))
        current_nodes()[current_nodes()$id %in% node_ids, ]
      }
    })

    output$graph <- renderVisNetwork({
      visNetwork(
        filtered_nodes(),
        filtered_edges(),
        main = "Fluxo dos dados",
        footer = "Da informação à decisão."
      ) %>%
        visNodes(shape = "box", font = list(multi = TRUE)) %>%
        visEdges(arrows = "to", smooth = list(enabled = TRUE, type = "cubicBezier")) %>%
        visHierarchicalLayout(
          enabled = TRUE,
          direction = "LR",
          levelSeparation = 700,
          nodeSpacing = 350,
          edgeMinimization = TRUE
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
          nodesIdSelection = FALSE
        ) %>%
        visInteraction(navigationButtons = TRUE, dragView = TRUE) %>%
        visLegend(
          position = "left",
          main = "Legendas:",
          useGroups = FALSE,
          addNodes = nodes.legends_df,
          zoom = FALSE
        ) %>%
        addFontAwesome() %>%
        visEvents(stabilizationIterationsDone = "function() {
          this.setOptions({nodes: {fixed: {x: true, y: true}}});
        }")
    })
  }

  shinyApp(ui, server)
}

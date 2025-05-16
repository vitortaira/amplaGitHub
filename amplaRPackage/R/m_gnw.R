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
    tags$style(HTML("
      .minimal-frame {
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 16px;
        background: #fafbfc;
        box-shadow: 0 2px 6px rgba(0,0,0,0.03);
        margin-bottom: 20px;
      }
      .minimal-title {
        font-size: 1.3em;
        font-weight: 500;
        margin-bottom: 10px;
        color: #333;
      }
      .chart-frame {
        border: 2px solid #bbb;
        border-radius: 10px;
        background: #fff;
        padding: 10px;
        margin-bottom: 20px;
        position: relative;
      }
      /* Make legend box background light gray */
      .vis-legend {
        background-color: #f0f0f0 !important;
        border: 1px solid #ddd !important;
        border-radius: 4px !important;
        padding: 8px !important;
      }
    ")),
    div(class = "minimal-frame",
      div(class = "minimal-title", "Filtro de Arestas"),
      selectInput("edge_color", "Filtrar cor da aresta:",
                  choices = c("Todas", unique(edges_df$color)), selected = "Todas")
    ),
    div(class = "minimal-frame chart-frame",
      div(class = "minimal-title", "Visualização"),
      visNetworkOutput("graph", width = "100%", height = "600px")
    )
  )

  server <- function(input, output, session) {
    filtered_edges <- reactive({
      color <- input$edge_color
      if (is.null(color) || color == "Todas") {
        edges_df
      } else {
        edges_df[edges_df$color == color, ]
      }
    })

    filtered_nodes <- reactive({
      if (is.null(input$edge_color) || input$edge_color == "Todas") {
        nodes_df
      } else {
        edges <- filtered_edges()
        node_ids <- unique(c(edges$from, edges$to))
        nodes_df[nodes_df$id %in% node_ids, ]
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
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(
          position = "left",
          main = "Legendas:",
          useGroups = FALSE,
          addNodes = nodes.legends_df,
          zoom = FALSE
        )
    })
  }

  shinyApp(ui, server)
}

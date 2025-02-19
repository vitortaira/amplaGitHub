library(htmlwidgets)
library(visNetwork)

source(here("dados", "mapa", "Fluxo dos dados - nodes_df.R"))
source(here("dados", "mapa", "Fluxo dos dados - edges_df.R"))

# Renderização do grafo com visNetwork
dados_gnw <- 
  visNetwork(
    nodes_df, 
    edges_df, 
    width = "100%", 
    height = "800px",
    main = "Fluxo dos dados",
    footer = "Da informação à decisão."
  ) %>%
  # Ajustes globais dos nodes (forma "box", quebra de texto automática)
  visNodes(
    shape = "box",
    widthConstraint = list(maximum = 160), 
    font = list(multi = TRUE)
  ) %>%
  # Ajustes globais das edges
  visEdges(
    arrows = "to",
    color = "white"
  ) %>%
  # Layout hierárquico com espaçamento maior
  visHierarchicalLayout(
    enabled = TRUE, 
    direction = "LR",
    levelSeparation = 1200,  # Aumenta o espaçamento vertical
    nodeSpacing = 250,      # Aumenta o espaçamento horizontal entre nós irmãos
    blockShifting = FALSE,
    parentCentralization = FALSE,
    edgeMinimization = TRUE,  # Ensures edges are less likely to cross
    sortMethod = "directed" # "hubsize" is another possibility
  ) %>%
  # Física (physics) pode ajudar a evitar sobreposições
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",  # Use 'repulsion' instead of 'hierarchicalRepulsion' for better space management
    repulsion = list(
      nodeDistance = 200,   # Increased distance between nodes to prevent overlap
      centralGravity = 0.2,  # Adjust central gravity for better spread
      springLength = 200,    # Increased spring length for better spacing
      springConstant = 0.05
    ),
    stabilization = list(enabled = TRUE, iterations = 1000)
  ) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
    nodesIdSelection = TRUE
  ) %>% 
  visInteraction(navigationButtons = TRUE) %>% 
  visLegend(
    position = "left",
    main = "Legendas:",
    useGroups = FALSE,
    addNodes = nodes.legends_df,
    addEdges = edges.legends_df,
    zoom = FALSE
  ) %>% 
  onRender(
    "
    function(el, x) {
      setTimeout(function() {
        // Target the main graph area by its id (using a querySelector that looks for id starting with 'graphhtmlwidget')
        var graphArea = el.querySelector('div[id^=\"graphhtmlwidget-\"]');
        if (graphArea) {
          // Set the background color of the main plot area to black
          graphArea.style.backgroundColor = 'black';
        }
      }, 500); // Increased delay to ensure all elements are rendered
    }
    "
  )

dados_gnw

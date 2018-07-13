## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(magrittr)
library(visNetwork)
metrics_nodes <- read.csv(file = "./metrics_nodes.csv")
metrics_edges <- read.csv(file = "./metrics_edges.csv")

## ---- echo = FALSE, fig.width=7, out.width='100%'------------------------
visNetwork(nodes = metrics_nodes, edges = metrics_edges, height = "1000px") %>% 
  visGroups(groupname = "landscape", color = "steelblue") %>%
  visGroups(groupname = "classes", color = "#1e9c87") %>%
  visGroups(groupname = "patches", color = "#bade26") %>%
  visEdges(smooth = TRUE) %>% 
  visGroups(groupname = "area_and_shape", color = "orange") %>% 
  visOptions(highlightNearest = list(enabled = TRUE, 
                                     algorithm = "hierarchical", 
                                     hover = TRUE, 
                                     degree = list(from = 2, to = 0)), 
             selectedBy = list(variable = "calculate_per", multiple = TRUE)) %>% 
  visHierarchicalLayout(direction = "LR") %>%
  visPhysics(stabilization = FALSE)


library(networkD3)
library(igraph)

# Load data
source <- read.csv("data/win1997_source_edge_list.txt", header = FALSE)
target <- read.csv("data/win1997_target_edge_list.txt", header = FALSE)

nodes <- read.csv("data/win1997_node_list.txt", header = TRUE)
edges <- read.csv("data/win1997_edge_list.txt", header = TRUE)

# Create Graphs

networkData <- data.frame(source, target)

out <- matrix(unlist(lapply(edges, as.character)), ncol = 2, byrow = TRUE)
g <- graph_from_edgelist(out, directed = TRUE)

wc <- cluster_optimal(g)
members <- membership(wc)
graph_d3 <- igraph_to_networkD3(g,group = members)

# Plot
simpleNetwork(networkData)
forceNetwork(Links = graph_d3$links,
             Nodes = graph_d3$nodes,
             Source = "source",
             Target = "target",
             Group = "group",
             NodeID = "name")


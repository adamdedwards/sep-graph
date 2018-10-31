
library(networkD3)
library(igraph)
library(magrittr)

#################### Load data

nodes <- read.csv("data/win1997_node_list.txt", header = TRUE)
edges <- read.csv("data/win1997_edge_list.txt", header = TRUE)

#################### Create Graphs

gg <- graph_from_edgelist(matrix(unlist(lapply(edges, as.character)), ncol = 2, byrow = TRUE), directed = FALSE)
gg <- simplify(gg, remove.multiple = TRUE, remove.loops = TRUE,edge.attr.comb = igraph_opt("edge.attr.comb"))

members <- membership(cluster_fast_greedy(gg))

graph_d3 <- igraph_to_networkD3(gg,group = members)

graph_d3$nodes$degree <- as.character(degree(gg, v = V(gg)))
graph_d3$nodes$btwn <- as.character(betweenness(gg, v = V(gg), directed = TRUE))

# Plot
save <- forceNetwork(Links = graph_d3$links,
             Nodes = graph_d3$nodes,
             Source = "source",
             Target = "target",
             Group = "group",
             NodeID = "name", 
             Nodesize = "btwn",
             charge = -50,
             arrows = FALSE,
             legend = FALSE,
             bounded = FALSE,
             opacity = 1,
             fontFamily = "sans-serif",
             fontSize = 14,
             opacityNoHover = 0,
             zoom = TRUE
             ) 

saveNetwork(save, file = 'Net1.html')

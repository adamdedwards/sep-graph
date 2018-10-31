
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)

##############################     Load Data    ##############################

nodes <- read.csv("data/win1997_node_list.txt", header = TRUE)
edges <- read.csv("data/win1997_edge_list.txt", header = TRUE)

##############################  Create Graphs   ##############################

gg <- graph_from_edgelist(as.matrix(edges), directed = TRUE)                 # Load edge csv data.frame into an igraph object
gg <- simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)              # Simplify igraph object; remove dups and loops

############################## Graph Attributes ##############################

i <- cluster_walktrap(gg)$membership
gg <- set_vertex_attr(gg,"group",index = V(gg),i)                            # Create a "group" attribute and load it into node attributes

c <- rainbow(max(V(gg)$group))
gg <- set_vertex_attr(gg, "color",index = V(gg),c[i])                        # Add node colors based on group membership

graph_d3 <- igraph_to_networkD3(gg,group = i)
graph_d3$nodes$degree <- as.character(degree(gg, v = V(gg)))
graph_d3$nodes$btwn <- as.character(betweenness(gg, v = V(gg), directed = TRUE))

##############################        Plot      ############################## 

forceNetwork(Links = graph_d3$links,
             Nodes = graph_d3$nodes,
             Source = "source",
             Target = "target",
             Group = "group",
             NodeID = "name", 
             Nodesize = "degree",
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


graph <- graphjs(gg)
gg

graphjs(gg, layout=layout_with_fr(gg, dim=3))

gg  <- set_edge_attr(gg,"color", value = "#000000")
gg  <- set_edge_attr(gg, "weight", value=10)


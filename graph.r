
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)


##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season))      { loc <- paste("data/win",year,"_edge_list.txt",sep="")
  } else if(missing(year)) { loc <- paste("data/",season,"1997_edge_list.txt",sep="")
  } else                   { loc <- paste("data/",season,year,"_edge_list.txt",sep="")
  }
  
  edges <- read.csv(loc, header = TRUE)
  return(edges)
}

edgelist <- load.data(year=2016)

##############################  Create Graphs   ##############################

create.graph <- function(data) {
  gg <- graph_from_edgelist(as.matrix(data), directed = FALSE)               # load edge csv data.frame into an igraph object
  gg <- simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)            # simplify igraph object; remove dups and loops
  return(gg)
}

gg <- create.graph(edgelist)

############################## Graph Attributes ##############################

# igraph/threejs

gg <- set_vertex_attr(gg,"label",index = V(gg),V(gg)$name)

i <- cluster_edge_betweenness(gg)$membership                                 # create a "group" attribute and load it into node attributes
gg <- set_vertex_attr(gg,"group",index = V(gg),i)

c <- rainbow(max(V(gg)$group))                                               # add node/edge colors based on group membership
gg <- set_vertex_attr(gg, "color", index = V(gg),c[i])
gg <- set_edge_attr(gg, "color", index = E(gg), head_of(gg, E(gg))$color)

b <- betweenness(gg, v=V(gg))                                                # add node size based on betweenness centrality
b <- b/max(b)*10
gg <- set_vertex_attr(gg, "size", index = V(gg),b[i])

simple.graphics <- function(graph) {
  graph <- set_vertex_attr(graph, "color", index = V(graph),"red")
  graph <- set_edge_attr(graph, "color", index = E(graph), "red")
  graph <- set_vertex_attr(graph, "size", index = V(graph),1)
}

gg <- simple.graphics(gg)

# networkd3

graph_d3 <- igraph_to_networkD3(gg,group = i)
graph_d3$nodes$degree <- as.character(degree(gg, v = V(gg)))
graph_d3$nodes$btwn <- as.character(betweenness(gg, v = V(gg), directed = TRUE))

##############################        Plot      ############################## 

graphjs(gg, layout=layout_with_fr(gg, dim=3),vertex.label=V(gg)$label)       # Visualize graph with threejs

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
             )                                                               # Visualize graph with networkD3





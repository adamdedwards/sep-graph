
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)


##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season))      { loc <- paste("data/fall",year,"_edge_list.txt",sep="")
  } else if(missing(year)) { loc <- paste("data/",season,"1997_edge_list.txt",sep="")
  } else                   { loc <- paste("data/",season,year,"_edge_list.txt",sep="")
  }
  
  edges <- read.csv(loc, header = TRUE)
  return(edges)
}


##############################  Create Graphs   ##############################

create.graph <- function(data) {
  gg <- graph_from_edgelist(as.matrix(data), directed = FALSE)               # load edge csv data.frame into an igraph object
  gg <- simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)            # simplify igraph object; remove dups and loops
  return(gg)
}

############################## Graph Attributes ##############################

# igraph/threejs

simple.graphics <- function(g) {
  g <- set_vertex_attr(g, "color","red")
  g <- set_edge_attr(g, "color","red")
  g <- set_vertex_attr(g, "size",1)
}

complicated.graphics <-function(g) {
  g <- set_vertex_attr(g,"label",value = V(g)$name)                                # add labels
  
#  i <- cluster_edge_betweenness(g)$membership                                 # create a "group" attribute and load it into node attributes
  i <- cluster_fast_greedy(g)$membership
  g <- set_vertex_attr(g,"group",value = i)
  
  c <- rainbow(max(V(g)$group))                                               # add node/edge colors based on group membership
  g <- set_vertex_attr(g, "color",value = c[i])
  g <- set_edge_attr(g, "color",value = head_of(g, E(g))$color)
  
  b <-  unlist((evcent(g)$vector*10)+1)                                           # add node size based on eigenvector centrality
  g <- set_vertex_attr(g, "size",value = b)
}

overlapping.graphics <-function(g){
  
  
  
}

# networkd3

graph_d3 <- igraph_to_networkD3(gg,group = i)
graph_d3$nodes$degree <- as.character(degree(gg, v = V(gg)))
graph_d3$nodes$btwn <- as.character(betweenness(gg, v = V(gg), directed = TRUE))

##############################         Run      ##############################

sp.edgelist <- load.data(season="spr",year=1998)
su.edgelist <- load.data(season="sum",year=1998)
fa.edgelist <- load.data(season="fall",year=1998)
wi.edgelist <- load.data(season="win",year=1998)

gg.1 <- create.graph(sp.edgelist)
gg.2 <- create.graph(su.edgelist)
gg.3 <- create.graph(fa.edgelist)
gg.4 <- create.graph(wi.edgelist)


gg <- simple.graphics(gg)
gg <- complicated.graphics(gg)

gg.1 <- complicated.graphics(gg.1)
gg.2 <- complicated.graphics(gg.2)
gg.3 <- complicated.graphics(gg.3)
gg.4 <- complicated.graphics(gg.4)



##############################        Plot      ############################## 

# Static Graph
graphjs(gg, layout=layout_with_fr(gg, dim=3),vertex.label=V(gg)$label)       # Visualize graph with threejs

graphjs(gg.1, layout=layout_with_fr(gg.1, dim=3),vertex.label=V(gg.1)$label)
graphjs(gg.2, layout=layout_with_fr(gg.2, dim=3),vertex.label=V(gg.2)$label)
graphjs(gg.3, layout=layout_with_fr(gg.3, dim=3),vertex.label=V(gg.3)$label)
graphjs(gg.4, layout=layout_with_fr(gg.4, dim=3),vertex.label=V(gg.4)$label)

#Animated Graph
graphjs(list(gg.1,
             gg.2),
 layout=list(layout_with_fr(gg.1, dim=3),
             layout_with_fr(gg.2, dim=3)),
   main=list("Phase 1",
             "Phase 2"),
    fpl=300)





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

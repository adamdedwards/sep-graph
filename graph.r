library(ndtv)
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)
library(htmlwidgets)
library(intergraph)


##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season) & missing(year))      { loc <- paste("data/fall1998_edge_list.txt",sep="")
  } else if(missing(year))   { loc <- paste("data/",season,"1998_edge_list.txt",sep="")
  } else if(missing(season)) { loc <- paste("data/fall",year,"_edge_list.txt",sep="")
  } else                     { loc <- paste("data/",season,year,"_edge_list.txt",sep="")
  }
  print(paste("opening ",loc))
  edges <- read.csv(loc, header = FALSE) #should be TRUE?
  return(edges)
}


##############################  Create Graphs(datatype:igraph)  ##############################

create.graph <- function(data) {
  g <- graph_from_edgelist(as.matrix(data), directed = FALSE)               # load edge csv data.frame into an igraph object
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)            # simplify igraph object; remove dups and loops
  return(g)
}

############################## Graph Attributes ##############################

# igraph/threejs

simple.graphics <- function(g) {
  g <- set_vertex_attr(g, "color",value=rgb(1,0,0,1))
  g <- set_vertex_attr(g, "size",value=1)
  
  for(v in 1:length(V(g))) {                                                 # hide all isolated vertices
    if(degree(g)[v]==0) {
      g <- set_vertex_attr(g, "color", index = v, value=rgb(0,0,0,0.01))
    }
  }
  
  g <- set_edge_attr(g, "color",value=rgb(1,0,0,1))
}

graphics <-function(g) {	
  g <- set_vertex_attr(g, "color",value=rgb(1,0,0,1))
  g <- set_vertex_attr(g, "stroke",value=NULL)
  g <- set_vertex_attr(g, "size",value=1)
  
  for(v in 1:length(V(g))) {                                                 # hide all isolated vertices
    if(degree(g)[v]==0) {
      g <- set_vertex_attr(g, "color", index = v, value=rgb(0,0,0,0.01))
    }
  }
  
  g <- set_edge_attr(g, "color",value=rgb(1,0,0,1))
  
  g <- set_vertex_attr(g,"label",value = V(g)$name)                           # add labels
  
#  i <- cluster_fast_greedy(g)$membership                                      # create a "group" attribute and load it into node attributes
#  g <- set_vertex_attr(g,"group",value = i)
#  c <- rainbow(max(V(g)$group))                                               # add node/edge colors based on group membership
#  g <- set_vertex_attr(g, "color",value = c[i])
#  g <- set_edge_attr(g, "color",value = head_of(g, E(g))$color)
  
  b <-  unlist((evcent(g)$vector*10)+1)                                       # add node size based on eigenvector centrality
  g <- set_vertex_attr(g, "size",value = b)
  

}

##############################         CREATE IGRAPH OBJECTS      ##############################

years <- 1998:2000
seasons <- c("spr","sum","fall","win")
iterations <- length(years)*length(seasons)
sep.graphs <- vector("list", iterations) 

for(i in 0:(iterations-1)) {
   x <- create.graph(load.data(season=seasons[(i%%4)+1],year=years[(i%/%4)+1]))
   sep.graphs[[i+1]] <- x
}


################################### CONSTRUCT GRAPH LIST FOR GRAPHJS ###################################

big.intersect.graph <- sep.graphs[[1]]
big.union.graph     <- graph.empty(directed=FALSE)
each.year <- vector("list", iterations)

for(i in 2:length(sep.graphs)) {
  big.intersect.graph <- intersection(big.intersect.graph,sep.graphs[[i]])
}

for(i in 1:iterations) {
  big.union.graph <- union(big.intersect.graph,sep.graphs[[i]])
  each.year[[i]] <- graphics(big.union.graph)
  
}

# Static Graph Images
graphjs(sep.graphs[[x]], layout=layout_with_fr(sep.graphs[[x]], dim=3),vertex.label=V(sep.graphs[[x]])$label)       # Visualize graph with threejs

graphjs(sep.graphs[[1]], layout=layout_with_fr(sep.graphs[[1]], dim=3),vertex.label=V(sep.graphs[[1]])$label)       # TODO: Replace with loop
graphjs(sep.graphs[[2]], layout=layout_with_fr(sep.graphs[[2]], dim=3),vertex.label=V(sep.graphs[[2]])$label)
graphjs(sep.graphs[[3]], layout=layout_with_fr(sep.graphs[[3]], dim=3),vertex.label=V(sep.graphs[[3]])$label)
graphjs(sep.graphs[[4]], layout=layout_with_fr(sep.graphs[[4]], dim=3),vertex.label=V(sep.graphs[[4]])$label)

# Dynamic Graph Animation
graphjs(each.year,
        main=rep(years,each=4),
        bg="white",
        vertex.shape="sphere",
        edge.curved=1,
        fpl=50)


###################################DYNAMIC NETWORK ANIMATIONS WITH NDTV###################################

sep.graphs.network <- vector("list",12)

for(i in 1:12) {
  sep.graphs.network[[i]] <- as.network(asNetwork(sep.graphs[[i]]))
}

dynet <- networkDynamic(as.network(asNetwork(big.intersect.graph)),network.list =sep.graphs.network,vertex.pid="vertex.names")


d3.options <- list( animationDuration=800, 
                    scrubDuration=0, 
                    enterExitAnimationFactor=0, 
                    nodeSizeFactor=0.01, 
                    playControls=TRUE, 
                    animateOnLoad=TRUE, 
                    slider=TRUE, 
                    debugFrameInfo=FALSE, 
                    debugDurationControl=FALSE)

render.par <- list(tween.frames=10,
                   show.time=TRUE,
                   show.stats=NULL,
                   extraPlotCmds=NULL,
                   initial.coords=0)

render.d3movie(dynet, filename=tempfile(fileext = '.html'), 
               render.par,
               plot.par=list(bg='white'),
               d3.options, 
               output.mode=c('HTML','JSON','inline','htmlWidget'),
               script.type=c('embedded','remoteSrc'),
               launchBrowser=TRUE,
               verbose=TRUE)














##############################    networkd3 Plots     ############################## 

# networkd3

graph_d3 <- igraph_to_networkD3(gg,group = i)
graph_d3$nodes$degree <- as.character(degree(gg, v = V(gg)))
graph_d3$nodes$btwn <- as.character(betweenness(gg, v = V(gg), directed = TRUE))


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

library(ndtv)
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)
library(htmlwidgets)
library(intergraph)
library(scatterplot3d)


##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season) & missing(year))      { loc <- paste("data/fall1998_edge_list.txt",sep="")
  } else if(missing(year))   { loc <- paste("data/",season,"1998_edge_list.txt",sep="")
  } else if(missing(season)) { loc <- paste("data/fall",year,"_edge_list.txt",sep="")
  } else                     { loc <- paste("data/",season,year,"_edge_list.txt",sep="")
  }
  print(paste("opening ",loc))
  edges <- read.csv(loc, header = TRUE) 
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

hide.isolated.vertices <- function(g) {
  for(v in 1:length(V(g))) {                                                 # hide all isolated vertices
    if(degree(g)[v]==0) {
      if("igraph" %in% (.packages())){
        g <- igraph::set_vertex_attr(g, "color", index = v, value=rgb(0,0,0,0.01))
      }
      else {
        g <- network::set.vertex.attribute(g, "color", index = v, value=rgb(0,0,0,0.01))
      }
      
    }
  }
  return(g)
}

simple.graphics <- function(g) {
  
  if("igraph" %in% (.packages())){
    g <- set_vertex_attr(g, "color" ,value = rgb(1,0,0,1))
    g <- set_vertex_attr(g, "stroke",value = NULL)
    g <- set_vertex_attr(g, "size"  ,value = 1)
    g <- set_vertex_attr(g, "label" ,value = V(g)$name)                           # add labels
    
    g <- hide.isolated.vertices(g)
    
    g <- set_edge_attr(g, "color",value=rgb(1,0,0,1))
  }
  else {
    g <- set.vertex.attribute(g, "color" ,value = rgb(1,0,0,1))
    g <- set.vertex.attribute(g, "stroke",value = NULL)
    g <- set.vertex.attribute(g, "size"  ,value = 1)
    g <- set.vertex.attribute(g, "label" ,value = V(g)$name)                           # add labels
    
    g <- hide.isolated.vertices(g)
    
    g <- set.edge.attribute(g, "color",value=rgb(1,0,0,1))
  }
  return(g)
}

graphics <-function(g,comm) {	
  if("igraph" %in% (.packages())){
  g <- set_vertex_attr(g, "color" ,value = rgb(1,0,0,1))
  g <- set_vertex_attr(g, "stroke",value = NULL)
  g <- set_vertex_attr(g, "size"  ,value = 1)
  g <- set_vertex_attr(g, "label" ,value = V(g)$name)                           # add labels

  g <- set_edge_attr(g, "color",value=rgb(1,0,0,1))
  
  
  g <- set_vertex_attr(g,"group",value = comm$membership)
  c <- rainbow(max(V(g)$group))                                               # add node/edge colors based on group membership
  g <- set_vertex_attr(g, "color",value = c[comm$membership])
  g <- set_edge_attr(g, "color",value = head_of(g, E(g))$color)
  
  b <- unlist((evcent(g)$vector*10)+1)                                       # add node size based on eigenvector centrality
  g <- set_vertex_attr(g, "size",value = b)
  }
  else {
    g <- set.vertex.attribute(g, "color" ,value = rgb(1,0,0,1))
    g <- set.vertex.attribute(g, "stroke",value = NULL)
    g <- set.vertex.attribute(g, "size"  ,value = 1)
    g <- set.vertex.attribute(g, "label" ,value = V(g)$name)                           # add labels

    g <- set.edge.attribute(g, "color",value=rgb(1,0,0,1))
    
    
    g <- set.vertex.attribute(g,"group",value = comm$membership)
    c <- rainbow(max(get.vertex.attribute(g,"group")))                                               # add node/edge colors based on group membership
    g <- set.vertex.attribute(g, "color",value = c[comm$membership])
    g <- set.edge.attribute(g, "color",value = get.edge.attribute(g,"color"))
    
    b <- unlist((evcent(g)$vector*10)+1)                                       # add node size based on eigenvector centrality
    g <- set.vertex.attribute(g, "size",value = b)
  }
  g <- hide.isolated.vertices(g)
  return(g)
}

##############################         CREATE IGRAPH OBJECTS      ##############################

years <- 1998:2000
seasons <- c("spr","sum","fall","win") #
iterations <- length(years)*length(seasons)
sep.graphs <- vector("list", iterations) 

for(i in 0:(iterations-1)) {
   x <- create.graph(load.data(season=seasons[(i%%length(seasons))+1],year=years[(i%/%length(seasons))+1]))
   sep.graphs[[i+1]] <- x
}


################################### CONSTRUCT GRAPH LIST FOR GRAPHJS ###################################

big.intersect.graph  <- sep.graphs[[1]]
big.union.graph      <- graph.empty(directed=FALSE)
full.node.set.graphs <- vector("list", iterations)

for(i in 2:length(sep.graphs)) {
  big.intersect.graph <- intersection(big.intersect.graph,sep.graphs[[i]])
}

#top.level.communities <- cluster_fast_greedy(full.node.set.graphs[[iterations]])

top.level.communities <- igraph::cluster_walktrap(full.node.set.graphs[[iterations]])

for(i in 1:iterations) {
  big.union.graph <- union(big.intersect.graph,sep.graphs[[i]])
#  full.node.set.graphs[[i]] <- simple.graphics(big.union.graph)                                         # edits graphics to color and hide nodes
  full.node.set.graphs[[i]] <- graphics(big.union.graph,top.level.communities)                                         # edits graphics to color and hide nodes
  
}

# Static Graph Images
graphjs(full.node.set.graphs[[x]], layout=layout_with_fr(full.node.set.graphs[[x]], dim=3),vertex.label=V(full.node.set.graphs[[x]])$label)       # Visualize graph with threejs

graphjs(full.node.set.graphs[[1]], layout=layout_with_fr(full.node.set.graphs[[1]], dim=3),vertex.label=V(full.node.set.graphs[[1]])$label)       # TODO: Replace with loop
graphjs(full.node.set.graphs[[2]], layout=layout_with_fr(full.node.set.graphs[[2]], dim=3),vertex.label=V(full.node.set.graphs[[2]])$label)
graphjs(full.node.set.graphs[[3]], layout=layout_with_fr(full.node.set.graphs[[3]], dim=3),vertex.label=V(full.node.set.graphs[[3]])$label)
graphjs(full.node.set.graphs[[4]], layout=layout_with_fr(full.node.set.graphs[[4]], dim=3),vertex.label=V(full.node.set.graphs[[4]])$label)

# Dynamic Graph Animation
graphjs(full.node.set.graphs,
        main=rep(years,each=4),
        bg="white",
        fpl=50)


###################################DYNAMIC NETWORK ANIMATIONS WITH NDTV###################################

design.sep.graphs <- vector("list",iterations)

full.node.set.graphs[[i]]

for(i in 1:iterations) {
  design.sep.graphs[[i]] <- graphics(full.node.set.graphs[[i]],top.level.communities)
}

sep.graphs.network <- vector("list",iterations)

for(i in 1:iterations) {
  sep.graphs.network[[i]] <- asNetwork(design.sep.graphs[[i]])
}

dynet <- networkDynamic(sep.graphs.network[[iterations]],network.list =sep.graphs.network,vertex.pid="label")


d3.options <- list( animationDuration=2000,
                    enterExitAnimationFactor=0, 
                    nodeSizeFactor=0.01, 
                    playControls=TRUE, 
                    animateOnLoad=TRUE, 
                    slider=TRUE, 
                    debugFrameInfo=FALSE)

render.par <- list(tween.frames=10,
                   show.time=TRUE,
                   show.stats=NULL,
                   extraPlotCmds=NULL,
                   initial.coords=0)

anim <- compute.animation(dynet)
anim <- compute.animation(dynet,animation.mode='kamadakawai')

render.d3movie(anim, 
               vertex.tooltip = paste("<b>Article:</b>", (anim %v% 'label')),
               edge.col = '#444444',
               vertex.cex = 1,
               vertex.col = dynet %v% 'color',
               filename=tempfile(fileext = '.html'), 
               render.par,
               plot.par=list(bg='white'),
               d3.options, 
               output.mode=c('HTML','JSON','inline','htmlWidget'),
               script.type=c('embedded','remoteSrc'),
               launchBrowser=TRUE,
               verbose=TRUE)


timePrism(anim,at=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
          displaylabels=F,planes = TRUE,
          label.cex=0.5)


timeline(dynet)

filmstrip(dynet, displaylabels=F,
          slice.par=list(start=0, end=11, interval=4, 
                         aggregate.dur=4, rule='any'))




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

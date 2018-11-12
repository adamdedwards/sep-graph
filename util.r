##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season) & missing(year))      { loc <- "data/fall1998_edge_list.txt"
  } else if(missing(year))   { loc <- paste("data/",season,"1998_edge_list.txt",sep="")
  } else if(missing(season)) { loc <- paste("data/fall",year,"_edge_list.txt",sep="")
  } else                     { loc <- paste("data/",season,year,"_edge_list.txt",sep="")
  }
  print(paste("opening",loc))
  edges <- read.csv(loc, header = TRUE) 
  return(edges)
}


##############################  Create Graphs(datatype:igraph)  ##############################

create.igraph <- function(data) {
  g <- graph_from_edgelist(as.matrix(data), directed = FALSE)               # load edge csv data.frame into an igraph object
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)            # simplify igraph object; remove dups and loops
  return(g)
}

#############################    Create Graphs (datatype:statnet)   ###########################

create.netgraph <- function(data) {
  n=network(data,matrix.type="edgelist",directed=FALSE)
  return(n)
}


############################## Graph Attributes ##############################

# igraph/threejs

hide.isolated.vertices <- function(g) {
  if(class(g)=="igraph"){
    for(v in 1:length(V(g))) {                                                 # hide all isolated vertices
      if(igraph::degree(g)[v]==0) {
        g <- igraph::set_vertex_attr(g, "color", index = v, value=rgb(0,0,0,0.01))
      }
    }
  }
  else if(class(g)=="network"){
    for(v in 1:length(network.vertex.names(g))) {                                                 # hide all isolated vertices
      if(sna::degree(g)[v]==0) {
        g <- network::set.vertex.attribute(g, "color", index = v, value=rgb(0,0,0,0.01))
      }
    }
  }
  else {print("ERROR NOT A GRAPH")}
  return(g)
}

simple.graphics <- function(g) {
  
  if(class(g)=="igraph"){
    g <- set_vertex_attr(g, "color" ,value = rgb(1,0,0,1))
    g <- set_vertex_attr(g, "stroke",value = NULL)
    g <- set_vertex_attr(g, "size"  ,value = 1)
    g <- set_vertex_attr(g, "label" ,value = V(g)$name)                           # add labels
    
    g <- hide.isolated.vertices(g)
    
    g <- set_edge_attr(g, "color",value=rgb(1,0,0,1))
  }
  else if(class(g)=="network"){
    g <- network::set.vertex.attribute(g, "color" ,value = rgb(1,0,0,1))
    g <- network::set.vertex.attribute(g, "stroke",value = NA)
    g <- network::set.vertex.attribute(g, "size"  ,value = 1)
    g <- network::set.vertex.attribute(g, "label" ,value = network.vertex.names(g))                           # add labels
    
    g <- hide.isolated.vertices(g)
    
    g <- network::set.edge.attribute(g, "color",value=rgb(1,0,0,1))
  }
  else {print("ERROR NOT A GRAPH")}
  return(g)
}

graphics <-function(g,color="red",comm=NULL) {	
  if(class(g)=="igraph"){
    g <- set_vertex_attr(g, "color" ,value = color)
    g <- set_vertex_attr(g, "stroke",value = NULL)
    g <- set_vertex_attr(g, "size"  ,value = 1)
    g <- set_vertex_attr(g, "label" ,value = V(g)$name)                           # add labels
    
    if(is.null(comm)==FALSE) {
      g <- set_vertex_attr(g,"group",value = comm$membership)
      c <- rainbow(max(V(g)$group))                                               # add node/edge colors based on group membership
      g <- set_vertex_attr(g, "color",value = c[comm$membership])
      g <- set_edge_attr(g, "color",value = head_of(g, E(g))$color)
    }
    b <- unlist((evcent(g)$vector*10)+1)                                       # add node size based on eigenvector centrality
    g <- set_vertex_attr(g, "size",value = b)
  }
  else if(class(g)=="network"){
    g <- network::set.vertex.attribute(g, "color" ,value = rgb(1,0,0,1))
    g <- network::set.vertex.attribute(g, "stroke",value = NA)
    g <- network::set.vertex.attribute(g, "size"  ,value = 1)
    g <- network::set.vertex.attribute(g, "label" ,value = network.vertex.names(g))                           # add labels
    
    g <- network::set.vertex.attribute(g,"group",as.vector(membership(temp.communities)))
    #c <- rainbow(max(network::get.vertex.attribute(g,"group")))                                               # add node/edge colors based on group membership
    #g <- network::set.vertex.attribute(g, "color",value = c[comm$membership])
    #g <- network::set.edge.attribute(g, "color",value = network::get.edge.attribute(g,"color"))
    
    #b <- unlist((sna::evcent(g)*10)+1)                                       # add node size based on eigenvector centrality
    #g <- network::set.vertex.attribute(g, "size",value = b)
  }
  else {print("ERROR NOT A GRAPH")}
  g <- hide.isolated.vertices(g)
  return(g)
}


############################## Long Season Names ##############################

long.season.names <- function(s) {
  
  lsn <- vector("character")
  
  if("spr" %in% s)  {lsn <- c(lsn,"Spring")}
  if("sum" %in% s)  {lsn <- c(lsn,"Summer")}
  if("fall" %in% s) {lsn <- c(lsn,"Autumn")}
  if("win" %in% s)  {lsn <- c(lsn,"Winter")}
  if(is.na(lsn[1])) {print("No seasons listed.")}
  return(lsn)
  
  
}

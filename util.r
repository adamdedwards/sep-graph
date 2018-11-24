##############################     Load Data    ##############################
load.data <- function(season,year) 
{
  if(missing(season) & missing(year))      { 
    loc <- "data/1998_spr_edge_list.csv"
  } else if(missing(year))   { 
    loc <- paste("data/","1998_",season,"_edge_list.csv",sep="")
  } else if(missing(season)) { 
    loc <- paste("data/",year,"_spr_edge_list.csv",sep="")
  } else                     { 
    loc <- paste("data/",year,"_",season,"_edge_list.csv",sep="")
  }
  
  print(paste("opening",year,season))
  edges <- read.csv(loc, header = TRUE) 
  return(edges)
}


##############################  Create Graphs(datatype:igraph)  ##############################

create.igraph <- function(data) {
  g <- graph_from_data_frame(data, directed = TRUE)
  E(g)$weight <- 1
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  return(g)
}

#############################    Create Graphs (datatype:statnet)   ###########################

create.netgraph <- function(data) {
  n=network(data,matrix.type="edgelist",directed=TRUE)
  return(n)
}


############################## Graph Attributes ##############################

# igraph/threejs

hide.isolated.vertices <- function(g) {
  if(class(g)=="igraph"){
    for(i in 1:length(V(g))-1) {                                                 # hide all isolated vertices
      if(V(g)$degree[i]==0) {
        g <- V(g)$color <- rgb(0,0,0,0.01)
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
    g <- V(g)$color  <- rgb(1,0,0,1)
    g <- V(g)$stroke <- NULL
    g <- V(g)$size   <- 1
    g <- V(g)$label  <- V(g)$name                           # add labels
    
    g <- hide.isolated.vertices(g)
    
    g <- E(g)$color  <- rgb(1,0,0,1)
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

graphics <-function(g,color="red",comm=NULL,undir=FALSE,size=FALSE) {
  
  if(class(g)=="igraph"){
    g <- g
    if(undir) {g <- as.undirected(g)}
    
    V(g)$indegree     <- degree(g, mode = "in")
    V(g)$outdegree    <- degree(g, mode = "out")
    V(g)$degree       <- degree(g, mode = "all")
    
    V(g)$betweenness  <- betweenness(g, V(g), directed=TRUE, normalized=TRUE, weights=NULL)
    V(g)$closeness    <- closeness(g,mode="all", normalized = TRUE)
    V(g)$eigenvector  <- eigen_centrality(g, directed = TRUE, weights=NULL)[[1]]
    
    
    E(g)$arrow.size	  <- 0.1
    E(g)$curved       <- 0.3
    
    V(g)$color        <- color
    V(g)$size         <- 5
    if(size) {V(g)$size         <- 5 + 20*round((V(g)$betweenness/max(V(g)$betweenness)),digits=3)}
    V(g)$label        <- V(g)$name                           # add labels
    V(g)$label.family	<- "mono"
    V(g)$label.cex	  <- 1
    
    if(is.null(comm)==FALSE) {
      V(g)$group <- comm$membership
      c <- colorRampPalette(brewer.pal(11, "Spectral"))(max(V(g)$group))        # add node/edge colors based on group membership

      V(g)$color <- c[V(g)$group]
      E(g)$color <- adjustcolor(c[tail_of(g,E(g))$group],alpha.f = 0.4)
    }
    
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
  #if(hide) {g <- hide.isolated.vertices(g)}
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



#############################  Community Analysis #############################

philosophical.communities <- function(g, comm=NULL) {
  
  cv <- vector("list",length = length(comm))
  
  for(i in 1:length(comm)) {
    subg <- induced.subgraph(g,comm[[i]])
    cv[[i]] <- subg
  }
  return(cv)
}


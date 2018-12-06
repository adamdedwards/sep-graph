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

############################# VisNet Package Visualization ############################

sep.viznet <- function(graphlist,subgroups=FALSE) {
  
  if(subgroups) {
    cg  <- cluster_louvain(as.undirected(graphlist))
    gg <- vector("list",length(cg))
    for(i in 1:length(cg)) {
      gg[[i]] <- induced.subgraph(graphlist,cg[[i]])
    }
    graphlist <- gg
  }
  
  for(i in 1:length(graphlist)) {
    c <- colorRampPalette(brewer.pal(11, "Spectral"))(length(graphlist))
    visnet <- toVisNetworkData(graphics(graphlist[[i]],color=c[i],undir=TRUE))
    
    visnet$nodes$font <- "14px monospace black"
    visnet$nodes$title <- paste("<h4><a href=\"https://plato.stanford.edu/archives/spr",years[i],"/entries/",visnet$nodes$label,"/\">SEP/",visnet$nodes$label,"</a></h4>",
                                "<p><b>Degree: </b>",visnet$nodes$degree,"&emsp;<b>Max: </b>",max(visnet$nodes$degree),"</p>",
                                "<p><b>In-Degree: </b>",visnet$nodes$indegree,"&emsp;<b>Max: </b>",max(visnet$nodes$indegree),"</p>",
                                "<p><b>Out-Degree: </b>",visnet$nodes$outdegree,"&emsp;<b>Max: </b>",max(visnet$nodes$outdegree),"</p>",
                                "<p><b>Betweenness: </b>",round((visnet$nodes$betweenness/max(visnet$nodes$betweenness)),digits=3),"</p>",
                                "<p><b>Eigenvector: </b>",round((visnet$nodes$eigenvector/max(visnet$nodes$eigenvector)),digits=3),"</p>",
                                "<p><b>Group: </b>",visnet$nodes$group,"</p>",sep="")
    
    
    # Run stats for centrality measures
    d.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$degree)]),10),sep="",collapse=", ")   # list of nodes in order of degree/betweenness/etc centrality
    b.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$betweenness)]),10),sep="",collapse=", ")
    e.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$eigenvector)]),10),sep="",collapse=", ")
    
    cat("GRAPH FOR Group",i,"\n========================\nDegree centrality:", d.top10,"\nBetweenness centrality:", b.top10,"\nEigenvector centrality:", e.top10,"\n\n\n")
    
    
    # Produce the visualization
    visNetwork(nodes      = visnet$nodes, 
               edges      = visnet$edges, 
               main       = list(text=paste("<h1>The SEP in the year ",years[i],"</h1>",sep=""),style="font-family: \"Inconsolata\", monospace;"),
               height     = "700px", 
               width      = "100%",
               background = "rgba(0, 0, 0, 0)",
               footer     = list(text=paste("<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Degree_(graph_theory)\">degree centrality</a>:</h3><p>",d.top10,"</p>",
                                            "<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Betweenness_centrality\">betweenness centrality</a>:</h3><p>",b.top10,"</p>",
                                            "<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Eigenvector_centrality\">eigenvector centrality</a>:</h3><p>",e.top10,"</p>",sep=""),style="font-family: \"Inconsolata\", monospace;")) %>%
      visEdges(smooth = FALSE,arrows = list(to = list(enabled = TRUE, scaleFactor = .5))) %>%
      visIgraphLayout(layout = "layout_with_kk", physics = TRUE, randomSeed = 8128) %>%
      visPhysics(solver = "forceAtlas2Based", #barnesHut forceAtlas2Based
                 forceAtlas2Based = list(gravitationalConstant = -80, centralGravity=.01, avoidOverlap=1, springConstant=0.1, damping=1),
                 minVelocity = 1,
                 stabilization = FALSE) %>%
      visOptions(selectedBy       = list(variable="group",style = 'font-family: \"Inconsolata\", monospace; width: 200px; height: 32px; background: #f2f2f2; color:black; border:none; border-radius:12px; outline:none;'), 
                 highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE, hideColor = "rgba(0,0,0,.1)",labelOnly = FALSE), 
                 nodesIdSelection = list(enabled = TRUE, style = 'font-family: \"Inconsolata\", monospace; width: 200px; height: 32px; background: #f2f2f2; color:black; border:none; border-radius:12px; outline:none;')) %>%
      visInteraction(keyboard = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay=200, tooltipStyle='font-family: \"Inconsolata\", monospace; font-size:16px; background: #f2f2f2; color:black; padding:2px 12px; border:none; border-radius:12px; outline:none; position:fixed; visibility:hidden;') %>%
      visSave(file = paste("group_",i,".html",sep=""))
  }
}

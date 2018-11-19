#library(ndtv)
library(network)
library(networkD3)
library(igraph)
#library(threejs)
#library(intergraph)
#library(scatterplot3d)
#library(tsna,ergm)
library(RColorBrewer)

#library("data.table")
#library(dplyr)
library(visNetwork)

if(getwd() != "C:/Users/adame/Dropbox/git/sep-graph") {setwd("C:/Users/adame/Dropbox/git/sep-graph")}
source("util.r")
##############################         CREATE IGRAPH & STATNET OBJECTS      ##############################

years <- as.character(1998:2008)
seasons <- c("spr") # "fall","sum","win"
iterations <- length(years)*length(seasons)
sep.igraphs <- vector("list", iterations) 
sep.netgraphs <- vector("list", iterations) 

for(i in 0:(iterations-1)) {
       edges <- load.data(season=seasons[(i%%length(seasons))+1],year=years[(i%/%length(seasons))+1])
  data.edges <- as.data.frame(edges)
  sep.igraphs[[i+1]]   <- create.igraph(data.edges)
  sep.netgraphs[[i+1]] <- create.netgraph(data.edges)
}


################################### CONSTRUCT GRAPH LIST FOR GRAPHJS ###################################

big.intersect.graph  <- sep.igraphs[[1]]
big.union.graph      <- graph.empty(directed=FALSE)
full.node.set.igraphs <- vector("list", iterations)

for(i in 1:length(sep.igraphs)) {
  big.intersect.graph <- intersection(big.intersect.graph,sep.igraphs[[i]])
}

for(i in 1:iterations) {
  full.node.set.igraphs[[i]] <- union(big.intersect.graph,sep.igraphs[[i]])
}

#top.level.communities <- cluster_fast_greedy(full.node.set.graphs[[iterations]])
top.level.communities <- cluster_walktrap(full.node.set.igraphs[[iterations]])

for(i in 1:iterations) {
  full.node.set.igraphs[[i]] <- graphics(full.node.set.igraphs[[i]],top.level.communities)                                         # edits graphics to color and hide nodes
}

write.table(betweenness(full.node.set.igraphs[[1]]),"btw2018.csv",sep=",")

# Static Graph Images
for(i in 1:1) { #length(full.node.set.igraphs)
  g <- full.node.set.igraphs[[i]]
  l <- layout_with_kk(g,dim=3)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  gjs <- graphjs(g, 
                 layout=l*0.2,
                 vertex.label=V(g)$label,
                 minx = NULL, maxx = 300, 
                 miny = NULL, maxy = 300, 
                 minz = NULL, maxz = 300)
  saveNetwork(gjs,paste("/pages/graphs/Fall_",years[i],"_Graph.html",sep=""), selfcontained = TRUE)
}

# Dynamic Graph Animation
graphjs(full.node.set.igraphs,
        main=rep(years,each=1),
        bg="white",
        fpl=100)

#write.table(igraph::betweenness(full.node.set.igraphs[[1]]),file="btw.csv",sep = ",")

###################################DYNAMIC NETWORK ANIMATIONS WITH NDTV###################################


for(i in 1:iterations) {                                                                                       # simplify and create graph series w/ graphics
  temp.communities <- cluster_fast_greedy(simplify(asIgraph(sep.netgraphs[[i]])))
  sep.netgraphs[[i]] <- graphics(sep.netgraphs[[i]],temp.communities)
}

dynet <- networkDynamic(network.list = sep.netgraphs, vertex.pid="vertex.names")                               # create the dynamic network

final.slice.communities <- cluster_fast_greedy(simplify(asIgraph(sep.netgraphs[[iterations]])))                # this adds an attribute to force all groups to be based on the last slice
network::set.vertex.attribute(dynet,"group.static",as.vector(membership(final.slice.communities)))

for(i in 1:iterations) {                                                                                       # this loop assigns group as a dynamic attribute
  current.groups <- network::get.vertex.attribute(sep.netgraphs[[i]],"group")
  res <- rep(0, length(as.vector(membership(final.slice.communities))))
  where <- match(sep.netgraphs[[i]]%v%"vertex.names", dynet%v%"vertex.names")
  res[where] <- current.groups
  res <- ifelse(res==0,NA,res)
  activate.vertex.attribute(dynet,"group",value=res,onset=i-1,terminus=i)
}

fixed.group.colors <- rainbow(max(get.vertex.attribute.active(dynet,"group",at=11)))                           # assigns global colors for groups



########################### ANIMATION CONTROLS ##################################

d3.options <- list(animationDuration=800,
                   enterExitAnimationFactor=0, 
                   nodeSizeFactor=0.01, 
                   playControls=TRUE, 
                   animateOnLoad=TRUE, 
                   slider=TRUE)

render.par <- list(tween.frames=10,
                   show.time=FALSE,
                   show.stats="~edges",
                   extraPlotCmds=NULL,
                   initial.coords=0)

anim <- compute.animation(dynet,animation.mode='kamadakawai')

anim%n%'slice.par'<-list(start=0,
                         end=19,
                         interval=1, 
                         aggregate.dur=1,
                         rule='latest')

#saveVideo and  render.cache='none'
render.d3movie(anim, 
               main = function(s) {paste("Season:",long.season.names(seasons)[((s-1)%%length(seasons))+1],"Year:",years[((s-1)%/%length(seasons))+1],sep=" ")},
               vertex.tooltip = function(slice){ network::network.vertex.names(slice) },
               displaylabels  = F,
               edge.col       = '#bbbbbb',
               vertex.cex     = function(slice){ (sna::betweenness(slice,rescale=TRUE)*10)+0.4  },
               #vertex.col     = function(slice){ ifelse(!is.na(slice%v%"group"),fixed.group.colors[slice%v%"group"],'white') },
               vertex.col     = function(slice){  rainbow(max(dynet%v%"group.static"))[slice%v%"group.static"]},
               vertex.lwd     = 0,
               filename       = tempfile(fileext = '.html'), 
               render.par,
               plot.par       = list(bg='white'),
               d3.options, 
               output.mode    ='HTML',
               script.type    = 'embedded',
               launchBrowser  = TRUE,
               verbose        = TRUE)






timePrism(anim,at=c(0,5,10,15),
          displaylabels=F,planes = TRUE,
          label.cex=0.5)


timeline(dynet)
tErgmStats(dynet,"edges")
tErgmStats(dynet,"meandeg")

plot( tEdgeFormation(dynet) )


tSnaStats(dynet,
          'connectedness',
          start=0,
          end = 12)

filmstrip(dynet, displaylabels=F,
          slice.par=list(start=0, end=11, interval=4, aggregate.dur=4, rule='any'))




##############################    networkd3 Plots     ############################## 

# networkd3
ig <- sep.igraphs[[1]]

V(ig)$label <- V(ig)$name
V(ig)$degree <- degree(ig)
V(ig)$betweenness <- betweenness(ig,V(ig),directed=TRUE)


i  <- cluster_walktrap(ig)
c1 <- induced.subgraph(ig,i[[5]])

class(gg)

gg <- igraph_to_networkD3(c1,membership(cluster_walktrap(c1)))
gg$nodes$deg <- as.character(degree(c1, v = V(c1)))
gg$nodes$btw <- as.character(betweenness(c1, v = V(c1), directed = TRUE))
gg$nodes$clo <-as.character(closeness(c1, v = V(c1),mode="all"))
gg$nodes$eig <-as.character(eigen_centrality(c1, directed = TRUE)[[1]])
gg$nodes$size <- as.character("1")
gg$nodes$id <- row.names(gg$nodes)

rename("from"=source, "to"=target)


gg$nodes <- setorder(gg$nodes,"name")



nd3 <- forceNetwork(Links       = gg$links,
                    Nodes       = gg$nodes,
                    Source      = "source",
                    Target      = "target",
                    Group       = "group",
                    NodeID      = "name", 
                    Nodesize    = "size",
                    radiusCalculation = JS("0"),
                    charge      = -200,
                    arrows      = FALSE,
                    legend      = TRUE,
                    bounded     = FALSE,
                    opacity     = 1,
                    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                    linkColour = as.character("#00000011"),
                    fontFamily  = "monospace",
                    fontSize    = 14,
                    opacityNoHover = 1,
                    zoom = TRUE)                                                               # Visualize graph with networkD3

saveNetwork(nd3,"test.html", selfcontained = TRUE)
saveNetwork(visIgraph(c1),"test2.html",selfcontained=TRUE)
saveNetwork(visNetwork(nodes=gg$nodes,edges=gg$links),"test3.html",selfcontained=TRUE)



visIgraph(c1)

#################################### REVIEW FALL 2018 ###################################

fall2018 <- sep.igraphs[[1]]

philosophical.communities <- function(g) {
  
  pc <- cluster_walktrap(g)
  cv <- vector("list",length = length(pc))
  
  for(i in 1:length(pc)) {
    subg <- induced.subgraph(g,pc[[i]])
    cv[[i]] <- subg
  }
  return(cv)
}

communities_for_analysis <- philosophical.communities(fall2018)

for (i in 1:length(communities_for_analysis)) {
  
  g <- graphics(communities_for_analysis[[i]],color=rainbow(length(communities_for_analysis))[i])
  
  l <- layout_with_kk(g,dim=3)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  gjs <- graphjs(g, 
                 layout=l*0.2,
                 vertex.label=V(g)$label,
                 minx = NULL, maxx = 300, 
                 miny = NULL, maxy = 300, 
                 minz = NULL, maxz = 300)
  
  saveNetwork(gjs,paste("community_",i,".html",sep=""), selfcontained = TRUE)
  print(paste("Status: Generating visualization for community ",i,sep=""))
}


####################### vizNetwork ###################

sep.viznet <- function() {
  for(i in 1:length(sep.igraphs)) {
    g <- sep.igraphs[[i]]
    g <- as.undirected(g)
    
    V(g)$label        <- V(g)$name
    V(g)$degree       <- degree(g, mode = "all")
    V(g)$betweenness  <- betweenness(g,V(g),directed=FALSE,normalized=TRUE)
  #  V(g)$closeness    <- closeness(g,mode="all",normalized = TRUE)
    V(g)$eigenvector <- eigen_centrality(g, directed = FALSE, weights=E(g)$weight)[[1]]
    V(g)$group        <- membership(cluster_walktrap(g))
    
    colors <- colorRampPalette(brewer.pal(11, "Spectral"))(max(V(g)$group))
    
    V(g)$color        <- colors[V(g)$group]
    V(g)$size         <- 5 + 20*round((V(g)$betweenness/max(V(g)$betweenness)),digits=3)
    V(g)$label.family	<- "mono"
    V(g)$label.cex	  <- 1
    
    E(g)$color        <- adjustcolor(colors[tail_of(g,E(g))$group],alpha.f = 0.4)
    E(g)$arrow.size	  <- 0.1
    E(g)$curved       <- 0.3
    
    visnet <- toVisNetworkData(g)
    
    visnet$nodes$font <- "14px monospace black"
    visnet$nodes$title <- paste("<h4><a href=\"https://plato.stanford.edu/archives/spr",years[i],"/entries/",visnet$nodes$label,"/\">SEP/",visnet$nodes$label,"</a></h4>",
                                "<p><b>Degree Centrality: </b>",visnet$nodes$degree,"&emsp;<b>Max: </b>",max(visnet$nodes$degree),"</p>",
                                "<p><b>Betweenness Centrality: </b>",round((visnet$nodes$betweenness/max(visnet$nodes$betweenness)),digits=3),"</p>",
                                "<p><b>Eigenvector Centrality: </b>",round((visnet$nodes$eigenvector/max(visnet$nodes$eigenvector)),digits=3),"</p>",
                                "<p><b>Group: </b>",visnet$nodes$group,"</p>",sep="")
    
    
    d.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$degree)]),10),sep="",collapse=", ") # list of nodes in order of degree/betweenness/etc centrality
    b.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$betweenness)]),10),sep="",collapse=", ")
    e.top10 <- paste(head(rev(visnet$nodes$label[order(visnet$nodes$eigenvector)]),10),sep="",collapse=", ")
    
    visNetwork(nodes = visnet$nodes, 
               edges      = visnet$edges, 
               main       = list(text=paste("<h1>The SEP in the year ",years[i],"</h1>",sep=""),style="font-family: \"Inconsolata\", monospace;"),
               height     = "700px", 
               width      = "80%",
               background = "rgba(0, 0, 0, 0)",
               footer     = list(text=paste("<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Degree_(graph_theory)\">degree centrality</a>:</h3><p>",d.top10,"</p>",
                                            "<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Betweenness_centrality\">betweenness centrality</a>:</h3><p>",b.top10,"</p>",
                                            "<h3>Top nodes by <a href=\"https://en.wikipedia.org/wiki/Eigenvector_centrality\">eigenvector centrality</a>:</h3><p>",e.top10,"</p>",sep=""),style="font-family: \"Inconsolata\", monospace;")) %>%
    visEdges(smooth = FALSE) %>%
    visIgraphLayout(layout = "layout_with_kk",physics = TRUE,randomSeed = 8128) %>%
  # visLayout(randomSeed = 12, 
  #           improvedLayout = TRUE,
  #           hierarchical = FALSE) %>%
  # visClusteringByGroup(c(1:length(visnet$nodes$group))) %>%
    visPhysics(solver = "forceAtlas2Based", #barnesHut forceAtlas2Based
               forceAtlas2Based = list(gravitationalConstant = -80,centralGravity=.01,avoidOverlap=1,springConstant=0.1,damping=1),
               minVelocity = 1,
               stabilization=FALSE) %>%
    visOptions(selectedBy       = list(variable="group",style = 'font-family: \"Inconsolata\", monospace; width: 200px; height: 32px; background: #f2f2f2; color:black; border:none; border-radius:12px; outline:none;'), 
               highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE, hideColor = "rgba(0,0,0,.1)",labelOnly = FALSE), 
               nodesIdSelection = list(enabled = TRUE, style = 'font-family: \"Inconsolata\", monospace; width: 200px; height: 32px; background: #f2f2f2; color:black; border:none; border-radius:12px; outline:none;')) %>%
   visInteraction(keyboard = TRUE, hideEdgesOnDrag = TRUE, tooltipDelay=200, tooltipStyle='font-family: \"Inconsolata\", monospace; font-size:16px; background: #f2f2f2; color:black; padding:2px 12px; border:none; border-radius:12px; outline:none; position:fixed; visibility:hidden;') %>%
   visSave(file = paste("viz/spring_",years[i],".html",sep=""))
  }
}

sep.viznet()

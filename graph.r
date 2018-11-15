# install.packages("data.table")
# install.packages("dplyr")
 install.packages("visNetwork")

library(ndtv)
library(networkD3)
library(igraph)
library(magrittr)
library(threejs)
library(htmlwidgets)
library(intergraph)
library(scatterplot3d)
library(tsna,ergm)
library(RColorBrewer)

library("data.table")
library(dplyr)
library(visNetwork)

source("util.r")
##############################         CREATE IGRAPH & STATNET OBJECTS      ##############################

years <- 2018:2018
seasons <- c("fall") # ,"sum","spr","win"
iterations <- length(years)*length(seasons)
sep.igraphs <- vector("list", iterations) 
sep.netgraphs <- vector("list", iterations) 



for(i in 0:(iterations-1)) {
  data <- load.data(season=seasons[(i%%length(seasons))+1],year=years[(i%/%length(seasons))+1])
  x <- create.igraph(data)
  sep.igraphs[[i+1]] <- x
}

for(i in 0:(iterations-1)) {
  data <- load.data(season=seasons[(i%%length(seasons))+1],year=years[(i%/%length(seasons))+1])
  x <- create.netgraph(data)
  sep.netgraphs[[i+1]] <- x
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
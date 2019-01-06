
library(network)
library(igraph)
library(RColorBrewer)
library(data.table)

if(getwd() != "C:/Users/adame/Dropbox/git/sep-graph") {setwd("C:/Users/adame/Dropbox/git/sep-graph")}
source("util.r")

##############################         CREATE IGRAPH & NETWORK OBJECTS      ##############################

years <- as.character(2018)
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


################################### VISUALIZATIONS WITH GRAPHJS ###################################

install.packages("threejs")
library(threejs)


big.intersect.graph  <- sep.igraphs[[1]]
big.union.graph      <- graph.empty(directed=FALSE)
full.node.set.igraphs <- vector("list", iterations)

for(i in 1:iterations) {
  big.intersect.graph <- intersection(big.intersect.graph,sep.igraphs[[i]])
}

for(i in 1:iterations) {
  full.node.set.igraphs[[i]] <- union(big.intersect.graph,sep.igraphs[[i]])
}

#top.level.communities <- cluster_fast_greedy(full.node.set.graphs[[iterations]])
top.level.communities <- cluster_walktrap(full.node.set.igraphs[[iterations]])

for(i in 1:iterations) {
  full.node.set.igraphs[[i]] <- graphics(full.node.set.igraphs[[i]],
                                         comm=top.level.communities,
                                         size=FALSE,
                                         hide=FALSE,
                                         undir=FALSE)     # edits graphics to color and hide nodes
}


# Static Graph Images
for(i in 1:iterations) {
  g <- full.node.set.igraphs[[i]]
  l <- layout_with_kk(g,dim=3)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

  gjs <- graphjs(g,
                 layout=l*0.2,
                 vertex.label=V(g)$label,
                 minx = NULL, maxx = 300,
                 miny = NULL, maxy = 300,
                 minz = NULL, maxz = 300)
  saveNetwork(gjs,paste("Fall_",years[i],"_Graph.html",sep=""), selfcontained = TRUE)
}

# Dynamic Graph Animation
dyn.js <- graphjs(full.node.set.igraphs,
                  main = rep(years,each=1),
                  bg="white",
                  fpl=100)
saveNetwork(dyn.js,paste("Dynamic_Graph.html",sep=""), selfcontained = TRUE)


###################################  DYNAMIC NETWORK ANIMATIONS WITH NDTV  ###################################

install.packages("ndtv")
library(ndtv)


for(i in 1:iterations) {                                                                                       # simplify and create graph series w/ graphics
  temp.communities <- cluster_fast_greedy(asIgraph(sep.netgraphs[[i]]))
  sep.netgraphs[[i]] <- graphics(sep.netgraphs[[i]],temp.communities)
}

dynet <- networkDynamic(network.list = sep.netgraphs, vertex.pid="vertex.names")                               # create the dynamic network

final.slice.communities <- cluster_fast_greedy(asIgraph(sep.netgraphs[[iterations]]))                          # this adds an attribute to force all groups to be based on the last slice
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



########################### NDTV ANIMATION CONTROLS ##################################

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




##############################    NETWORKD3 PLOTS     ##############################

install.packages("networkD3")
library(networkD3)

# networkd3
ig <- sep.igraphs[[1]]
i  <- cluster_walktrap(ig)

ig <-graphics(ig,comm=i)

c1 <- induced.subgraph(ig,i[[5]])

gg <- igraph_to_networkD3(c1,membership(cluster_walktrap(c1)))

gg$nodes$deg  <- as.character(degree(c1, v = V(c1)))                               # TODO: replace with graphics function
gg$nodes$btw  <- as.character(betweenness(c1, v = V(c1), directed = TRUE))
gg$nodes$clo  <- as.character(closeness(c1, v = V(c1),mode="all"))
gg$nodes$eig  <- as.character(eigen_centrality(c1, directed = TRUE)[[1]])
gg$nodes$size <- as.character("10")
gg$nodes$id   <- row.names(gg$nodes)
gg$nodes <- setorder(gg$nodes,"name")

rename("from"=source, "to"=target)

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


#################################### EVALUATE FALL 2018 ###################################

fall2018 <- sep.igraphs[[1]]                             # TODO: replace with functions to do general community-level analysis

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


####################### VISNETWORK ###################

install.packages("visNetwork")
library(visNetwork)

sep.2018 <- sep.igraphs[[1]]

communities_for_analysis <- philosophical.communities(sep.2018,cluster_louvain(as.undirected(sep.2018)))

sep.viznet(communities_for_analysis)


###############################  MISC ANALYSIS  #################################

sep.analysis <- graphics2(sep.igraphs)

sep.frame <- as_data_frame(sep.analysis[[1]],what="both")

dd <- degree.distribution(sep.analysis[[1]],cumulative=F)*length(V(sep.analysis[[1]]))

ggplot(sep.frame$vertices) + geom_histogram(aes(x=alldegree), binwidth = 1,boundary=0.5) +
  theme_light() +
  scale_x_continuous("degree", breaks=c(2,4,6,8,10,12,14,16,18), labels=c(2,4,6,8,10,12,14,16,18))

ggplot(sep.frame$vertices, aes(x=betweenness),fill = "white", color = "black") + geom_freqpoly()
ggplot(sep.frame$vertices, aes(x=eigenvector),fill = "white", color = "black") + stat_count(width = 0.5)

ggplot(sep.frame$vertices, aes(x=betweenness, y=eigenvector)) + geom_point(shape=1)

sep.frame


sep.frame.1998 <- as_data_frame(sep.analysis[[1]],what="both")[[1]]
sep.frame.1999 <- as_data_frame(sep.analysis[[2]],what="both")[[1]]
sep.frame.2000 <- as_data_frame(sep.analysis[[3]],what="both")[[1]]
sep.frame.2001 <- as_data_frame(sep.analysis[[4]],what="both")[[1]]

hegel <- subset(sep.frame$vertices,name=="hegel")
sep.frame$vertices[sep.frame$vertices$name=="hegel",]


sep.frame.1998.sorted.deg <- sep.frame.1998[order(sep.frame.1998$alldegree),]
sep.frame.1999.sorted.deg <- sep.frame.1999[order(sep.frame.1999$alldegree),]
sep.frame.2000.sorted.deg <- sep.frame.2000[order(sep.frame.2000$alldegree),]
sep.frame.2001.sorted.deg <- sep.frame.2001[order(sep.frame.2001$alldegree),]


sep.10.deg.1998 <- data.table(tail(sep.frame.1998.sorted.deg,10),key="name")
sep.10.deg.1999 <- data.table(tail(sep.frame.1999.sorted.deg,10),key="name")
sep.10.deg.2000 <- data.table(tail(sep.frame.2000.sorted.deg,10),key="name")
sep.10.deg.2001 <- data.table(tail(sep.frame.2001.sorted.deg,10),key="name")

all.names <- unique(c(sep.10.deg.1998$name, sep.10.deg.1999$name, sep.10.deg.2000$name, sep.10.deg.2001$name))


c <- brewer.pal(name="Blues",n=8)

ggplot() +
  geom_point(data=sep.10.deg.2001, aes(x = rank((11-sep.10.deg.2001$alldegree),ties.method = "min"), y = sep.10.deg.2001$name),color=c[6],size=3,position=position_jitter(width=0,height=.1)) +
  geom_point(data=sep.10.deg.2000, aes(x = rank((11-sep.10.deg.2000$alldegree),ties.method = "min"), y = sep.10.deg.2000$name),color=c[5],size=3,position=position_jitter(width=0,height=.1)) +
  geom_point(data=sep.10.deg.1999, aes(x = rank((11-sep.10.deg.1999$alldegree),ties.method = "min"), y = sep.10.deg.1999$name),color=c[4],size=3,position=position_jitter(width=0,height=.1)) +
  geom_point(data=sep.10.deg.1998, aes(x = rank((11-sep.10.deg.1998$alldegree),ties.method = "min"), y = sep.10.deg.1998$name),color=c[3],size=3,position=position_jitter(width=0,height=.1)) +
  scale_x_continuous("rank",breaks=c(1:10), labels=c(1:10),limits = c(1,10)) +
  scale_y_discrete(limits=all.names)

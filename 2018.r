library(igraph)
library(RColorBrewer)

setwd("C:/Users/Adam/Dropbox/git/sep-graph")

nodes <- read.csv("data/2018_fall_node_list_bi.csv", header = TRUE)
edges <- read.csv("data/2018_fall_edge_list_bi.txt", header = TRUE)
as.data.frame(nodes)

g <- graph_from_data_frame(edges, directed = TRUE,vertices=nodes)
E(g)$weight <- 1
g <- simplify(g, edge.attr.comb=list(weight="sum"))

V(g)$color <- c("blue", "red")[V(g)$type+1]

plot(g, vertex.label=NA,vertex.size=5,layout=layout.kamada.kawai) 

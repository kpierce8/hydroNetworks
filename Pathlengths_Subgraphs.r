##Previously Create Graph Fle##
library(igraph)


  
bob.clusters <- clusters(bob8)

temp1 <- bob.clusters$membership[-1]
temp2 <- data.frame(temp1,bob.clusters$csize[temp1+1])
temp3 <- data.frame(uni.nodes,temp2)

bob8 <- set.vertex.attribute(bob8,"membership",V(bob8),bob.clusters$membership)
bob8 <- set.vertex.attribute(bob8,"xcoor",V(bob8)[-1],temp3[,47])
bob8 <- set.vertex.attribute(bob8,"ycoor",V(bob8)[-1],temp3[,48])

grtest<- subgraph(bob8,V(bob8)[V(bob8)$membership==2])

##plot test
#gBlocks <- cohesive.blocks(grtest)
#plot(gBlocks,layout=layout.kamada.kawai,vertex.size=7)

# 0 value for betweenness yields location of ends
betweenness(grtest)
origin <- topological.sort(grtest)[length(topological.sort(grtest))]

be.grtest2<-betweenness(grtest)
gb2<- which(be.grtest2==0)
spaths2<-get.shortest.paths(grtest,origin,gb2-1)
#get lengths from path 1
path1 <-get.edge.attribute(grtest,'length',E(grtest))[spaths2[[1]]]

bob2<-pathlengths(spaths2)
psums2<- pathsums(bob2)
#find which path is longest
mpath2<-which.max(psums2)
#create vector of path vertices

##NEW FROM GRAPH INVESTIGATION
delpath2 <- spaths2[mpath2]
test2 <- delete.vertices(grtest,delpath2[[1]])


nowgraph <- test2
nclus<- clusters(nowgraph)
wclus <- which.max(nclus$csize)
bb1<-table(nclus$membership)
bb2<-which.max(table(nclus$membership))
maxc<- as.numeric(names(bb1)[bb2])

gr3<- subgraph(nowgraph,V(nowgraph)[nclus$membership==maxc])

origin <- topological.sort(gr3)[length(topological.sort(gr3))]
be.grtest2<-betweenness(gr3)
gb2<- which(be.grtest2==0)
spaths2<-get.shortest.paths(gr3,origin,gb2-1)
bob2<-pathlengths(spaths2)
psums2<- pathsums(bob2)

#gBlocks <- cohesive.blocks(gr3)
#plot(gBlocks,layout=layout.kamada.kawai,vertex.size=7)

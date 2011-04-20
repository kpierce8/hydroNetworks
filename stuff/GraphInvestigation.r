es<- E(bob7)
get.edges(bob7,es)[1]

bob8 <- bob7
bob.clusters <- clusters(bob8)
#test change

bob8 <- set.vertex.attribute(bob8,"membership",V(bob8),bob.clusters$membership)
bob8 <- set.edge.attribute(bob8,"length",E(bob8),lines.dbf4[,35])
bob8 <- set.vertex.attribute(bob8,"xcoor",V(bob8),temp3[,47])
bob8 <- set.vertex.attribute(bob8,"ycoor",V(bob8),temp3[,48])


gr1<- subgraph(bob8,V(bob8)[V(bob8)$membership==2])
plot(gr1)

summary(gr1)

plot(gr1,layout=cbind(V(gr1)$xcoor,V(gr1)$ycoor),vertex.size=7)
get.edge.attribute(gr1,"ComID")

test=cbind(V(gr1)$xcoor,V(gr1)$ycoor)

test


topological.sort(gr1)

gBlocks <- cohesive.blocks(gr1)

plot(gBlocks,layout=layout.kamada.kawai,vertex.size=7)

# 0 value for betweenness yields location of ends
betweenness(gr1)
origin <- topological.sort(gr1)[length(topological.sort(gr1))]

be.gr1<-betweenness(gr1)
gb<- which(be.gr1==0)
spaths<-get.shortest.paths(gr1,origin,gb-1)
#get lengths from path 1
path1 <-get.edge.attribute(gr1,'length',E(gr1))[spaths[[1]]]

pathlengths<- function(spaths,gr1){
     lpaths <- vector("list",length(spaths))
  for(i in 1:length(spaths)){
     lpaths[[i]] <-get.edge.attribute(gr1,'length',E(gr1))[spaths[[i]]]
     }
     return(lpaths)
  }
bob<-pathlengths(spaths,gr1)


pathsums <- function(bob){
  psums <- vector("numeric",length(bob))
  for (i in 1:length(bob)){
    psums[i]<-sum(bob[[i]]-bob[[i]][1])
 }
 return(psums)
}
psums<- pathsums(bob)

#find which path is longest
mpath<-which.max(psums)
#create vector of path vertices
delpath <- spaths[mpath]


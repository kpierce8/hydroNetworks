##Moslty functions for graph subsetting and breaking
#grtest<- subgraph(bob8,V(bob8)[V(bob8)$membership==3])


# 0 value for betweenness yields location of ends
#FUNCTION 1
pathlengths<- function(spaths,grtest=grtest){
     lpaths <- vector("list",length(spaths))
  for(i in 1:length(spaths)){
     lpaths[[i]] <-get.edge.attribute(grtest,'length',E(grtest))[spaths[[i]]]
     }
     return(lpaths)
  }
#FUNCTION 2
pathsums <- function(bob2){
  psums <- vector("numeric",length(bob2))
  for (i in 1:length(bob2)){
    psums[i]<-sum(bob2[[i]])-bob2[[i]][1]
 }
 return(psums)
}
#FUNCTION 3
rem.longest<- function(grtest){
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
  delpath2 <- spaths2[mpath2]
  ##NEW FROM GRAPH INVESTIGATION
  test2 <- delete.vertices(grtest,delpath2[[1]])
  return(test2)
}
#FUNCTION 4
get.biggest<- function(nowgraph){
  nclus<- clusters(nowgraph)
  wclus <- which.max(nclus$csize)
  bb1<-table(nclus$membership)
  bb2<-which.max(table(nclus$membership))
  maxc<- as.numeric(names(bb1)[bb2])
  gr3<- subgraph(nowgraph,V(nowgraph)[nclus$membership==maxc])
}
#FUNCTION 5
graph.plot<-function(newg4,hbig=7){
gBlocks <- cohesive.blocks(newg4)
plot(gBlocks,layout=layout.kamada.kawai,vertex.size=hbig)
}


#
#origin <- topological.sort(gr3)[length(topological.sort(gr3))]
#be.grtest2<-betweenness(gr3)
#gb2<- which(be.grtest2==0)
#spaths2<-get.shortest.paths(gr3,origin,gb2-1)
#bob2<-pathlengths(spaths2)
#psums2<- pathsums(bob2)
#
get.objid <- function(gr2,verts){
    numvert<- summary(gr2)[1]
    objids<-V(gr2)$UNINDEX
    objid<- match(verts,objids)
    return(objid)
    }
#
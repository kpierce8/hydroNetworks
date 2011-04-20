library(igraph)
library(foreign)
source( "E:\\CODE_local\\HydroNetworks_local\\Trunk\\NetFunctions.r")

wria <- c("WA")
LINEID <- c("OBJECTID") #wchydro is LLID
LENGTH <- c("Shape_Length") #wchydro is LLID
library(RODBC)
cnn1 <- odbcConnectAccess("c:/data/projectslocal/SegmentsHARN0809.mdb")
edges.dbf <- sqlFetch(cnn1,"edges")

edges.dbf <- edges.dbf[,-c(match("Shape",names(edges.dbf)))]


nodes.dbf<- sqlFetch(cnn1,"junctions")

nodes.dbf<- nodes.dbf[order(nodes.dbf$OBJECTID),]
idcol<- match(LINEID,names(edges.dbf))
lencol<- match(LENGTH,names(edges.dbf))
#nodes.dbf <-read.dbf(paste("C:\\temp\\wchydro",wria,"_FeatureVerticesToP.dbf",sep=""))
#edges.dbf <-read.dbf(paste("C:\\data\\WCCheck\\wchydro",wria,".dbf",sep=""))

############################################
#Duplicate and node finding
############################################

#create dataset of 4 position columns
ends <- c("FromX","FromY","ToX","ToY")
get.ends <- match(ends,names(edges.dbf))
dataset <- signif(edges.dbf[,get.ends],digits=8)

#create nodeset of 2 point locations
nodes <- c("XLoc","YLoc")
get.nodes <- match(nodes,names(nodes.dbf))
nodesset<- signif(nodes.dbf[,get.nodes],digits=8)

##Check for dupes##################################
#find polyedges with dulicate start and end positions (could be dupes or loops)
  bob<-apply(dataset,1,paste,collapse="")
  bob.dupes<-bob[duplicated(bob)]

  dupes.index <- bob %in% bob.dupes
  length(bob[dupes.index])
  edges.dupes <- edges.dbf[dupes.index,]
  length(edges.dbf[duplicated(edges.dbf[,idcol]),1])

  #look for reachcode&length dupes in those discovered above
  # or some combination of ID and length

  bob2 <- apply(edges.dupes[,c(idcol,lencol)],1,paste,collapse="")
  reach.dupes<-bob2[duplicated(bob2)]
  reach.index <- bob2 %in% reach.dupes
  length(bob2[reach.index])

  edges.dupes <- data.frame(edges.dupes,DupeLength = reach.index)

  #UNCOMMENT TO write files
  #write.dbf(edges.dupes, "G:\\edges\\edges.dupes.dbf")
  #table.norow(edges.dupes, "G:\\edges\\edges.dupes.txt")
######################################################


##CREATE Ends and Node IDs with position information
from.nodes <-  apply(signif(dataset[,1:2],digits=8),1,paste,collapse="")
to.nodes <-  apply(signif(dataset[,3:4],digits=8),1,paste,collapse="")
nodesB <-  apply(signif(nodesset[,1:2],digits=8),1,paste,collapse="")

uni.nodes<- nodes.dbf[duplicated(nodesB)=='FALSE',]
uni.nodes<- data.frame(UNINDEX = seq(1:nrow(uni.nodes)),uni.nodes)
nodes <-  nodesB[duplicated(nodesB)=='FALSE']

mfrom <- match(from.nodes, nodes)
mto <- match(to.nodes, nodes)

summary(mfrom)
summary(mto)

edges.dbf2 <- data.frame(edges.dbf,from=uni.nodes[mfrom,1],to=uni.nodes[mto,1])
summary(edges.dbf2)

length(edges.dbf2[duplicated(edges.dbf2[,idcol]),1])


##################################################################################
#Below ReachIdentifier, Length, ToNode and FromNode
edgesnet <- data.frame(edges.dbf2[,c(idcol,lencol,dim(edges.dbf)[2] +1 ,dim(edges.dbf)[2] +2)])

options("scipen"=6)
nona <- is.na(apply(edgesnet[,3:4],1,sum))
edgesnet.filt <- edgesnet[nona == 'FALSE',]
edgesnet.attr <- edges.dbf[nona == 'FALSE',]

##Write Graph Connectivity File##
dim(edgesnet.filt)
table.norow(edgesnet.filt[,3:4],paste("c:/data/projectslocal/segments/SEGnet.GR",wria,".txt",sep=""),col.names=F)
##################################################################################


##Import Graph and add identifier attributes
graphset<- read.graph(paste("c:/data/projectslocal/segments/SEGnet.GR",wria,".txt",sep=""))
graphset <- set.edge.attribute(graphset,"length",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"weight",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"OBJECTID",E(graphset),edgesnet.attr[,idcol])
graphset <- set.edge.attribute(graphset,"ToNode",E(graphset),edgesnet.filt$to)
graphset <- set.edge.attribute(graphset,"FromNode",E(graphset),edgesnet.filt$from)

lastv <-  length(V(graphset))
graphset <- set.vertex.attribute(graphset,"nodes",V(graphset),c(0,nodes))
V(graphset)$nodes[1:10]

##END LINEWORK PREP##
##########################################################
##CLUSTER VECTOR
# Cluters are based on vertices but vertices have no attributes
# the "temp" vectors below add the cluster attributes to the vertices
# reading the graph appears to prepend one vertex which is not
# present in the nodes prior to export, need to check this

################ BELOW NOT CAPTURED IN CLUST ASSIGN function

bob.clust <- clusters(graphset)

temp1 <- bob.clust$membership   #get cluster number
temp2 <- data.frame(clustid=temp1,clustsize=bob.clust$csize[temp1+1]) #match to cluster size, cluster 1 is position 2
temp3 <- data.frame(rbind(uni.nodes[1,],uni.nodes),temp2)
temp3[1,] <- 0
graphset <- set.vertex.attribute(graphset,"members",V(graphset),c(bob.clust$csize[temp1+1]))
graphset <- set.vertex.attribute(graphset,"membership",V(graphset),bob.clust$membership)
#graphset <- set.vertex.attribute(graphset,"xcoor",V(graphset),c(0,temp3$XLoc))
#graphset <- set.vertex.attribute(graphset,"ycoor",V(graphset),c(0,temp3$YLoc))
#graphset <- set.vertex.attribute(graphset,"UNINDEX",V(graphset),c(0,temp3$UNINDEX))
#graphset <- set.vertex.attribute(graphset,"OBJECTID",V(graphset),c(0,temp3$OBJECTID))
graphset <- set.vertex.attribute(graphset,"xcoor",V(graphset),temp3$XLoc)
graphset <- set.vertex.attribute(graphset,"ycoor",V(graphset),temp3$YLoc)
graphset <- set.vertex.attribute(graphset,"UNINDEX",V(graphset),temp3$UNINDEX)
graphset <- set.vertex.attribute(graphset,"OBJECTID",V(graphset),temp3$OBJECTID)



V(graphset)$xcoor[1:10]
V(graphset)$ycoor[1:10]
V(graphset)$UNINDEX[1:10]
V(graphset)$OBJECTID[1:10]
V(graphset)$membership[1:10]
cfrom.nodes <-  apply(cbind(edgesnet.attr$FromX,edgesnet.attr$FromY),1,paste,collapse="")
cto.nodes <-  apply(cbind(edgesnet.attr$ToX,edgesnet.attr$ToY),1,paste,collapse="")

cmfrom <- match(cfrom.nodes, c(0,nodes)) #added zero to account for shift in temp3
cmto <- match(cto.nodes, c(0,nodes))


tindexes<- data.frame(members=temp3[cmfrom,dim(temp3)[2]],cluster=temp3[cmfrom,dim(temp3)[2]-1])
edges.dbf.clust <- data.frame(edgesnet.attr,tocluster=temp3[cmfrom,1],fromclust=temp3[cmto,1],tindexes)
#WRITE OUT CLUSTER vector
write.dbf(edges.dbf.clust,paste("C:/data/projectslocal/segments/edgetest2",wria,".dbf",sep=""))
write.dbf(temp3,paste("C:/data/projectslocal/segments/nodestest",wria,".dbf",sep=""))

########################################




gr1<- subgraph(graphset,V(graphset)[V(graphset)$members > 9])
summary(gr1)
uni.clus<-unique(get.vertex.attribute(gr1,"membership",V(gr1)))

clusters(gr1)
uni.clus


gr2 <- as.undirected(gr1,mode=c("each"))
summary(gr1)
summary(gr2)
bob <- get.shortest.paths(gr2,499,V(gr2)[1:2000])
bob2 <- pathlengths(bob,gr2)
bob3 <- pathsums(bob2)
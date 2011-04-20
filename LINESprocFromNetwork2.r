library(igraph)
library(foreign)
memory.limit(1024*3.75)
source( "E:\\CODE_local\\HydroNetworks_local\\Trunk\\NetFunctions.r")
#source( "g:\\str24project\\Trunk\\NetFunctions.r")
wria <- c("WA")
LINEID <- c("OBJECTID") #wchydro is LLID
LENGTH <- c("Shape_Length") #wchydro is LLID
library(RODBC)
cnn1 <- odbcConnectAccess("c:/data/projectslocal/pointsholder.mdb")
#cnn1 <- odbcConnectAccess("g:\\str24project\\StrNetOut.mdb")
edges.dbf <- sqlFetch(cnn1,"edgesNOCOL")
nodes.dbf<- sqlFetch(cnn1,"junctionsNOCOL")


edges.dbf <- edges.dbf[,-c(match("Shape",names(edges.dbf)))]
nodes.dbf <- nodes.dbf[,-c(match("SHAPE",names(nodes.dbf)))]

demValues <- read.csv("C:\\Documents and Settings\\pierckbp\\Local Settings\\Temp\\newsamp2")
sum(nodes.dbf$POINT_X-demValues$x)
nodes.dbf <- data.frame(nodes.dbf, WADEM=demValues$wadem10)





nodes.dbf<- nodes.dbf[order(nodes.dbf$OBJECTID),]
idcol<- match(LINEID,names(edges.dbf))
lencol<- match(LENGTH,names(edges.dbf))
tocol<- match("ToNode",names(edges.dbf))
fromcol<- match("FromNode",names(edges.dbf))

#

##################################################################################
#Below ReachIdentifier, Length, ToNode and FromNode
edgesnet <- data.frame(edges.dbf[,c(idcol,lencol, tocol, fromcol)])

options("scipen"=6)
nona <- is.na(apply(edgesnet[,3:4],1,sum))
edgesnet.filt <- edgesnet[nona == 'FALSE',]
edgesnet.attr <- edges.dbf[nona == 'FALSE',]
rm(nona)
rm(edgesnet)
##Write Graph Connectivity File##
dim(edgesnet.filt)
table.norow(edgesnet.filt[,3:4],paste("c:/data/projectslocal/segments/SEGnet.GR",wria,".txt",sep=""),col.names=F)
#table.norow(edgesnet.filt[,3:4],paste("g:/str24project/SEGnet.GR",wria,".txt",sep=""),col.names=F)
##################################################################################


##Import Graph and add identifier attributes
#graphset<- read.graph(paste("g:/str24project/SEGnet.GR",wria,".txt",sep=""))
graphset<- read.graph(paste("c:/data/projectslocal/segments/SEGnet.GR",wria,".txt",sep=""))
graphset <- set.edge.attribute(graphset,"length",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"weight",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"OBJECTID",E(graphset),edgesnet.attr[,idcol])
graphset <- set.edge.attribute(graphset,"ToNode",E(graphset),edgesnet.filt$ToNode)
graphset <- set.edge.attribute(graphset,"FromNode",E(graphset),edgesnet.filt$FromNode)
lastv <-  length(V(graphset))



graphset <- set.vertex.attribute(graphset,"nodes",V(graphset),c(0,nodes.dbf$OBJECTID))
graphset <- set.vertex.attribute(graphset,"WADEM",V(graphset),c(0,nodes.dbf$WADEM))
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
temp3 <- data.frame(NODEID=c(0,nodes.dbf$OBJECTID),temp2)
temp3[1,] <- 0
graphset <- set.vertex.attribute(graphset,"members",V(graphset),c(bob.clust$csize[temp1+1]))
graphset <- set.vertex.attribute(graphset,"membership",V(graphset),bob.clust$membership)
graphset <- set.vertex.attribute(graphset,"UNINDEX",V(graphset),temp3$UNINDEX)
graphset <- set.vertex.attribute(graphset,"NODEID",V(graphset),temp3$NODEID)
graphset <- set.vertex.attribute(graphset,"COAST",V(graphset),c(0,nodes.dbf$CoastCol))
graphset <- set.vertex.attribute(graphset,"DEGREE",V(graphset),degree(graphset,v=V(graphset),mode=c("total")))



V(graphset)$members[1:100]
V(graphset)$NODEID[1:100]
V(graphset)$membership[1:100]
V(graphset)$DEGREE[1:100]
V(graphset)$WADEM[1:100]

cfrom.nodes <-  E(graphset)$FromNode
cto.nodes <-  E(graphset)$ToNode

length(cfrom.nodes)
length(cto.nodes)

allnodes <- V(graphset)$NODEID
setElev <- data.frame(NODEID=V(graphset)$NODEID, ClustID=V(graphset)$membership, DEGREE=V(graphset)$DEGREE, DEM=V(graphset)$WADEM, ClustSize = V(graphset)$members, ISSINK = 0, TIES = -1,COAST=V(graphset)$COAST)


list.vertex.attributes(graphset)
list.edge.attributes(graphset)

cmfrom <- match(cfrom.nodes, allnodes) #added zero to account for shift in temp3
cmto <- match(cto.nodes, allnodes)




tindexes<- data.frame(ClusterSize=temp3[cmfrom,dim(temp3)[2]],ClusterID=temp3[cmfrom,dim(temp3)[2]-1])     
edges.dbf.clust <- data.frame(edgesnet.attr,tonode=temp3[cmto,1],fromnode=temp3[cmfrom,1],tindexes,toElev=setElev[cmto,"DEM"],fromElev=setElev[cmfrom,"DEM"])
tanGradient <- (edges.dbf.clust$fromElev- edges.dbf.clust$toElev)/edges.dbf.clust$Shape_Length
Gradient <- atan(tanGradient)/2/pi*360

negGrad<-Gradient[Gradient<0]
length(negGrad[!is.na(negGrad)])
hist(negGrad[!is.na(negGrad)])

edges.dbf.clust <- data.frame(edges.dbf.clust, Gradient)

#WRITE OUT CLUSTER vector
#write.dbf(edges.dbf.clust,paste("e:/str24project/edgetest2",wria,".dbf",sep=""))
#write.dbf(setElev,paste("e:/str24project/nodestest",wria,".dbf",sep=""))

write.dbf(edges.dbf.clust,paste("c:/data/projectslocal/segments/edgetest2",wria,".dbf",sep=""))
write.dbf(setElev,paste("c:/data/projectslocal/segments/nodestest",wria,".dbf",sep=""))



cor(edges.dbf$OBJECTID, edges.dbf.clust$OBJECTID)
cor(nodes.dbf$OBJECTID, temp3$NODEID[-1])
rm(temp3)

########################################



#
#gr1<- subgraph(graphset,V(graphset)[V(graphset)$members > 100])
#summary(gr1)
#uni.clus<-unique(get.vertex.attribute(gr1,"membership",V(gr1)))
#
#clusters(gr1)
#uni.clus
#
#
#gr2 <- as.undirected(gr1,mode=c("each"))
#summary(gr1)
#summary(gr2)
#bob <- get.shortest.paths(gr2,1,200)
#bob2 <- pathlengths(bob,gr2)
#bob3 <- pathsums(bob2)
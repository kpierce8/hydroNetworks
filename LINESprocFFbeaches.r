library(igraph)
library(foreign)
source( "e:/foragefish/HydroNetworks/source/NetFunctions.r")

wria <- c("SZ")
LINEID <- c("UNIT_ID") #wchydro is LLID
LENGTH <- c("Shape_Length") #wchydro is LLID
library(RODBC)
cnn1 <- odbcConnectAccess("e:/ForageFish/myForageFish.mdb")
lines.dbf <- sqlFetch(cnn1,"szlinePF_single")
nodes.dbf<- sqlFetch(cnn1,"SZline_Nodes")
idcol<- match(LINEID,names(lines.dbf))
lencol<- match(LENGTH,names(lines.dbf))
#nodes.dbf <-read.dbf(paste("C:\\temp\\wchydro",wria,"_FeatureVerticesToP.dbf",sep=""))
#lines.dbf <-read.dbf(paste("C:\\data\\WCCheck\\wchydro",wria,".dbf",sep=""))

############################################
#Duplicate and node finding
############################################

#create dataset of 4 position columns
ends <- c("FromX","FromY","ToX","ToY")
get.ends <- match(ends,names(lines.dbf))
dataset <- signif(lines.dbf[,get.ends],digits=8)

#create nodeset of 2 point locations
nodes <- c("XLoc","YLoc")
get.nodes <- match(nodes,names(nodes.dbf))
nodesset<- signif(nodes.dbf[,get.nodes],digits=8)

##Check for dupes##################################
#find polylines with dulicate start and end positions (could be dupes or loops)
  bob<-apply(dataset,1,paste,collapse="")
  bob.dupes<-bob[duplicated(bob)]

  dupes.index <- bob %in% bob.dupes
  length(bob[dupes.index])
  lines.dupes <- lines.dbf[dupes.index,]
  length(lines.dbf[duplicated(lines.dbf[,idcol]),1])

  #look for reachcode&length dupes in those discovered above
  # or some combination of ID and length

  bob2 <- apply(lines.dupes[,c(idcol,lencol)],1,paste,collapse="")
  reach.dupes<-bob2[duplicated(bob2)]
  reach.index <- bob2 %in% reach.dupes
  length(bob2[reach.index])

  lines.dupes <- data.frame(lines.dupes,DupeLength = reach.index)

  #UNCOMMENT TO write files
  #write.dbf(lines.dupes, "G:\\lines\\lines.dupes.dbf")
  #table.norow(lines.dupes, "G:\\lines\\lines.dupes.txt")
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

lines.dbf2 <- data.frame(lines.dbf,from=uni.nodes[mfrom,1],to=uni.nodes[mto,1])
summary(lines.dbf2)

length(lines.dbf2[duplicated(lines.dbf2[,idcol]),1])


##################################################################################
#Below ReachIdentifier, Length, ToNode and FromNode
linesnet <- data.frame(lines.dbf2[,c(idcol,lencol,dim(lines.dbf)[2] +1 ,dim(lines.dbf)[2] +2)])

options("scipen"=6)
nona <- is.na(apply(linesnet[,3:4],1,sum))
linesnet.filt <- linesnet[nona == 'FALSE',]
linesnet.attr <- lines.dbf[nona == 'FALSE',]

##Write Graph Connectivity File##
dim(linesnet.filt)
table.norow(linesnet.filt[,3:4],paste("e:/foragefish/wchydroGR",wria,".txt",sep=""),col.names=F)
##################################################################################


##Import Graph and add identifier attributes
graphset<- read.graph(paste("e:/foragefish/wchydroGR",wria,".txt",sep=""))
graphset <- set.edge.attribute(graphset,"length",E(graphset),linesnet.attr[,2])
graphset <- set.edge.attribute(graphset,"OBJECTID",E(graphset),linesnet.attr[,1])
graphset <- set.edge.attribute(graphset,"ToNode",E(graphset),linesnet.filt$to)
graphset <- set.edge.attribute(graphset,"FromNode",E(graphset),linesnet.filt$from)

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
temp2 <- data.frame(temp1,bob.clust$csize[temp1+1]) #match to cluster size, cluster 1 is position 2
temp3 <- data.frame(rbind(uni.nodes[1,],uni.nodes),temp2)
temp3[1,] <- 0
graphset <- set.vertex.attribute(graphset,"members",V(graphset),c(bob.clust$csize[temp1+1],1))
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
cfrom.nodes <-  apply(cbind(linesnet.attr$FromX,linesnet.attr$FromY),1,paste,collapse="")
cto.nodes <-  apply(cbind(linesnet.attr$ToX,linesnet.attr$ToY),1,paste,collapse="")

cmfrom <- match(cfrom.nodes, c(0,nodes)) #added zero to account for shift in temp3
cmto <- match(cto.nodes, c(0,nodes))


tindexes<- data.frame(members=temp3[cmfrom,dim(temp3)[2]],cluster=temp3[cmfrom,dim(temp3)[2]-1])
lines.dbf.clust <- data.frame(linesnet.attr,tocluster=temp3[cmfrom,1],fromclust=temp3[cmto,1],tindexes)
#WRITE OUT CLUSTER vector
write.dbf(lines.dbf.clust,paste("C:/temp/edgetest2",wria,".dbf",sep=""))
write.dbf(temp3,paste("C:/temp/nodestest",wria,".dbf",sep=""))

########################################




gr1<- subgraph(graphset,V(graphset)[V(graphset)$members > 9])
summary(gr1)
uni.clus<-unique(get.vertex.attribute(gr1,"membership",V(gr1)))

clusters(gr1)
uni.clus
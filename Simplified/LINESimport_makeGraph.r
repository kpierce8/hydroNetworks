##Make Separate Script
##Import Graph and add identifier attributes

graphset<- read.graph(paste(pldir,"/SEGnet.GR",wria,".txt",sep=""))
graphset <- set.edge.attribute(graphset,"length",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"weight",E(graphset),edgesnet.attr[,lencol])
graphset <- set.edge.attribute(graphset,"OBJECTID",E(graphset),edgesnet.attr[,idcol])
graphset <- set.edge.attribute(graphset,"ToNode",E(graphset),edgesnet.filt$ToNode)
graphset <- set.edge.attribute(graphset,"FromNode",E(graphset),edgesnet.filt$FromNode)
lastv <-  length(V(graphset))



graphset <- set.vertex.attribute(graphset,"nodes",V(graphset),c(0,nodes.dbf$OBJECTID))
graphset <- set.vertex.attribute(graphset,"WADEM",V(graphset),c(0,nodes.dbf[,elevcol]))
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
tanGradient <- (edges.dbf.clust$fromElev- edges.dbf.clust$toElev)/edges.dbf.clust$Shape_Leng
Gradient <- atan(tanGradient)/2/pi*360

negGrad<-Gradient[Gradient<0]
length(negGrad[!is.na(negGrad)])
hist(negGrad[!is.na(negGrad)])

edges.dbf.clust <- data.frame(edges.dbf.clust, Gradient)

#WRITE OUT CLUSTER vector
#write.dbf(edges.dbf.clust,paste("e:/str24project/edgetest2",wria,".dbf",sep=""))
#write.dbf(setElev,paste("e:/str24project/nodestest",wria,".dbf",sep=""))

write.dbf(edges.dbf.clust,paste(pldir,"/edgetest2",wria,".dbf",sep=""))
write.dbf(setElev,paste(pldir,"/nodestest",wria,".dbf",sep=""))



cor(edges.dbf$OBJECTID, edges.dbf.clust$OBJECTID)
cor(nodes.dbf$OBJECTID, temp3$NODEID[-1])
rm(temp3)

library(igraph)
library(foreign)
source( "C:\\data\\WCCheck\\HydroNetworks\\Trunk\\NetFunctions.r")

wria <- 22

nodes.dbf <-read.dbf(paste("C:\\temp\\wchydro",wria,"_FeatureVerticesToP.dbf",sep=""))
lines.dbf <-read.dbf(paste("C:\\data\\WCCheck\\wchydro",wria,".dbf",sep=""))

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
  length(lines.dbf[duplicated(lines.dbf$LLID),1])

  #look for reachcode&length dupes in those discovered above
  # or some combination of ID and length

  bob2 <- apply(lines.dupes[,c(2,35)],1,paste,collapse="")
  reach.dupes<-bob2[duplicated(bob2)]
  reach.index <- bob2 %in% reach.dupes
  length(bob2[reach.index])

  lines.dupes <- data.frame(lines.dupes,DupeLength = reach.index)

  #UNCOMMENT TO write files
  #write.dbf(lines.dupes, "G:\\lines\\lines.dupes.dbf")
  #table.norow(lines.dupes, "G:\\lines\\lines.dupes.txt")
######################################################


##CREATE Ends and Node IDs with poition information
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

length(lines.dbf2[duplicated(lines.dbf2$ObjectID),1])

##################################################################################
#Below ReachIdentifier, Length, ToNode and FromNode
linesnet <- data.frame(lines.dbf2[,c(1,35,46,47)])

options("scipen"=6)
nona <- is.na(apply(linesnet[,3:4],1,sum))
linesnet.filt <- linesnet[nona == 'FALSE',]
linesnet.attr <- lines.dbf[nona == 'FALSE',]

##Write Graph Connectivity File##
dim(linesnet.filt)
table.norow(linesnet.filt[,3:4],paste("c:/rprojects/networks/wchydroGR",wria,".txt",sep=""),col.names=F)
##################################################################################


##Import Graph and add identifier attributes
graphset<- read.graph(paste("c:/rprojects/networks/wchydroGR",wria,".txt",sep=""))
graphset <- set.edge.attribute(graphset,"length",E(graphset),linesnet.attr$SHAPE_len)
graphset <- set.edge.attribute(graphset,"weight",E(graphset),linesnet.attr$SHAPE_len)
graphset <- set.edge.attribute(graphset,"OBJECTID",E(graphset),linesnet.attr$OBJECTID)
graphset <- set.edge.attribute(graphset,"ToNode",E(graphset),linesnet$to)
graphset <- set.edge.attribute(graphset,"FromNode",E(graphset),linesnet$from)
graphset <- set.vertex.attribute(graphset,"nodes",V(graphset)[-1],nodes)


##END LINEWORK PREP##
##########################################################
##CLUSTER VECTOR
# Cluters are based on vertices but vertices have no attributes
# the "temp" vectors below add the cluster attributes to the vertices
# reading the graph appears to prepend one vertex which is not
# present in the nodes prior to export, need to check this






################ BELOW CAPTURED IN CLUST ASSIGN function

bob.clusters <- clusters(graphset)

temp1 <- bob.clusters$membership[-1]   #get cluster number
temp2 <- data.frame(temp1,bob.clusters$csize[temp1+1]) #match to cluster size
temp3 <- data.frame(uni.nodes,temp2)

cfrom.nodes <-  apply(linesnet.attr[,42:43],1,paste,collapse="")
cto.nodes <-  apply(linesnet.attr[,44:45],1,paste,collapse="")

cmfrom <- match(cfrom.nodes, nodes)
cmto <- match(cto.nodes, nodes)

lines.dbf.clust <- data.frame(linesnet.attr,tocluster=temp3[cmfrom,1],fromclust=temp3[cmto,1],members=temp3[cmfrom,50])
#WRITE OUT CLUSTER vector
write.dbf(lines.dbf.clust,paste("C:/temp/edgetest",wria,".dbf",sep=""))
write.dbf(temp3,paste("C:/temp/nodestest",wria,".dbf",sep=""))

#testno<- 1030
#temp2[testno,]
#dim(temp2[temp2[,1]== temp2[testno,1],])

#no.clusters(graphset)
#hist(log(clusters(graphset)[[2]]),nclass=10)
#max(clusters(graphset)[[2]])
#clus.list<- clusters(graphset)[[2]]
#hist(clus.list[clus.list <10])
#hist(clus.list[clus.list >=200])
#########################################################
gr1<- subgraph(graphset,V(graphset)[V(graphset)$membership == 7])

gr2 <- as.undirected(gr1,mode="each")


gBlocks <- cohesive.blocks(gr2)
plot(gBlocks,layout=layout.kamada.kawai,vertex.size=7)


pt1 <- get.shortest.paths(gr2,48,9)
pt2 <- subgraph(gr2,pt1[[1]])
average.path.length(pt2)

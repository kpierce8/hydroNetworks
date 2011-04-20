#str24.dupes <- str24.dbf[duplicated(str24.dbf[,18:21],margin=1),]

library(igraph)
library(foreign)
str24.dbf <-read.dbf("G:\\str24\\str24.dbf")
nodes.dbf <-read.dbf("G:\\str24\\str24nodes.dbf")


############################################
#Duplicate and node finding
############################################

#create dataset of 4 position columns
dataset <- signif(str24.dbf[,15:18],digits=8)

#find polylines with dulicate start and end positions (could be dupes or loops)
bob<-apply(dataset,1,paste,collapse="")
bob.dupes<-bob[duplicated(bob)]

dupes.index <- bob %in% bob.dupes
length(bob[dupes.index])
str24.dupes <- str24.dbf[dupes.index,]
length(str24.dbf[duplicated(str24.dbf$LLID),15])

#look for reachcode&length dupes in those discovered above
bob2 <- apply(str24.dupes[,c(5,7)],1,paste,collapse="")
reach.dupes<-bob2[duplicated(bob2)]
reach.index <- bob2 %in% reach.dupes
length(bob2[reach.index])

str24.dupes <- data.frame(str24.dupes,DupeLength = reach.index)

#UNCOMMENT TO write files
#write.dbf(str24.dupes, "G:\\str24\\str24.dupes.dbf")
#table.norow(str24.dupes, "G:\\str24\\str24.dupes.txt")




from.nodes <-  apply(signif(dataset[,1:2],digits=8),1,paste,collapse="")
to.nodes <-  apply(signif(dataset[,3:4],digits=8),1,paste,collapse="")
nodes <-  apply(signif(nodes.dbf[,3:4],digits=8),1,paste,collapse="")

mfrom <- match(from.nodes, nodes)
mto <- match(to.nodes, nodes)

summary(mfrom)
summary(mto)


str24.dbf2 <- data.frame(str24.dbf,from=nodes.dbf[mfrom,1],to=nodes.dbf[mto,1])
summary(str24.dbf2)

#str24.dbf3 <- str24.dbf2[str24.dbf2$FType == 460,]
#summary(str24.dbf3)
#Still no NA's for ComID

#addrows<-  str24.dbf2[grep("xWaterbody",str24.dbf2$KBP_types),]
#dim(addrows)
#addrows<-  addrows[addrows$FType == 558,]
#dim(addrows)
#str24.dbf3 <- rbind(str24.dbf3, addrows)
length(str24.dbf2[duplicated(str24.dbf2$LLID),5])


#str24.dbf3 <- str24.dbf3[order(str24.dbf3$HydroID),]
#summary(str24.dbf3)
#Below LLID, length, To and From
str24net <- data.frame(str24.dbf2[,c(5,7,19,20)])

options("scipen"=6)
nona <- is.na(apply(str24net[,3:4],1,sum))
str24net.filt <- str24net[nona == 'FALSE',]
str24_prob <- str24net[nona,]


str24_prob <- str24.dbf[str24.dbf2$LLID %in% str24_prob$LLID,]

write.dbf(str24_prob,"C:/temp/str24_prob.dbf")


dim(str24net.filt)
table.norow(str24net.filt[,3:4],"c:/rprojects/networks/biggraph2.txt",col.names=F)
bob7<- read.graph("c:/rprojects/networks/biggraph2.txt")

no.clusters(bob7)
hist(log(clusters(bob7)[[2]]),nclass=10)
max(clusters(bob7)[[2]])

bob.clusters <- clusters(bob7)

clus.list<- clusters(bob7)[[2]]
hist(clus.list[clus.list <10])
hist(clus.list[clus.list >=200])

temp1 <- bob.clusters$membership[-1]
temp2 <- data.frame(temp1,bob.clusters$csize[temp1+1])

testno<- 1030
temp2[testno,]
dim(temp2[temp2[,1]== temp2[testno,1],])

temp3 <- data.frame(nodes.dbf,temp2)


str24.dbf4 <- str24.dbf[nona == 'FALSE',]

cfrom.nodes <-  apply(str24.dbf4[,18:19],1,paste,collapse="")
cto.nodes <-  apply(str24.dbf4[,20:21],1,paste,collapse="")

cmfrom <- match(cfrom.nodes, nodes)
cmto <- match(cto.nodes, nodes)



str24.dbf.clust <- data.frame(str24.dbf4,tocluster=temp3[cmfrom,11],fromclust=temp3[cmto,11],members=temp3[cmfrom,12])

write.dbf(str24.dbf.clust,"C:/temp/edgetest.dbf")
write.dbf(temp3,"C:/temp/nodestest.dbf")
#More nodes are produced than edges because singleton edges must have two nodes
#froms and tos are node ids, clusters are tagged to node ids, create a node:cluster tables and search for the cluster number
#head.bob.clust)with to or from nodes (they should be the same, a good check perhaps)

es<- E(bob7)
get.edges(bob7,es)[1]


bob8 <- bob7
bob8 <- set.vertex.attribute(bob8,"membership",V(bob8),bob.clusters$membership)
bob8 <- set.edge.attribute(bob8,"length",E(bob8),str24.dbf4[,13])
bob8 <- set.vertex.attribute(bob8,"xcoor",V(bob8)[-1],temp3[,8])
bob8 <- set.vertex.attribute(bob8,"ycoor",V(bob8)[-1],temp3[,9])
bob8 <- set.edge.attribute(bob8,"ComID",E(bob8),str24.dbf4[,2])


summary(bob8)

gr1<- subgraph(bob8,V(bob8)[V(bob8)$membership==15])
plot(gr1)

summary(gr1)

plot(gr1,layout=cbind(V(gr1)$xcoor,V(gr1)$ycoor),vertex.size=7)
get.edge.attribute(gr1,"ComID")


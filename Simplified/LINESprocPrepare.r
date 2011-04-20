#Output sent to pldir

library(igraph)
library(foreign)
library(RODBC)
memory.limit(1024*3.75)
source( "E:\\CODE_local\\HydroNetworks_local\\Trunk\\NetFunctions.r")

######### INSTANCE DATA ########################################################
wria <- c("15")
LINEID <- c("OBJECTID") #wchydro is LLID
LENGTH <- c("LENGTH") #wchydro is LLID   SHORTENED FOR DBF
ELEVATION <- c("RASTERVALU")
nodes.dbf<- read.dbf("C:\\data\\ProjectsLocal\\Segments\\wria15\\w15nodes.dbf")
edges.dbf <- read.dbf("C:\\data\\ProjectsLocal\\Segments\\wria15\\w15edges.dbf")
######### END INSTANCE DATA #######################################################
idcol<- match(LINEID,names(edges.dbf))
lencol<- match(LENGTH,names(edges.dbf))
elevcol<- match(ELEVATION,names(nodes.dbf))
nodes.dbf<- nodes.dbf[order(nodes.dbf[,idcol]),]

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
table.norow(edgesnet.filt[,3:4],paste(pldir,"/SEGnet.GR",wria,".txt",sep=""),col.names=F)
#table.norow(edgesnet.filt[,3:4],paste("g:/str24project/SEGnet.GR",wria,".txt",sep=""),col.names=F)
##################################################################################



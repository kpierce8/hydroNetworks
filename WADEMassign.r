################################################################################
####Iterate through clusters finding vector of lowest elevation points
#### save for each cluster the 
####    1) clusterID
####    2) lowest elevation
####    3) vector of NODEIDs with the lowest elevation
################################################################################

minList <- list() #List to hold cluster information

theClusters <- unique(bob.clust$membership) # Vector of unique cluster IDS

#Cluster number, NodeID data.frame
vertMem <- data.frame(clustnum =V(graphset)$membership,NODEID = V(graphset)$NODEID)

#Simple analysis data frame with NODEID, CLUSTID, DEGREE, ELEVATION, COAST
#setElev <- data.frame(NODEID=V(graphset)$NODEID, ClustID=V(graphset)$membership, DEGREE=V(graphset)$DEGREE, DEM=V(graphset)$WADEM, COAST=V(graphset)$COAST)


# Subset dataframe for only TERMINAL nodes (i.e. valence/degree = 1)
terminalElev <- setElev[setElev$DEGREE == 1,]

# iterate through clusters to populate MINLIST
for(i in 2:length(theClusters)){
vertList <- terminalElev[terminalElev$ClustID == theClusters[i],]
minElev <- min(vertList$DEM, na.rm=T)
returnList <- vertList[vertList$DEM == minElev, "NODEID"]
minList[[i]] <- list(theClusters[i], minElev, returnList[!is.na(returnList)] )                
}


# Create dataframe by CLUSTID with each row containing, CLUSTID, TIES, NODEID (of first node in vector)
clusterID <- c()
    for(i in 1:length(minList)){
           clusterID <- c(clusterID,   minList[[i]][[1]][1])
           }
TIES <- c()
for(i in 1:length(minList)){
           TIES <- c(TIES,   length(minList[[i]][[3]]))
           }
          
lowNodes <- c()

for(i in 1:length(minList)){
           lowNodes <- c(lowNodes,   minList[[i]][[3]][1])
           }

mySinksAll <- data.frame( theClusters, TIES, lowNodes=c(0,lowNodes), csize=bob.clust$csize)
sinksVector <- mySinksAll[mySinksAll$TIES<2, "lowNodes"]
sinksVector <- sinksVector[!is.na(sinksVector)]
table.norow(sinksVector, "C:/data/projectslocal/segments/sinksvector.txt", col.names=F)
mySinksAll2 <- data.frame( clusterID, TIES[-1], lowNodes)
table.norow(mySinksAll, "C:/data/projectslocal/segments/sinksdata.txt", col.names=T)     
   
           
# Create DataFrame from three Vectors with a row for each low node           
all.lowNodes <- c()   # All NODEIDs listed as low node ids (variable length per cluster)
all.clusterID <- c()  # CLUSTID vector, same length as all.lowNodes with NODEIDs for each low node
all.TIES <- c()       # TIES vector,same length as all.lowNodes with TIESs for each low node
for(i in 1:length(minList)){
           mylen <- length(minList[[i]][[3]])
           all.lowNodes <- c(all.lowNodes,   minList[[i]][[3]])
           all.clusterID <- c(all.clusterID,   rep(minList[[i]][[1]][1],mylen))
           all.TIES <- c(all.TIES, rep(mylen, mylen))
           }
allSinks <- data.frame(OBJECTID=all.lowNodes, CLUSTERID=all.clusterID, ISSINK = 1, TIES = all.TIES)
table.norow(allSinks, "C:/data/projectslocal/segments/NodesAsSinks.txt", col.names=T)

#########  Analyze TIES > 1 for single edge anomolies and clusters with a single terminal node as CoastColumbia

tieSinks <- allSinks[allSinks$TIES > 1,]
TSlist<-table(tieSinks$TIES)
ClusterTieCounts<-table(tieSinks$TIES)/as.numeric(dimnames(TSlist)[[1]])

TiesUnique <-  unique(tieSinks$CLUSTERID)  #List of Unique CLUSTIDs with TIES > 1 
##########################################################
## RUN LLIDsummary      New TIME BOTTLENECK  
#############################################################
logicClusters <- c()  #Vector for CLUSTIDs with single assignable sink points
logicNodes <- c()     #Vector od sink NODEIDs


for(i in 1:length(TiesUnique)){

#for(i in 10){

vertList <- terminalElev[terminalElev$ClustID == TiesUnique[i],]
vGet <- match(tieSinks$OBJECTID, vertList$NODEID)
vertList <- vertList[vGet[!is.na(vGet)],]

gEdge <- unique(c(match(vertList$NODEID, LLIDedgesALL$tonode), match(vertList$NODEID, LLIDedgesALL$fromnode)))
gEdge <- gEdge[!is.na(gEdge)]
gEdgeList<- LLIDedgesALL[gEdge,]

RankSum <- sum(gEdgeList[gEdgeList$RANK == 1,"RANK"])

addNode <- c()
addClust <- c()
coastSum <- sum(vertList$COAST, na.rm = T)
#If only one vertex in vertList is a Coast vertex then that gets set as the flow node
  if(coastSum == 1) {addNode <- vertList[which(vertList$COAST == 1), "NODEID"]; addClust <- TiesUnique[i] }   else
  if(coastSum > 1  && RankSum == 1 && dim(gEdgeList)[1] > 1)   {checkNodes <- as.vector(gEdgeList[which(gEdgeList$RANK == 1),c("tonode","fromnode")]); checkNodes <- c(checkNodes[1,1],checkNodes[1,2]); addNode <- checkNodes[!is.na(match(checkNodes,vertList$NODEID))]; addClust <- TiesUnique[i] }

logicNodes <- c(logicNodes, addNode)
logicClusters <- c(logicClusters, addClust)
if (length(addNode) > 1)  {mNodes <- addNode; mClust <- addClust}
rm(addNode)
rm(addClust)
}

logicNodes
logicClusters
length(logicNodes)
length(logicClusters)

ProblemClusters <- setdiff(TiesUnique, logicClusters)
ProblemsC <- tieSinks[tieSinks$CLUSTERID  %in%  ProblemClusters,]
ProblemsCtally<- table(ProblemsC$CLUSTERID)

SpotCheck <- ProblemsCtally[ProblemsCtally > 2]
write.table(SpotCheck,"C:/data/projectslocal/segments/SpotCheck.txt") 

sinksVector2 <- c(sinksVector,logicNodes)
table.norow(sinksVector2, "C:/data/projectslocal/segments/sinksvector.txt", col.names=F)

##SEts ISSINK and TIES propertie
gSink <- match(allSinks$OBJECTID, setElev$NODEID)
setElev[gSink,"ISSINK"] <- 1
setElev[gSink,"TIES"] <- allSinks$TIES
write.dbf(setElev,paste("c:/data/projectslocal/segments/nodestest",wria,".dbf",sep=""))

gTIES <- match(edges.dbf.clust$ClusterID, mySinksAll$theClusters)
outputEdges <- data.frame(edges.dbf.clust, TIES=mySinksAll[gTIES,"TIES"])
gColumns <- c("OBJECTID", "ClusterSize", "ClusterID", "toElev", "fromElev", "Gradient", "TIES")
gCol<-match(gColumns, names(outputEdges))


write.dbf(outputEdges[,gCol],paste("c:/data/projectslocal/segments/edgeDATA",wria,".dbf",sep=""))



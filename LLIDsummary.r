##Code below
LLIDedgesALL <- c()
#for(i in 1:length(theClusters)){

clLength<- length(theClusters)

for(i in 1:clLength){
#i <- 6
clEdges <- edges.dbf.clust[edges.dbf.clust$ClusterID == theClusters[i],]
LLIDtab<-cbind(Count=with(clEdges,(tapply(Shape_Leng, LLID, length))),Length=with(clEdges,(tapply(Shape_Leng, LLID, sum))))
LLIDtab1 <- data.frame(LLID=row.names(LLIDtab),LLIDtab)
LLIDtab3 <- LLIDtab1[order(LLIDtab1$Length,decreasing=T),]
RANKunique <- unique(LLIDtab3[,2])
RANKorder <- data.frame(Count=RANKunique,LLIDRANK=sort(rank(RANKunique)))
cMat <- match(clEdges$LLID, LLIDtab3$LLID)
rMat <- match(LLIDtab3$Count,RANKorder$Count)
LLIDtab4 <- data.frame(LLIDtab3, RANK=RANKorder[rMat, "LLIDRANK"])
LLIDedges <- data.frame(clEdges, LLIDtab4[cMat,])
LLIDedgesALL <- rbind(LLIDedgesALL, LLIDedges)
}

dim(LLIDedges)
ed <- LLIDedges[,36:39]
tail(ed[order(ed$Length),])
tail(ed[order(ed$Count),])

write.dbf(LLIDedgesALL, "c:/data/projectslocal/segmentsLLIDedges.dbf")

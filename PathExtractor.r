
p1<-get.objid(gr2,c(2493,2480))
p2 <- get.shortest.paths(gr2,p1[1]-1,p1[2]-1)
gr3 <- subgraph(gr2,p2[[1]])
sum(E(gr3)$length)


psverts <- V(gr2)$OBJECTID
Sys.time()
for(f in 11:length(missingdist)){
    j<- missingdist[f]
    targ <- get.objid(gr2,psverts[j])
    p3 <- get.shortest.paths(gr2,targ[1]-1,V(gr2))
    V(gr2)$OBJECTID[p2[[1]]+1]
    
    PSdistmat <- data.frame(matrix(0,length(p3),3))
    
    for(i in 1:length(p3)){
    try(PSdistmat[i,1]<- (V(gr2)$OBJECTID[p3[[i]][1]]) +1) 
    try(PSdistmat[i,2]<- (V(gr2)$OBJECTID[p3[[i]][length(p3[[i]])]]) +1)
    PSdistmat[i,3]<- sum(E(gr2,path=p3[[i]])$length)
    }
    assign(paste("PS.dm.",j,sep=""),PSdistmat)
    write.table(PSdistmat,file=paste("c:/distfiles2/PMdist.",j,".all",sep=""))
}    
Sys.time()

#Nclusts <- ls(pat="PS.dm")
#Nclus2.all <- get(Nclusts[1])
#for(i in 2:length(Nclusts)){
#Nclus2.all <- rbind(Nclus2.all,get(Nclusts[i]))
#}
#
#dim(Nclus2.all)
#
#write.table(Nclus2.all,file=paste("e:/PMdist",j,".all",sep=""))
#write.table(Nclusts,file=paste("e:/PM.dir",j,".all",sep=""))
#
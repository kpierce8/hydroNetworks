

exp.membership<- function(graphset){
  #This function should accept a graph and assign membership to edges
  bob.clusters <- clusters(graphset)
  temp1 <- bob.clusters$membership[-1]   #get cluster number
  temp2 <- data.frame(temp1,bob.clusters$csize[temp1+1]) #match to cluster size
  temp3 <- data.frame(uni.nodes,temp2)
  cfrom.nodes <-  apply(cbind(linesnet.attr$FromX,linesnet.attr$FromY),1,paste,collapse="")
  cto.nodes <-  apply(cbind(linesnet.attr$ToX,linesnet.attr$ToY),1,paste,collapse="")
  cmfrom <- match(cfrom.nodes, nodes)
  cmto <- match(cto.nodes, nodes)
  lines.dbf.clust <- data.frame(linesnet.attr,tocluster=temp3[cmfrom,1],fromclust=temp3[cmto,1],members=temp3[cmfrom,50])
  #WRITE OUT CLUSTER vector
  return(lines.dbf.clust)
  write.dbf(lines.dbf.clust,paste("C:/temp/edgetest",wria,".dbf",sep=""))
  write.dbf(temp3,paste("C:/temp/nodestest",wria,".dbf",sep=""))
}
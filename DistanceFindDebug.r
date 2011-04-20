library(ecodist)

#for(f in 1:length(uni.clus)){
f <- 1
  k<-5
#      if(f != k){
        gr2<- subgraph(graphset,V(graphset)[V(graphset)$membership == uni.clus[f]])
        gr3<- subgraph(graphset,V(graphset)[V(graphset)$membership == uni.clus[k]])    
        clus1 <- data.frame(get.vertex.attribute(gr2,"xcoor",V(gr2)),get.vertex.attribute(gr2,"ycoor",V(gr2)))
        clus2 <- data.frame(get.vertex.attribute(gr3,"xcoor",V(gr3)),get.vertex.attribute(gr3,"ycoor",V(gr3)))
        t1<-Sys.time()
        names(clus1) <- c("xcoor","ycoor")
        names(clus2) <- c("xcoor","ycoor")
        #create matrix of length target cluster and columns equal to cluster number
        #below took about 10 minutes for 70k+ cluster against small cluster    
        clus1.mat <- matrix(0,nrow(clus1),2)
        for(i in 1:dim(clus1)[1]){
          temp1<- rbind(clus1[i,],clus2)
          dtemp1<-dist(temp1)    
          #dtemp2<- fixdmat(dtemp1)    
          gvert<-  which.min(dtemp1[2:dim(temp1)[1]])   
          clus1.mat[i,1]<-V(gr2)$UNINDEX[i]
          clus1.mat[i,2]<-V(gr3)$UNINDEX[gvert]
          }
        Sys.time()-t1  
        assign(paste("cluster",f,k,sep="."),clus1.mat)
#        } else
        print(c(f, k))
        
    }
#  }
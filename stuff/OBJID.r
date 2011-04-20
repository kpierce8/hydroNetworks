get.objid <- function(gr2,verts){
    numvert<- summary(gr2)[1]
    objids<-V(gr2)$UNINDEX
    objid<- match(verts,objids)
    return(objid)
    }
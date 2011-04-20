library(sp)

samps<- sample(seq(1:7000),20)

tocol<- temp3$UNINDEX[samps[1:10]]
fromcol<- temp3$UNINDEX[samps[11:20]]

tests <- cbind(tocol,fromcol)
line.test <- list()
i<- 1

l1<-cbind(c(temp3$XLoc[temp3$UNINDEX == tests[i,1]],temp3$XLoc[temp3$UNINDEX == tests[i,2]]), c(temp3$YLoc[temp3$UNINDEX == tests[i,1]],temp3$YLoc[temp3$UNINDEX == tests[i,2]]))
t1<- Line(l1)
line.test[[i]]<-
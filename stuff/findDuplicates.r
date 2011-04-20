#nhd.dupes <- nhd.dbf[duplicated(nhd.dbf[,18:21],margin=1),]

library(foreign)
library(foreign)
nhd.dbf <-read.dbf("C:\\data\\NHDH1711\\NHDFlowlineCopy.dbf")

dataset <- nhd.dbf[,18:21]

bob<-apply(dataset,1,paste,collapse="")
bob.dupes<-bob[duplicated(bob)]

dupes.index <- bob %in% bob.dupes
length(bob[dupes.index])

nhd.dupes <- nhd.dbf[dupes.index,]

#length(nhd.dbf[duplicated(nhd.dbf$ComID),2])

bob2 <- apply(nhd.dupes[,7:8],1,paste,collapse="")
reach.dupes<-bob2[duplicated(bob2)]
reach.index <- bob2 %in% reach.dupes
length(bob2[reach.index])

nhd.dupes <- data.frame(nhd.dupes,DupeLength = reach.index)

write.dbf(nhd.dupes, "C:\\data\\NHDH1711\\NHDH1711.dupes.dbf")
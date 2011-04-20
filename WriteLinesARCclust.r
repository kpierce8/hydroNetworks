#library(sp)
test.file<- read.table("f:\\Nclus2.all")

tests<-test.file
natest<- apply(tests,1,sum)
tests<-tests[!is.na(natest),]                                       
#line.test <- list()


#
#test <- file(paste("g:/LINES4.txt",sep=""), "w")
#cat("Polyline\n", sep=" ", file=test)
#for(i in 1:dim(tests)[1]){
#bob<- c(temp3$XLoc[temp3$UNINDEX == tests[i,1]],temp3$YLoc[temp3$UNINDEX == tests[i,1]], temp3$XLoc[temp3$UNINDEX == tests[i,2]],temp3$YLoc[temp3$UNINDEX == tests[i,2]])
#cat(i,"0\n",sep=" ", file=test)
#cat(1,bob[1:2],0,"0\n",sep=" ", file=test)
#cat(2,bob[3:4],0,"0\n",sep=" ", file=test)
#}
#cat("END", sep=" ", file=test)
#close(test)
#dim(tests)
#
nums <- 100000

output <- c("Polyline")
for(i in 1:100000){
output<-c(output,paste(i,0,sep=" "))
output<-c(output,paste(1,temp3$XLocD[temp3$UNINDEX == tests[i,1]],temp3$YLocD[temp3$UNINDEX == tests[i,1]],0,0,sep=" "))
output<-c(output,paste(2,temp3$XLocD[temp3$UNINDEX == tests[i,2]],temp3$YLocD[temp3$UNINDEX == tests[i,2]],0,0,sep=" "))
}
output <- c(output,"END")

write.table(output,"f:/foragefish/net100k.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
#CreateFeaturesFromTextFile C:\Rprojects\reports\LINES2.txt . linestest2 #
#CreateFeaturesFromTextFile E:\ForageFish\net100k.txt . E:\ForageFish\net100kD #
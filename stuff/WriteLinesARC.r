library(sp)

samps<- sample(seq(1:7000),20)

tocol<- temp3$UNINDEX[samps[1:10]]
fromcol<- temp3$UNINDEX[samps[11:20]]

tests <- cbind(tocol,fromcol)
line.test <- list()

test <- file(paste("C:/Rprojects/reports/LINES.txt",sep=""), "w")

cat("Polyline\n", sep=" ", file=test)


for(i in 1:10){
bob<- c(temp3$XLoc[temp3$UNINDEX == tests[i,1]],temp3$YLoc[temp3$UNINDEX == tests[i,1]], temp3$XLoc[temp3$UNINDEX == tests[i,2]],temp3$YLoc[temp3$UNINDEX == tests[i,2]])
cat(i,"0\n",sep=" ", file=test)
cat(1,bob[1:2],0,"0\n",sep=" ", file=test)
cat(2,bob[3:4],0,"0\n",sep=" ", file=test)
}

cat("END", sep=" ", file=test)


#CreateFeaturesFromTextFile C:\Rprojects\reports\LINES2.txt . linestest2 #
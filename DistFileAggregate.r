bob<-dir("c:/distfiles")
bob2<-substr(bob,7,100)
bob3 <- unlist(strsplit(bob2,".all"))
bob4<- sub('\\.', '', bob3)
bob4[1:50]


bob5 <- sort(as.numeric(bob4))
bob5[1:50]

missingdist<- setdiff(fullseq,bob5)


#Basefile <- read.table(paste("f:/distfiles/",bob[1],sep=""))
#
#for(i in 2:100){
#
#temp <- read.table(paste("g:/distfiles/",bob[i],sep=""))
#Basefile <- rbind(Basefile,temp)
#}
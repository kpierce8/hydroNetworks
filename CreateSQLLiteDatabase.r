library(RSQLite)
m <- dbDriver("SQLite")

        # initialize a new database to a tempfile and copy some data.frame
        # from the base package into it
tfile <- c("c:/disttemp2.sql")
con <- dbConnect(m, dbname = tfile)

bob<-dir("c:/distfiles2")
bob2<-substr(bob,7,100)
bob3 <- unlist(strsplit(bob2,".all"))
bob4<- sub(".", "", bob3)
bob5 <- sort(as.numeric(bob4))
length(bob)
Basefile <- read.table(paste("c:/distfiles2/",bob[1],sep=""))

dbWriteTable(con, "PSDistances", Basefile)


for(i in 1:447){
temp <- read.table(paste("c:/distfiles2/",bob[i],sep=""))
dbWriteTable(con, "PSDistances", temp, append=TRUE)
}

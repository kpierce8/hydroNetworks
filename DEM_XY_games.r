# Create x,y, text file
library(RODBC)
cnn1 <- odbcConnectAccess("c:/data/projectslocal/pointsholder.mdb")

juncs.dbf <- sqlFetch(cnn1,"junctionsNOCOL")

table.norow(juncs.dbf[,7:8], "c:\\data\\projectslocal\\jpoints.txt")

# add DEM to NODEID vector

# Location of outfile "C:\Documents and Settings\pierckbp\Local Settings\Temp\newsamp2"

demValues <- read.csv("C:\\Documents and Settings\\pierckbp\\Local Settings\\Temp\\newsamp2")
demFiles <- data.frame(juncs.dbf[,-match("SHAPE",names(juncs.dbf))], demValues)

sum(demFiles$POINT_X-demFiles$x)
max(abs(demFiles$POINT_X-demFiles$x))

sum(demFiles$POINT_Y-demFiles$y)
max(abs(demFiles$POINT_Y-demFiles$y))

write.csv(demFiles, "C:\\data\\projectslocal\\segments\\dempts.csv")
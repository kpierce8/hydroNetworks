Nclusts <- ls(pat="PS.dm")
Nclus2.all <- get(Nclusts[1])
for(i in 2:length(Nclusts)){
Nclus2.all <- rbind(Nclus2.all,get(Nclusts[i]))
}

dim(Nclus2.all)

write.table(Nclus2.all,file="f:/PMdist1.all")

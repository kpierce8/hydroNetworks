#years visited analysis
ffsallS <- ffsall[order(ffsall$szline_Near_UNIT_ID),]
ffsallS <- ffsall[order(ffsall[,23],ffsall[,4]),]

yrmed <- as.numeric(tapply(ffsallS$Year_,ffsallS$szline_Near_UNIT_ID,median,na.rm=TRUE))
yrmax <- as.numeric(tapply(ffsallS$Year_,ffsallS$szline_Near_UNIT_ID,max,na.rm=TRUE))
yrmin <- as.numeric(tapply(ffsallS$Year_,ffsallS$szline_Near_UNIT_ID,min,na.rm=TRUE))
ffsFISH<- ffsallS[ffsallS$Smelt_Ind == 1,]
yrmedF <- as.numeric(tapply(ffsFISH$Year_,ffsFISH$szline_Near_UNIT_ID,median,na.rm=TRUE))
yrmaxF <- as.numeric(tapply(ffsFISH$Year_,ffsFISH$szline_Near_UNIT_ID,max,na.rm=TRUE))
bset <- unique(ffsFISH[,23])
ffsmat <- ffsallS[ffsallS[,23] %in% bset,]
yrmaxFM <- as.numeric(tapply(ffsmat$Year_,ffsmat$szline_Near_UNIT_ID,max,na.rm=TRUE))
hist(yrmaxFM-yrmaxF)
boxplot(yrmaxFM-yrmedF,yrmax-yrmin)
lastvisit<- yrmaxFM[yrmaxFM-yrmaxF == 0]
hist(lastvisit)

###

sh_units <- unique(ffsFISH$szline_Near_UNIT_ID)

beach_dates<-tapply(ffsall$Survey_Date,ffsall[,23],unique)

dates_sums<-sapply(beach_dates,length)

beach_surv <- tapply(ffsall$Survey_Id,ffsall[,23],length)

plot(dates_sums+(rnorm(length(beach_surv))/5),beach_surv+(rnorm(length(beach_surv))/5),xlim=c(0,30),ylim=c(0,30),pch=20,cex=.5)

ind_date_beach <- aggregate(ffsall$Smelt_Ind,ffsall[,c(23,4)],sum)
ind_survs_beach2 <- aggregate(ffsall$Survey_Id,ffsall[,c(23,4)],length)
ind_surv_data <- data.frame(ind_date_beach,ind_survs_beach[,3])
names(ind_surv_data)[3:4]<- c("SumInd","NumSurv")
write.table(ind_surv_data,file="G:/surv_data_smelt.csv",sep=",")
plot(jitter(ind_surv_data$NumSurv),jitter(ind_surv_data$SumInd),pch=".")
                                   [,
surv_summary<- aggregate(ind_surv_data[,1],ind_surv_data[,c(4,3)],length)

ind_surv_data3 <- ind_surv_data[ind_surv_data[,4] <4,]

IS_numsum<-tapply(ind_surv_data3[,4],ind_surv_data3[,1],sum)
IS_INDsum<-tapply(ind_surv_data3[,3],ind_surv_data3[,1],sum)
IS_beach<-tapply(ind_surv_data3[,1],ind_surv_data3[,1],mean)
sum_beach<- data.frame(IS_beach,IS_numsum,IS_INDsum)
write.dbf(sum_beach,file="G:/sum_beach.dbf")

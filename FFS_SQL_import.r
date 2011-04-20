FFS <- odbcConnectAccess("g:/ForageFish/FF_SQL_Copy.mdb")

q1<- "TRANSFORM Count(EGG_STAGE_LUT.Egg_Stage_Desc) AS CountOfEgg_Stage_Desc"
q2<- "SELECT SURVEY_D.Survey_Id FROM EGG_STAGE_LUT INNER JOIN (((BEACH_D INNER JOIN (SURVEY_D INNER JOIN (SUBSTRATE_SAMPLE_D INNER JOIN EGG_COUNT_D ON (SUBSTRATE_SAMPLE_D.Substrate_Seq_Num = EGG_COUNT_D.Substrate_Seq_Num) AND (SUBSTRATE_SAMPLE_D.Survey_Id = EGG_COUNT_D.Survey_Id)) ON SURVEY_D.Survey_Id = SUBSTRATE_SAMPLE_D.Survey_Id) ON BEACH_D.Beach_Id = SURVEY_D.Beach_Id) INNER JOIN SPECIES_LUT ON EGG_COUNT_D.Species_LUT_Id = SPECIES_LUT.Species_LUT_Id) INNER JOIN EGG_STAGE_D ON EGG_COUNT_D.Egg_Count_Id = EGG_STAGE_D.Egg_Count_Id) ON EGG_STAGE_LUT.Egg_Stage_LUT_Id = EGG_STAGE_D.Egg_Stage_LUT_Id"
q3<- "WHERE (((EGG_COUNT_D.Species_LUT_Id)=1)) GROUP BY SURVEY_D.Survey_Id PIVOT EGG_STAGE_LUT.Egg_Stage_Desc;"

table1<-sqlQuery(FFS,paste(q1,q2,q3))

column_order <- c(1,4,7,9,3,5,6,2,10,8)

table1 <- table1[,column_order]
table1[is.na(table1)] <- 0
ni(table1)

table1<-transform(table1, live = apply(table1[,2:9],1,sum),eggs = apply(table1[,2:10],1,sum))
table1<-transform(table1, proplive = live / eggs)

q4<- "WHERE (((EGG_COUNT_D.Species_LUT_Id)=2)) GROUP BY SURVEY_D.Survey_Id PIVOT EGG_STAGE_LUT.Egg_Stage_Desc;"
table2<-sqlQuery(FFS,paste(q1,q2,q4))
column_order <- c(1,4,7,9,3,5,6,2,10,8)
table2 <- table2[,column_order]
ni(table2)
table2[is.na(table2)] <- 0
table2<-transform(table2, live = apply(table2[,2:9],1,sum),eggs = apply(table2[,2:10],1,sum))
table2<-transform(table2, proplive = live / eggs)

write.dbf(table1,"g:/foragefish/smelt_eggs.dbf")
write.dbf(table2,"g:/foragefish/lance_eggs.dbf")


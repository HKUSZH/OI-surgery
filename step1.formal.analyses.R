library(tidyverse)


allRecords<-readxl::read_excel("data/summary-table-Sept-7-2022.xlsx", sheet = "pkchen")
allRecords<-allRecords %>% filter(isSpinal=="NO") %>% filter(!grepl("HALO", nonOsteoType))

genetics<-readxl::read_excel("O:/B002_OI_Seq_reports/OI-seq-summary-Jan-9-2023.xlsx", sheet = "SZH-OI")

keyan<-readxl::read_excel("keyan-system/surgical_records_all_OI.xlsx", sheet = "surgeries")
patientInfo<-readxl::read_excel("P:/HKU-SZH/DrZhou/Oct21-2022/patientInfo.xlsx", sheet = "basicInfo")
perbone<-readxl::read_excel("healing-per-bone/per-bone-radiographic-Jan18-2023.xlsx", sheet = "Sheet2")
 grepl("骨折|复位", perbone$SurgName)
 grepl("石膏", perbone$SurgName)
 grepl("髋人字", perbone$SurgName, ignore.case=T)

 grepl("清创|debride", perbone$SurgName, ignore.case=T)

perbone<-perbone%>% filter(perbone$isSpinal=="NO")

OI<-allRecords[allRecords$NewNameNoDup%in%allRecords$NewNameNoDup[allRecords$isOIall], ]

setdiff(perbone$NewName, OI$NewNameNoDup)
OI[OI$NewNameNoDup%in%setdiff(OI$NewNameNoDup, perbone$NewName), ]

SCOLIOSIS0<-readxl::read_excel("P:/HKU-SZH/DrZhou/Oct21-2022/Dec23-OIwithscoliosisandgenetype_aug30V2.0.LLD.xlsx", sheet = "pkchen")
ind1<-match(OI$NewNameNoDup, SCOLIOSIS0$nameChi)
subtypes<-SCOLIOSIS0$Sillence[ind1]

Sillence<-readxl::read_excel("O:/B002_OI_Seq_reports/Sillence_subtyping/need.sillence-2023-Jan-10.xlsx", sheet = "Sheet1")
ind2<-match(OI$NewNameNoDup[which(is.na(ind1))], Sillence$proband)
subtypes[which(is.na(ind1))]<-Sillence$SillenceType[ind2]

as.matrix(sort(table(table(OI$NewNameNoDup)), decreasing=T))
OI[["Sillence"]]<-subtypes

table(is.na(subtypes))

setdiff(perbone$NewName, osteotomy$NewNameNoDup)
indperosteo<-match(perbone$NewName, OI$NewNameNoDup)

perbone[["Sillence"]]<-subtypes[indperosteo]

#osteotomy0<- OI %>% filter(isJiegu=="YES")
#osteotomy<-OI[OI$NewNameNoDup %in% osteotomy0$NewNameNoDup, ]
##################################################################
##################################################################

length(unique(osteotomy$NewNameNoDup))


osteotomy<-OI[OI$NewNameNoDup %in% perbone$NewName, ]

table(is.na(subtypes))

osteotomy$AgeInDays<-difftime(as.Date(osteotomy$ActualDate), as.Date(osteotomy$DOB2), unit="days")
osteotomy$AgeInYears<-as.numeric(osteotomy$AgeInDays)/365
LAge<-split(osteotomy$AgeInYears, osteotomy$NewNameNoDup)
LSillence<-sapply(split(osteotomy$Sillence, osteotomy$NewNameNoDup), unique)

AgeFirst<-sapply(LAge, min)
AgeLast<-sapply(LAge, max)

 table(names(AgeFirst)==names(AgeLast))
 table(names(AgeLast)==names(LSillence))

summary(lm(AgeFirst~LSillence))
summary(lm(AgeLast~LSillence))

aggregate(AgeFirst, by=list(LSillence), mean)
aggregate(AgeLast, by=list(LSillence), mean)

AgeStrFirst<-c(paste0(round(sapply(split(AgeFirst, LSillence), mean), 1), "+-", 
	round(sapply(split(AgeFirst, LSillence), sd), 1)), 
	paste0(round(mean(AgeFirst), 1), "+-", round(sd(AgeFirst), 1)))
AgeStrLast<-c(paste0(round(sapply(split(AgeLast, LSillence), mean), 1), "+-", 
	round(sapply(split(AgeLast, LSillence), sd), 1)),
	paste0(round(mean(AgeLast), 1), "+-", round(sd(AgeLast), 1)))

osteotomyUNQ<-osteotomy%>%group_by(NewNameNoDup) %>% slice_head()

tabSurg<-table(osteotomy$NewNameNoDup)
table(names(tabSurg)==osteotomyUNQ$NewNameNoDup)
tabSurg2<-table(tabSurg, osteotomyUNQ$Sillence)

c(paste0(round(sapply(split(tabSurg, osteotomyUNQ$Sillence), mean), 1), "+-", 
	round(sapply(split(tabSurg, osteotomyUNQ$Sillence), sd), 1)),
	paste0(round(mean(tabSurg), 1), "+-", 
	round(sd(tabSurg), 1)))

summary(lm(tabSurg~osteotomyUNQ$Sillence))

matSurg<-rbind(tabSurg2[seq(5), ],
	colSums(tabSurg2[-seq(5),]))

rowSums(matSurg)


tabSEX<- table(osteotomyUNQ$SEX, osteotomyUNQ$Sillence)
round(tabSEX[1,]/colSums(tabSEX)*100, 1)
table(osteotomyUNQ$SEX)

table(osteotomyUNQ$Sillence)

table(osteotomy$NumBones, osteotomy$Sillence)
summary(lm(osteotomy$NumBones~osteotomy$Sillence))
sapply(split(osteotomy$NumBones, osteotomy$Sillence), mean)
sapply(split(osteotomy$NumBones, osteotomy$Sillence), sd)
##################################################################
surgIntervals<-sapply(sapply(sapply(split(osteotomy$AgeInYears, osteotomy$NewNameNoDup), sort), diff), mean)
surgIntervals[is.na(surgIntervals)]<-NA
table(names(surgIntervals)==osteotomyUNQ$NewNameNoDup)
paste0(round(sapply(split(surgIntervals, osteotomyUNQ$Sillence), mean, na.rm=T), 1), "+-", 
	round(sapply(split(surgIntervals, osteotomyUNQ$Sillence), sd, na.rm=T), 1))

mean(surgIntervals, na.rm=T)
sd(surgIntervals, na.rm=T)

summary(lm(surgIntervals~osteotomyUNQ$Sillence))
##################################################################

setdiff(osteotomy$HospitalID, as.matrix(patientInfo)[,20])
table(osteotomy$HospitalID%in%as.matrix(patientInfo)[,20],
	osteotomy$NewNameNoDup%in%as.matrix(patientInfo)[,2])
setdiff(osteotomy$NewNameNoDup, as.matrix(patientInfo)[,2])

setdiff(genetics$ChiNameOnReport, osteotomy$NewNameNoDup)
setdiff(osteotomy$NewNameNoDup, genetics$ChiNameOnReport)

table(osteotomy$HospitalID%in%genetics$HospitalIDs,
	osteotomy$NameChi%in%genetics$ChiNameOnReport)
############
ind1<-match(osteotomyUNQ$HospitalID, genetics$HospitalIDs)
ind2<-match(osteotomyUNQ$NewNameNoDup, genetics$ChiNameOnReport)
 table(is.na(ind1), is.na(ind2), useNA="always")
ind1[which(is.na(ind1))]<-ind2[which(is.na(ind1))]
geneticsUnq<-genetics[ind1, ]
geneticsUnqNoNA<-genetics[setdiff(ind1, NA), ]

tabGeno<-table(geneticsUnq$AffectedGenes, osteotomyUNQ$Sillence)
tabGeno[order(rowSums(tabGeno), decreasing=T), ]
tabGeno2<-tabGeno[order(rowSums(tabGeno), decreasing=T), ]

tabGeno3<-rbind(tabGeno2[c("COL1A1", "COL1A2", "WNT1", "IFITM5", "FKBP10", "SERPINF1"), ],
	tabGeno2[grep(",", rownames(tabGeno2)), ],
	tabGeno2[c("No mutation"), ])
rownames(tabGeno3)[nrow(tabGeno3)]<-"No mutation"
tabGeno4<-colSums(tabGeno2[which(!rownames(tabGeno2)%in%rownames(tabGeno3)), ])
tabGeno5<- rbind(tabGeno2[c("COL1A1", "COL1A2", "WNT1", "IFITM5", "FKBP10", "SERPINF1"), ],
	colSums(tabGeno2[grep(",", rownames(tabGeno2)), ]),
	tabGeno4, tabGeno2[c("No mutation"), ])
##########################################################
indPatInf<-match(osteotomy$HospitalID, as.matrix(patientInfo)[,20])

diffDOB<-difftime(as.Date(osteotomy$DOB2), as.Date(as.matrix(patientInfo)[indPatInf,6]), units ="days")
diffDOB[diffDOB!=0]

table(osteotomy$SEX,
	as.matrix(patientInfo)[indPatInf,3], useNA="always")

osteotomy[which(diffDOB>200), ]
##########################################################
##########################################################
DateName<-paste0(substr(perbone$surgeryDate, 1, 12),"__", perbone$NewName)
oiDateName<-paste0(substr(OI$ActualDate, 1, 12),"__", OI$NewNameNoDup)

table(oiDateName %in% DateName)
table(DateName %in% oiDateName )
perbone[["DateName"]]<-DateName

perbone[DateName %in% setdiff(DateName, oiDateName ), ]

perbone2<-perbone[which(DateName %in% oiDateName ), ]

osteotomyPB<-OI[which(oiDateName%in%perbone2$DateName), ]
osteotomyPB<-OI[match(unique(perbone2$DateName), oiDateName), ]
perbone2[["isJiegu"]]<-OI$isJiegu[match(perbone2$DateName, oiDateName)]
perbone2[["isFracture"]]<-OI$isFracture[match(perbone2$DateName, oiDateName)]

table(perbone2$isFracture, grepl("骨折|复位", perbone2$SurgName))
perbone2[perbone2$isFracture=="NO"&grepl("骨折", perbone2$SurgName), ]

grepl("骨折|复位", perbone$SurgName)

##########################################################
##########################################################

tabPBSil<-table(osteotomyPB$Sillence)
paste0(tabPBSil[1:4], paste0(" (",round(tabPBSil/sum(tabPBSil)*100, 1) ,")"))


paste0(round(sapply(split(osteotomyPB$NumBones, osteotomyPB$Sillence), mean), 1),"+-",
	round(sapply(split(osteotomyPB$NumBones, osteotomyPB$Sillence), sd), 1))
mean(osteotomyPB$NumBones)
sd(osteotomyPB$NumBones)

tabJiegu<-table(osteotomyPB$isJiegu, osteotomyPB$Sillence, useNA="always")

osteotomyPBjg<-osteotomyPB %>% filter(osteotomyPB$isJiegu=="YES")
osteotomyPBnonjg<-osteotomyPB %>% filter(osteotomyPB$isJiegu!="YES")

tabFrac<-table(osteotomyPB$isFracture, osteotomyPB$Sillence, useNA="always")
tabPlate<-table(osteotomyPB$isPlateUsed, osteotomyPB$Sillence, useNA="always")
tabUnplan<-table(osteotomyPB$isUnplanned, osteotomyPB$Sillence, useNA="always")

paste0(tabFrac[2, 1:4], paste0(" (",round(tabFrac[2, 1:4]/tabPBSil*100, 1) ,")"))
paste0(tabUnplan[1, 1:4], paste0(" (",round(tabUnplan[1, 1:4]/tabPBSil*100, 1) ,")"))
paste0(tabPlate[2, 1:4], paste0(" (",round(tabPlate[2, 1:4]/tabPBSil*100, 1) ,")"))
paste0(tabJiegu[2, 1:4], paste0(" (",round(tabJiegu[2, 1:4]/tabPBSil*100, 1) ,")"))

osteotomyPB[which(is.na(osteotomyPB$isJiegu)), ]

table(osteotomyPB$Sillence)

table(osteotomyPBnonjg$Sillence)
revisionTypes<-unique(sort(unlist(strsplit(names(table(osteotomyPBnonjg$nonOsteoType)), ","))))
matRevi<-t(sapply(revisionTypes, function(revi){
	table(grepl(revi, osteotomyPBnonjg$nonOsteoType), osteotomyPBnonjg$Sillence)["TRUE",]
}))
matRevi2<-matRevi[order(rowSums(matRevi), decreasing=T), ]
matRevi3<-rbind(colSums(matRevi2[c(1,3), ]),
		colSums(matRevi2[c(4,5,9), ]),
		colSums(matRevi2[c(10,11), ]),
		matRevi2[c(2,6,7), ])
rownames(matRevi3)[1:3]<-c(paste0(rownames(matRevi2)[c(1,3)], collapse=","),
		paste0(rownames(matRevi2)[c(4,5,9)], collapse=","),
		paste0(rownames(matRevi2)[c(10,11)], collapse=","))
matRevi4<-matRevi3[order(rowSums(matRevi3), decreasing=T), ]
cbind(matRevi4, rowSums(matRevi4))
##########################################################
##########################################################

length(unique(perbone$NewName))

DateNameOsteo<-paste0(substr(osteotomy$ActualDate, 1, 12),"__", osteotomy$NewNameNoDup)
DateNameOsteoJB<-paste0(substr(osteotomyPBjg$ActualDate, 1, 12),"__", osteotomyPBjg$NewNameNoDup)
DateNameOsteoNonJB<-paste0(substr(osteotomyPBnonjg$ActualDate, 1, 12),"__", osteotomyPBnonjg$NewNameNoDup)


table(perbone2$DateName%in%DateNameOsteoJB)
table(DateNameOsteoJB%in%perbone2$DateName)
table(DateNameOsteoNonJB%in%perbone2$DateName)

perboneJG<-perbone2[perbone2$DateName%in%DateNameOsteoJB, ]

table(DateNameOsteoJB%in%perboneJG$DateName)
table(perboneJG$DateName%in%DateNameOsteoJB)

perBoneSillunq<-perboneJG$Sillence[match(sort(unique(perboneJG$DateName)), perboneJG$DateName)]
table(perBoneSillunq)

allBONES<-names(sort(table(perboneJG$whichBone), decreasing=T))
longBONES<-names(sort(table(perboneJG$whichBone), decreasing=T)[1:4])

Lbones<-split(perboneJG$whichBone, perboneJG$DateName)

Llongbones<-sapply(Lbones, function(x) sum(x%in%allBONES[1:8]))
lowerlimbs<-sapply(Lbones, function(x)sum(x%in%c(longBONES[1:4])))

matBones<-sapply(allBONES, function(thisbone){
	sapply(Lbones, function(x) sum(x%in%thisbone))
})
table(sort(unique(perboneJG$DateName))==rownames(matBones))
matBones2<-t(apply(matBones, 2, function(x){as.integer(table(x, perBoneSillunq)["1", ])}))
cbind(matBones2, rowSums(matBones2))

paste0(round(sapply(split(Llongbones, perBoneSillunq), mean), 1), "+-",
	round(sapply(split(Llongbones, perBoneSillunq), sd), 1))

summary(lm(Llongbones~perBoneSillunq))

paste0(table(perBoneSillunq), " (",round(table(perBoneSillunq)/length(perBoneSillunq)*100, 1) ,")")

matLimbs<-cbind(sapply(Lbones, function(x) sum(x%in%c("ZG","YG"))>0),
	sapply(Lbones, function(x) sum(x%in%c("ZJ","YJ"))>0),
	sapply(Lbones, function(x) sum(x%in%c("YGONG","ZGONG", "ZCHI",    "YCHI",    "YRAO",    "ZRAO",    "ZZHOU"))>0))
matLimbs2<-t(apply(matLimbs, 2, function(x){as.integer(table(x, perBoneSillunq)["TRUE", ])}))
rowSums(matLimbs2)/538

t(apply(matLimbs2, 1, function(x)round(x/table(perBoneSillunq)*100, 1)))

#####################################################################
perbone3<-perbone2 %>% filter(isJiegu=="YES")
perbone4<-perbone2 %>% filter(isJiegu!="YES")

table(perbone3$whichBone, perbone3$linkTo, useNA="always")
table(perbone4$whichBone, perbone4$linkTo, useNA="always")








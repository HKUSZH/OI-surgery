library(tidyverse)
library(ggplot2)



allRecords<-readxl::read_excel("data/summary-table-Sept-7-2022.xlsx", sheet = "pkchen")
allRecords<-allRecords %>% filter(isSpinal=="NO") %>% filter(!grepl("HALO", nonOsteoType))

genetics<-readxl::read_excel("O:/B002_OI_Seq_reports/OI-seq-summary-Jan-9-2023.xlsx", sheet = "SZH-OI")

keyan<-readxl::read_excel("keyan-system/surgical_records_all_OI.xlsx", sheet = "surgeries")
patientInfo<-readxl::read_excel("P:/HKU-SZH/DrZhou/Oct21-2022/patientInfo.xlsx", sheet = "basicInfo")

perbone<-readxl::read_excel("healing-per-bone/per-bone-radiographic-Feb13-2023.xlsx", sheet = "Sheet2")
ageSurgDays<-difftime(as.Date(perbone$surgeryDate), as.Date(perbone$DOB), unit="days")
perbone[["ageSurgDays"]]<-ageSurgDays
perbone[["ageSurgYrs"]]<-ageSurgDays/365

perbone[["DaysSurgToCutoff"]]<-difftime("2022-08-01", substr(perbone$surgeryDate, 1, 10), unit="days")

setdiff(perbone$NewName, as.matrix(patientInfo)[,2])
#as.matrix(patientInfo)[,6][match(perbone$NewName, as.matrix(patientInfo)[,2])]


####################################################

perbone[["nameBoneOrder"]]<-paste0(perbone$NewName, "-", perbone$boneOrder)
perbone[["nameLinkTo"]]<-paste0(perbone$NewName, "-", perbone$linkTo)

length(table(perbone[["nameBoneOrder"]]))
tail(sort(table(perbone[["nameBoneOrder"]])))


findID<-function(vecID, vecLinks, linki){
	if(is.na(linki)){return("compound")}
	if(linki==0){return("revisionSZH")}
	if(linki==-1){return("revisionNonSZH")}
	if(linki==-2){return("others")}
	return(findID(vecID, vecLinks, vecLinks[linki]))
}
Lident<-rep(list(),length(unique(perbone$NewName)))
for(i in 1:length(unique(perbone$NewName))){
	perbone_i<-perbone[which(perbone$NewName==unique(perbone$NewName)[i]), ]
	vecID<-perbone_i$boneOrder
	vecLinks<-as.integer(perbone_i$linkTo)
	IDi<-c()
	for(j in 1:length(vecLinks))
		IDi[j]<-findID(vecID, vecLinks, linki=vecLinks[j])
	Lident[[i]]<-IDi
}
surgGroups<-unlist(Lident)
surgGroups[perbone$linkTo==0]<-"indexSurg"
surgGroups[perbone$linkTo==-1]<-"indexRevisionNonSZH"

table(perbone$surgGroups, perbone$surgGroups)

####################################################
tab1<-table(perbone$surgReasons, perbone$linkTo)

notLimbs<-grepl("Halo", perbone$surgReasons) | perbone$isSpinal=="YES" | perbone$whichBone %in% c("noBone", "Patella", "Rib")
perboneLimbs<-perbone[!notLimbs, ]

isLimbsAndSZH<-!notLimbs & perbone$surgGroups!="revisionNonSZH" & !perbone$linkTo %in% c(-1)
perboneLimbsSZH<-perbone[isLimbsAndSZH, ]

table(surgGroups[isLimbsAndSZH], perboneLimbs$nameLinkTo %in% perboneIndexSurg$nameBoneOrder)


table(surgGroups[isLimbsAndSZH], perboneLimbs$nameLinkTo %in% perboneIndexSurg$nameBoneOrder)
perboneLimbs[surgGroups[isLimbsAndSZH]=="revisionSZH" & !perboneLimbs$nameLinkTo %in% perboneIndexSurg$nameBoneOrder, ]

table(perboneLimbs$surgReasons, perboneLimbs$surgGroups, useNA="always")
####################################################
perboneIndexSurg<-perboneLimbsSZH[which(perboneLimbsSZH$linkTo==0), ]
perboneRevisons<-perboneLimbsSZH[which(!perboneLimbsSZH$linkTo %in% c(0, -2)), ]

perboneIndexOsteo<-perboneIndexSurg[perboneIndexSurg$surgType=="osteotomy", ]
perboneIndexOsteo[["revised"]]<-perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo

#perboneLimbsSZH[["nameBoneOrder"]]<-paste0(perboneLimbsSZH$NewName, "-", perboneLimbsSZH$boneOrder)
#perboneLimbsSZH[["nameLinkTo"]]<-paste0(perboneLimbsSZH$NewName, "-", perboneLimbsSZH$linkTo)

table(perboneIndexSurg$nameLinkTo %in% perboneIndexSurg$nameBoneOrder)
table(perboneLimbsSZH$nameLinkTo %in% perboneIndexSurg$nameBoneOrder)
table(perboneIndexSurg$nameBoneOrder%in% perboneLimbsSZH$nameLinkTo )

table(perboneRevisons$nameLinkTo %in% perboneIndexSurg$nameBoneOrder)
table(perboneIndexSurg$nameBoneOrder %in% perboneRevisons$nameLinkTo)

perboneRevisons[!perboneRevisons$nameLinkTo %in% perboneIndexSurg$nameBoneOrder, ]
perboneRevisons$surgGroups[!perboneRevisons$nameLinkTo %in% perboneIndexSurg$nameBoneOrder]<-"RevisionOfRevision"

table(perboneRevisons$nameLinkTo[perboneRevisons$surgGroups=="RevisionOfRevision"] %in% perboneRevisons$nameBoneOrder[perboneRevisons$surgGroups!="RevisionOfRevision"])
perboneRevisons[perboneRevisons$surgGroups=="RevisionOfRevision", ][!perboneRevisons$nameLinkTo[perboneRevisons$surgGroups=="RevisionOfRevision"] %in% perboneRevisons$nameBoneOrder[perboneRevisons$surgGroups!="RevisionOfRevision"], ]

table(perboneIndexOsteo$surgReasons, perboneIndexOsteo$revised)
table(perboneIndexOsteo$surgReasons, perboneIndexOsteo$NailType)
table(perboneIndexOsteo$surgReasons, perboneIndexOsteo$whichBone)

table(perboneIndexOsteo$surgGroups, !is.na(perboneIndexOsteo[["FU1-RUST"]]))

####################################################
TABULATE<-function(var1="surgReasons", var2="linkTo"){
	tab1<-table(perboneLimbsSZH[[var1]], perboneLimbsSZH[[var2]])
	tab1[order(rowSums(tab1), decreasing=T), order(colSums(tab1), decreasing=T)]
}

tab1<-TABULATE()
tab2<-TABULATE(var2="NailType")
tab3<-TABULATE(var2="whichBone")
tab4<-TABULATE(var2="surgType")
tab5<-TABULATE(var2="numSites")
####################################################
####################################################
source("step10.helper.funcs.R")
#perboneLimbsSZH<-perbone
perboneRevisonsOfIndex<-perboneRevisons[perboneRevisons$nameLinkTo %in% perboneIndexOsteo$nameBoneOrder, ]
OBJ1<-perboneIndexOsteo
OBJ1<-perbone
OBJ1<-perboneRevisonsOfIndex

FUdate<-OBJ1[, grepl("FU.*-Date", colnames(OBJ1))]

matSurgDays<-sapply(1:ncol(FUdate), function(i){
		as.integer(difftime(as.Date(FUdate[[i]]), as.Date(OBJ1$surgeryDate),unit="days"))
	})
inddaysNonPos<-which(apply(matSurgDays, 1, function(x)sum(diff(x)<=0, na.rm=T))>0)
matSurgDays[inddaysNonPos, ]

lastFUday<-apply(matSurgDays[,-1], 1, max, na.rm=T)
FUrust<-OBJ1[, grepl("FU.*-RUST", colnames(OBJ1))]
FUrust2<-apply(as.matrix(FUrust), 2, function(x)gsub("\\(.*$", "", x))

sort(table(FUrust2))
table(unlist(strsplit(unique(as.vector(FUrust2)), ";")))

MatMinRUST<-apply(FUrust2, 2, function(x){
	Ltmp<-sapply(strsplit(x, ";"), as.numeric)
	sapply(Ltmp, min)
})
MatMinRUSTprox<-apply(FUrust2, 2, function(x){
	Ltmp<-sapply(strsplit(x, ";"), as.numeric)
	sapply(Ltmp, function(x)x[1])
})
MatMinRUSTmid<-t(apply(FUrust2, 1, function(x){
	Ltmp<-sapply(strsplit(x, ";"), as.numeric)
	if(sapply(Ltmp, length)[1]<3){
		return(rep(NA,11))
	}else{
		return(sapply(Ltmp, function(x){
			mean(x[-c(1, length(x))], na.rm=T)
		}))
	}
}))
MatMinRUSTdist<-apply(FUrust2, 2, function(x){
	Ltmp<-sapply(strsplit(x, ";"), as.numeric)
	sapply(Ltmp, function(x)x[length(x)])
})

daysFirst10<-sapply(1:nrow(MatMinRUST), function(i){
	x<-MatMinRUST[i, ]
	ind10<-which(x>=10)[1]
	matSurgDays[i, ind10]
})
daysLast9<-sapply(1:nrow(MatMinRUST), function(i){
	x<-MatMinRUST[i, ]
	ind10<-which(x>=10)[1]
	ind9<-max(which(x<10))
	indnothealed<-min(c(ind10-1, ind9), na.rm=T)
	matSurgDays[i, indnothealed]
})

maxHeal<-apply(MatMinRUST, 1, max, na.rm=T)
maxHeal[maxHeal<0] <- NA

HealingGroups<-rep("--", nrow(MatMinRUST))
HealingGroups[daysFirst10<90]<-"a. <90"
HealingGroups[daysFirst10>=90 & daysFirst10<180]<-"b. 90~180"
HealingGroups[daysFirst10>=180& daysFirst10<365]<-"c. 180~365"
HealingGroups[daysFirst10>=365& daysFirst10<730]<-"d. 365~730"
HealingGroups[daysFirst10>730]<-"e. >730"

surgWithinOneYear<-rep("--", nrow(MatMinRUST))
surgWithinOneYear[perboneIndexOsteo$DaysSurgToCutoff<365]<-"surgWithinOneYr"
surgWithinOneYear[perboneIndexOsteo$DaysSurgToCutoff>=365]<-"surgOverOneYr"

isRevised<-rep("--", nrow(MatMinRUST))
isRevised[perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo]<-"alreadyRevised"
isRevised[!perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo]<-"notYetRevised"

table(HealingGroups, paste0(surgWithinOneYear, "__", isRevised))

table(daysLast9>=365, HealingGroups)
table(daysLast9>180 & daysLast9<365, HealingGroups)
table(daysLast9<180, HealingGroups)

notHealingGroups<-rep("--", nrow(MatMinRUST))
notHealingGroups[daysLast9<90]<-"a. <90"
notHealingGroups[daysLast9>=90 & daysLast9<180]<-"b. 90~180"
notHealingGroups[daysLast9>=180& daysLast9<365]<-"c. 180~365"
notHealingGroups[daysLast9>=365& daysLast9<730]<-"d. 365~730"
notHealingGroups[daysLast9>730]<-"e. >730"

table(notHealingGroups, HealingGroups)
####################################################
perboneIndexOsteo[["isRevised"]]<-perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo
outcomes<-rep("--", nrow(MatMinRUST))
healedDays<-sapply(1:nrow(MatMinRUST), function(i){
	x<-MatMinRUST[i, -1]
	ind10<- which(x>=10)[1]
	matSurgDays[i,-1][ind10]
})
outcomes[healedDays<180]<-"healed"

table(lastFUday > 10 & lastFUday <90, outcomes)

IndObs<- lastFUday > 10 & lastFUday <90 & perboneIndexOsteo$DaysSurgToCutoff < 365
IndNF<- lastFUday > 10 & lastFUday <90 & perboneIndexOsteo$DaysSurgToCutoff >= 365

outcomes[IndObs & outcomes=="--"] <- "StillObserving"
outcomes[IndNF& outcomes=="--"] <- "LossFollowUp"
table(perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo, outcomes)
outcomes[perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo] <- "Revised"

table(perboneIndexOsteo$surgReasons, maxHeal >= 10)
table(perboneIndexOsteo$surgReasons, outcomes)

table(perboneLimbsSZH$surgType[which(is.na(maxHeal))])
####################################################
PLOT<-function(flag1, TYPE, isProxDist){
	if(TYPE=="Tibia"){flag2<-perboneIndexOsteo$whichBone %in% c("ZJ", "YJ")}
	if(TYPE=="Femur"){flag2<-perboneIndexOsteo$whichBone %in% c("ZG", "YG")}
	if(TYPE=="Femur and Tibia"){flag2<-perboneIndexOsteo$whichBone %in% c("ZJ", "YJ", "ZG", "YG")}
	if(TYPE=="Upper limbs"){flag2<-!perboneIndexOsteo$whichBone %in% c("ZJ", "YJ", "ZG", "YG")}

	indMultiSites<-flag1 & flag2
	datRUST<-data.frame(y=as.vector(MatMinRUST[indMultiSites, ]), 
		yprox=as.vector(MatMinRUSTprox[indMultiSites, ]), 
		ymid=as.vector(MatMinRUSTmid[indMultiSites, ]), 
		ydist=as.vector(MatMinRUSTdist[indMultiSites, ]), x=as.vector(matSurgDays[indMultiSites, ]))
	table(datRUST$yprox, datRUST$ydist)

	fit1<-loess(y~x, data=datRUST)
	pred1<-predict(fit1, data.frame(x = seq(1, 1500, 1)), se = TRUE)

	fit2<-loess(yprox~x, data=datRUST)
	pred2<-predict(fit2, data.frame(x = seq(1, 1500, 1)), se = TRUE)

	fit3<-loess(ydist~x, data=datRUST)
	pred3<-predict(fit3, data.frame(x = seq(1, 1500, 1)), se = TRUE)

	fit4<-loess(ymid~x, data=datRUST)
	pred4<-predict(fit4, data.frame(x = seq(1, 1500, 1)), se = TRUE)

	if(isProxDist){
		plot(as.vector(matSurgDays[indMultiSites, ]), as.vector(MatMinRUSTprox[indMultiSites, ]), xlab="days post op", ylab="RUST scores", main=paste0(TYPE, " (prox-vs-dist)"), ylim=c(4,12), xlim=c(0,1800), col=3)
		mtext("(for multi-site osteotomies only)")
		points(as.vector(matSurgDays[indMultiSites, ]), as.vector(MatMinRUSTdist[indMultiSites, ]), col=2)
		#lines(sort(fit1$x), fit1$fitted[order(fit1$x)])
		lines(seq(1, 1500, 1), pred2$fit, col=3, lwd=2)
		lines(seq(1, 1500, 1), pred3$fit, col=2, lwd=2)
		#lines(seq(1, 1500, 1), pred4$fit, col=4, lwd=2)
		legend("bottomright", lty=1, col=c(3,2), lwd=2, legend=c("proximal sites", "distal sites"))
	}

	if(!isProxDist){
		plot(as.vector(matSurgDays[indMultiSites, ]), as.vector(MatMinRUST[indMultiSites, ]), xlab="days post op", ylab="RUST scores(nminimum used for multi-site osteotomies)", main=TYPE, ylim=c(4,12), xlim=c(0,1800))
		mtext("(both single- and multi-site osteotomies used.)")
		lines(seq(1, 1500, 1), pred1$fit, col=2)
		lines(seq(1, 1500, 1), pred1$fit+pred1$se*1.96, lty=2, col=2)
		lines(seq(1, 1500, 1), pred1$fit-pred1$se*1.96, lty=2, col=2)

		legend("bottomright", legend=paste0("(n=", sum(indMultiSites), ")"))
	}
}
PLOT(perboneIndexOsteo$numSites>0, "Femur and Tibia", FALSE)
PLOT(perboneIndexOsteo$numSites>0, "Femur", FALSE)
PLOT(perboneIndexOsteo$numSites>0, "Tibia", FALSE)

PLOT(perboneIndexOsteo$numSites>1, "Femur and Tibia", TRUE)
PLOT(perboneIndexOsteo$numSites>1, "Upper limbs", TRUE)
PLOT(perboneIndexOsteo$numSites>1, "Femur", TRUE)
PLOT(perboneIndexOsteo$numSites>1, "Tibia", TRUE)







####################################################


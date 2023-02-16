library(tidyverse)
library(ggplot2)

source("step10.helper.funcs.R")


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

HEAL.perbone<-HEALING(perbone)
perbone[["notHealingGroups"]]<-HEAL.perbone$notHealingGroups
perbone[["HealingGroups"]]<-HEAL.perbone$HealingGroups
perbone[["HEAL"]]<-HEAL.perbone$GROUPS
table(perbone[["HEAL"]], perbone$HealingGroups)
table(perbone[["HEAL"]], perbone$notHealingGroups)

detach(HEAL.perbone)

attach(HEAL.perbone)

MatMinRUST[which(apply(apply(MatMinRUST, 1, diff), 2, min, na.rm=T)<= -2), ]
perbone[which(apply(apply(MatMinRUST, 1, diff), 2, min, na.rm=T)<= -2), ]


vecMin<-jitter(as.vector(t(MatMinRUST)))
vecDaysPostOp<-as.vector(t(matSurgDays))
vecID<-rep(1:nrow(perbone), rep(11, nrow(perbone)))
datRUST<-data.frame(vecMin, vecDaysPostOp, vecID)[!is.na(vecMin), ]
ggplot(datRUST, aes(x=vecDaysPostOp, y=vecMin, group=vecID)) +
	geom_point() +
	geom_line(aes(group=vecID))
####################################################

perbone[["nameBoneOrder"]]<-paste0(perbone$NewName, "-", perbone$boneOrder)
perbone[["nameLinkTo"]]<-paste0(perbone$NewName, "-", perbone$linkTo)

length(table(perbone[["nameBoneOrder"]]))
tail(sort(table(perbone[["nameBoneOrder"]])))

table(perbone$whichBone[match(perbone$nameLinkTo, perbone$nameBoneOrder)], perbone$whichBone)

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
perboneRevisonsOfIndex<-perboneRevisons[perboneRevisons$nameLinkTo %in% perboneIndexOsteo$nameBoneOrder, ]

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

table(perboneIndexOsteo$HEAL, perboneIndexOsteo$surgGroups)
table(perboneIndexOsteo$HEAL, perboneIndexOsteo$HealingGroups)
table(perboneIndexOsteo$HEAL, perboneIndexOsteo$notHealingGroups)

table(perboneIndexOsteo$HEAL,  ceiling(perboneIndexOsteo$DaysSurgToCutoff/365))
table(perboneRevisonsOfIndex$HEAL,  ceiling(perboneRevisonsOfIndex$DaysSurgToCutoff/365))

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
#perboneLimbsSZH<-perbone

HEAL.perboneIndexOsteo<-HEALING(perboneIndexOsteo)
HEAL.perboneRevisonsOfIndex<-HEALING(perboneRevisonsOfIndex)

table(perboneIndexOsteo$notHealingGroups, perboneIndexOsteo$HealingGroups)
table(perboneRevisonsOfIndex$notHealingGroups, perboneRevisonsOfIndex$HealingGroups)

detach(HEAL.perboneIndexOsteo)
detach(HEAL.perbone)
detach(HEAL.perboneRevisonsOfIndex)

attach(HEAL.perboneIndexOsteo)
OBJ1<-perboneIndexOsteo


attach(HEAL.perboneRevisonsOfIndex)
OBJ1<-perboneRevisonsOfIndex

surgAges<-as.integer(difftime(substr(OBJ1$surgeryDate, 1, 10), OBJ1$DOB, unit="days")/365)
surgAges[surgAges>25.5] <-25.5
ageGrp<-c("0-5yrs", "5-10yrs", "10-15yrs", "15-20yrs", "20-25yrs", ">25yrs")[ceiling(surgAges/5)]

table(OBJ1$HEAL, ageGrp)
table(OBJ1$HEAL, OBJ1$NailType)
table(OBJ1$HEAL, OBJ1$whichBone)
table(OBJ1$HEAL, OBJ1$surgReasons)

OBJ1[["ageGrp"]]<-ageGrp


set.seed(0)
vecMin<-jitter(as.vector(t(MatMinRUST)))
vecDaysPostOp<-jitter(as.vector(t(matSurgDays)))
vecID<-rep(1:nrow(OBJ1), rep(11, nrow(OBJ1)))
vecName<-rep(OBJ1$NewName, rep(11, nrow(OBJ1)))
vecHEAL<-rep(OBJ1$HEAL, rep(11, nrow(OBJ1)))

datRUST<-data.frame(vecMin, vecDaysPostOp, vecID, vecName, vecHEAL)[!is.na(vecMin), ]
ggplot(datRUST, aes(x=vecDaysPostOp, y=vecMin, group=vecID)) +
	geom_point() +
	geom_line(aes(group=vecID, color=vecName), size=1) +
	facet_wrap(~ vecHEAL, ncol=6) + theme(legend.position="none") + 
	xlim(c(-1, 730))

datRUST2<-datRUST[datRUST$vecHEAL==names(sort(table(datRUST$vecHEAL), decreasing=T))[1], ]
datRUST3<-datRUST[datRUST$vecHEAL %in% names(sort(table(datRUST$vecHEAL), decreasing=T))[1:8], ]

pdf("healing_curve_revisions.pdf", width=12)
	ggplot(datRUST3, aes(x=vecDaysPostOp, y=vecMin, group=vecHEAL)) +
#		geom_point(aes(color=vecHEAL)) +
		geom_smooth(data=datRUST3, aes(color=vecHEAL), size=2) +
		 ylim(c(4,12))

	ggplot(datRUST3, aes(x=vecDaysPostOp, y=vecMin, group=vecHEAL)) +
#		geom_point(aes(color=vecHEAL)) +
		geom_smooth(data=datRUST3, aes(color=vecHEAL), size=2) +
		xlim(c(-1, 730)) + ylim(c(4,12))

	ggplot(datRUST3, aes(x=vecDaysPostOp, y=vecMin, group=vecHEAL)) +
#		geom_point(aes(color=vecHEAL)) +
		geom_smooth(data=datRUST3, aes(color=vecHEAL), size=2) +
		xlim(c(-1, 365)) + ylim(c(4,12))

	ggplot(datRUST3, aes(x=vecDaysPostOp, y=vecMin, group=vecHEAL)) +
#		geom_point(aes(color=vecHEAL)) +
		geom_smooth(data=datRUST3, aes(color=vecHEAL), size=2) +
		xlim(c(-1, 182)) + ylim(c(4,12))
dev.off()

	facet_wrap(~ vecHEAL, ncol=6) + theme(legend.position="none") + 

ggplot(datRUST2, aes(x=vecDaysPostOp, y=vecMin, group=vecHEAL)) +
	geom_point(aes(color=vecHEAL)) +
	geom_line(aes(group=vecID, color=vecName), size=1) +
	geom_smooth(data=datRUST2) +
	theme(legend.position="none") + 
	xlim(c(-1, 730)) + ylim(c(0,12))


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


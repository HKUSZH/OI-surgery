HEALING<-function(OBJ1){
	#OBJ1<-perbone
	#OBJ1<-perboneRevisonsOfIndex

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
	surgWithinOneYear[OBJ1$DaysSurgToCutoff<365]<-"surgWithinOneYr"
	surgWithinOneYear[OBJ1$DaysSurgToCutoff>=365]<-"surgOverOneYr"

#	isRevised<-rep("--", nrow(MatMinRUST))
#	isRevised[perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo]<-"alreadyRevised"
#	isRevised[!perboneIndexOsteo$nameBoneOrder %in% perboneRevisons$nameLinkTo]<-"notYetRevised"

#	table(HealingGroups, paste0(surgWithinOneYear, "__", isRevised))

	table(daysLast9>=365, HealingGroups)
	table(daysLast9>180 & daysLast9<365, HealingGroups)
	table(daysLast9<180, HealingGroups)

	notHealingGroups<-rep("--", nrow(MatMinRUST))
	notHealingGroups[daysLast9<90]<-"a. <90"
	notHealingGroups[daysLast9>=90 & daysLast9<180]<-"b. 90~180"
	notHealingGroups[daysLast9>=180& daysLast9<365]<-"c. 180~365"
	notHealingGroups[daysLast9>=365& daysLast9<730]<-"d. 365~730"
	notHealingGroups[daysLast9>730]<-"e. >730"
	
	GROUPS<-rep("---", length(notHealingGroups))
	#GROUPS[OBJ1$DaysSurgToCutoff<365 & HealingGroups=="--"]<-"Observing (FU<12m and not healed yet)"
	GROUPS[notHealingGroups %in% c("a. <90") & HealingGroups=="--"]<-"f1. fast_to_notHealer (>0m)"
	GROUPS[notHealingGroups %in% c("b. 90~180") & HealingGroups=="--"]<-"f2. normal_to_notHealer (>3m)"
	GROUPS[notHealingGroups %in% c("c. 180~365") & HealingGroups=="--"]<-"f3. slow_to_notHealer (>6m)"
	GROUPS[notHealingGroups %in% c("d. 365~730") & HealingGroups=="--"]<-"f4. poor_to_notHealer (>12m)"

	GROUPS[notHealingGroups %in% c("--", "a. <90") & HealingGroups %in% c("a. <90")]<-"a. defly_fastHealer (<3m)"
	GROUPS[notHealingGroups %in% c("b. 90~180") & HealingGroups %in% c("b. 90~180")]<-"b1. defly_normalHealer (3~6m)"
	GROUPS[notHealingGroups %in% c("--", "a. <90") & HealingGroups %in% c("b. 90~180")]<-"b2. fast_normalHealer (3~6m)"
	GROUPS[notHealingGroups %in% c("c. 180~365") & HealingGroups %in% c("c. 180~365")]<-"c1. defly_slowHealer (6~12m)"
	GROUPS[notHealingGroups %in% c("b. 90~180") & HealingGroups %in% c("c. 180~365")]<-"c2. normal_slowHealer (3~12m)"
	GROUPS[notHealingGroups %in% c("--", "a. <90") & HealingGroups %in% c("c. 180~365")]<-"c3. fast_to_slowHealer (0~12m)"
	GROUPS[notHealingGroups %in% c("d. 365~730") & HealingGroups %in% c("d. 365~730")]<-"d1. defly_poorHealer (12~24m)"
	GROUPS[notHealingGroups %in% c("c. 180~365") & HealingGroups %in% c("d. 365~730")]<-"d2. slow_poorHealer (6~24m)"
	GROUPS[notHealingGroups %in% c("b. 90~180") & HealingGroups %in% c("d. 365~730")]<-"d3. normal_poorHealer (3~24m)"
	GROUPS[notHealingGroups %in% c("--", "a. <90") & HealingGroups %in% c("d. 365~730")]<-"d4. fast_to_poorHealer (0~24m)"
	GROUPS[notHealingGroups %in% c("e. >730")]<-"e1. defly_notHealer (>24m)"
	GROUPS[notHealingGroups %in% c("d. 365~730") & HealingGroups %in% c("e. >730")]<-"e2. poor_to_notHealer (12~24m)"
	GROUPS[notHealingGroups %in% c("c. 180~365") & HealingGroups %in% c("e. >730")]<-"e3. slow_to_notHealer (6~24m)"
	GROUPS[notHealingGroups %in% c("b. 90~180") & HealingGroups %in% c("e. >730")]<-"e4. normal_to_notHealer (3~24m)"
	GROUPS[notHealingGroups %in% c("--", "a. <90") & HealingGroups %in% c("e. >730")]<-"e5. fast_to_notHealer (0~24m)"

	LRES<-list(FUdate=FUdate,
		matSurgDays=matSurgDays,
		inddaysNonPos=inddaysNonPos,
		lastFUday=lastFUday,
		FUrust2=FUrust2,
		MatMinRUST=MatMinRUST,
		MatMinRUSTprox=MatMinRUSTprox,
		MatMinRUSTmid=MatMinRUSTmid,
		MatMinRUSTdist=MatMinRUSTdist,
		daysFirst10=daysFirst10,
		daysLast9=daysLast9,
		maxHeal=maxHeal,
		HealingGroups=HealingGroups,
		notHealingGroups=notHealingGroups, 
		GROUPS=GROUPS)
	return(LRES)
}

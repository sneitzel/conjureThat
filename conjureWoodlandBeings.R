conjureWoodlandBeings(fey)


setwd("C:/Users/Sarah/Downloads/D&D") #Edit this with the path to the folder where you stored the Fey CSV; make sure to use forward slashes

#Run this section of code to import the Fey CSV and set up the resulting object for the function

fey<-read.csv("Fey.csv", header=T, strip.white=T,
colClasses=c("character", "factor", "factor", "numeric", "list", "factor", "numeric", "list", "list", "list", "factor"))
for(i in 1:nrow(fey)){
	fey$Environments[[i]]<-strsplit(fey$Environments[[i]], ", ")[[1]]
	fey$Movement[[i]]<-strsplit(fey$Movement[[i]], ", ")[[1]]
	fey$Special[[i]]<-strsplit(fey$Special[[i]], ", ")[[1]]
	fey$Damage[[i]]<-strsplit(fey$Damage[[i]], ", ")[[1]]
}


#Run this section of code from here to end of script to define the Conjure Woodland Beings function

conjureWoodlandBeings<-function(df, spellLevel=4,
size=c("Tiny", "Small", "Medium", "Large", "Huge"),
cr=c(0.25, 0.5, 1, 2),
environment=c("Arctic", "Coastal", "Desert", "Forest", "Grassland", "Hill", "Mountain", "Swamp", "Underdark", "Underwater", "Urban"),
source=c("CC", "CFS", "MC", "MM", "MOF", "TOB", "VGM"),
movement=c("walk", "fly", "swim", "burrow", "climb", "glide"), 
type=c("animalesque", "elementalesque", "elf", "gnome", "humanesque", "monsteresque", "plantesque", "shapechanger", "unique"),
includeName="",
excludeName="",
includeAlignment="",
excludeAlignment="",
includeSpecial="",
excludeSpecial="",
minSpecial=0,
maxSpecial=15,
includeDamage="",
excludeDamage="none",
fullList=FALSE,
maxList=0,
randomization=0
){

	logicalSZ<-toupper(df$Size) %in% toupper(size)
	logicalCR<-toupper(df$Challenge) %in% toupper(cr)
	logicalSR<-toupper(df$Source) %in% toupper(source)
	logicalTY<-toupper(df$Type) %in% toupper(type)
	if(includeAlignment != ""){
		logicalIA<-toupper(df$Alignment) %in% toupper(includeAlignment)
	}else{
		logicalIA<-rep(T, nrow(df))
	}
	logicalAL<-toupper(df$Alignment) %in% toupper(excludeAlignment)
	logicalIN<-grepl(toupper(includeName), toupper(df$Name), fixed=T)
	if(excludeName != ""){
		logicalNM<-grepl(toupper(excludeName), toupper(df$Name), fixed=T)
	}else{
		logicalNM<-rep(F, nrow(df))
	}

	logicalList<-NULL
	logicalEN<-NULL
	logicalMV<-NULL
	logicalIS<-NULL
	logicalSP<-NULL
	logicalMS<-NULL
	logicalID<-NULL
	logicalDM<-NULL

	for(i in 1:nrow(df)){
		checkEN<-length(intersect(sapply(environment, toupper), sapply(df$Environment, toupper)[[i]]))
		checkMV<-length(intersect(sapply(movement, toupper), sapply(df$Movement, toupper)[[i]]))
		if(includeSpecial != ""){
			checkIS<-length(intersect(sapply(includeSpecial, toupper), sapply(df$Special, toupper)[[i]]))
		}else{
			checkIS<-1
		}
		checkSP<-length(intersect(sapply(excludeSpecial, toupper), sapply(df$Special, toupper)[[i]]))
		if(includeDamage != ""){
			checkID<-length(intersect(sapply(includeDamage, toupper), sapply(df$Damage, toupper)[[i]]))
		}else{
			checkID<-1
		}
		checkDM<-length(intersect(sapply(excludeDamage, toupper), sapply(df$Damage, toupper)[[i]]))
		if(logicalSZ[[i]] == T && logicalCR[[i]] == T && logicalSR[[i]] == T && logicalTY[[i]] == T
		&& logicalIN[[i]] == T && logicalNM[[i]] == F && logicalIA[[i]] == T && logicalAL[[i]] == F
		&& checkEN > 0 && checkMV > 0 && checkIS > 0
		&& length(df$Special[[i]]) >= minSpecial && length(df$Special[[i]]) <= maxSpecial
		&& checkSP == 0 && checkID > 0 && checkDM < length(df$Damage[[i]])){
			logicalList[[i]]<-T
		}else{
			logicalList[[i]]<-F
		}
		if(checkEN > 0){
			logicalEN[[i]]<-T
		}else{
			logicalEN[[i]]<-F
		}
		if(checkMV > 0){
			logicalMV[[i]]<-T
		}else{
			logicalMV[[i]]<-F
		}
		if(checkIS > 0){
			logicalIS[[i]]<-T
		}else{
			logicalIS[[i]]<-F
		}
		if(checkSP == 0){
			logicalSP[[i]]<-T
		}else{
			logicalSP[[i]]<-F
		}
		if(length(df$Special[[i]]) >= minSpecial && length(df$Special[[i]])){
			logicalMS[[i]]<-T
		}else{
			logicalMS[[i]]<-F
		}
		if(checkID > 0){
			logicalID[[i]]<-T
		}else{
			logicalID[[i]]<-F
		}
		if(checkDM < length(df$Damage[[i]])){
			logicalDM[[i]]<-T
		}else{
			logicalDM[[i]]<-F
		}
	}

	dfFin<-df[logicalList,]

	if(nrow(dfFin) < 1){
		sumSZ<-sum(logicalSZ, na.rm=T)
		sumCR<-sum(logicalCR, na.rm=T)
		sumSR<-sum(logicalSR, na.rm=T)
		sumTY<-sum(logicalTY, na.rm=T)
		sumIA<-sum(logicalIA, na.rm=T)
		sumAL<-nrow(df)-sum(logicalAL, na.rm=T)
		sumIN<-sum(logicalIN, na.rm=T)
		sumNM<-nrow(df)-sum(logicalNM, na.rm=T)
		sumEN<-sum(logicalEN, na.rm=T)
		sumMV<-sum(logicalMV, na.rm=T)
		sumIS<-sum(logicalIS, na.rm=T)
		sumSP<-sum(logicalSP, na.rm=T)
		sumMS<-sum(logicalMS, na.rm=T)
		sumID<-sum(logicalID, na.rm=T)
		sumDM<-sum(logicalDM, na.rm=T)
		allSum<-c(sumSZ, sumCR, sumSR, sumTY, sumIA, sumAL, sumIN, sumNM, sumEN, sumMV, sumIS, sumSP, sumMS, sumID, sumDM)
		allText<-c("size filter", "CR filter", "source filter",
		"type filter", "alignment inclusions", "alignment exclusions",
		"name inclusions", "name exclusions", "environment filter",
		"movement filter", "special ability inclusions",
		"special ability exclusions", "special ability limits",
		"damage inclusions", "damage exclusions")
		issue<-allText[[which.min(allSum)]]
		return(paste("No beasts fit your requirements, likely due to your", issue, "."))
	}else if(randomization != 0 && fullList == T){
		dfFin<-dfFin[order(sapply(dfFin$Special,length),dfFin$Challenge,sapply(dfFin$Damage,length),sapply(dfFin$Movement,length),sapply(dfFin$Environments,length),decreasing=c(F,T,F,F,F)),]
		ordered<-list()
		for(i in 1:nrow(dfFin)){
			if(i %% 2 == 0){
				ordered[[ceiling(nrow(dfFin)/2)+i-0.5*i]]<-dfFin[i,]
			}else{
				ordered[[ceiling(nrow(dfFin)/2)-i+ceiling(0.5*i)]]<-dfFin[i,]
			}
		}
		ordered<-do.call(rbind, ordered)
		if(maxList != 0 && maxList < nrow(ordered)){
			diff<-nrow(ordered)-maxList
			ordered<-ordered[-c(1:floor(diff/2),(nrow(ordered)-ceiling(diff/2)+1):nrow(ordered)),]
		}else if(maxList != 0){
			print("There were fewer options than your maximum.")
		}
		if(nrow(ordered) > 1){
			print(paste("You have ", nrow(ordered), " options:", sep=""))
		}else{
			print(paste("You have ", nrow(ordered), " option:", sep=""))
		}
		return(ordered)
	}else if(randomization != 0){
		dfFin<-dfFin[order(sapply(dfFin$Special,length),dfFin$Challenge,sapply(dfFin$Damage,length),sapply(dfFin$Movement,length),sapply(dfFin$Environments,length),decreasing=c(F,T,F,F,F)),]
		ordered<-list()
		for(i in 1:nrow(dfFin)){
			if(i %% 2 == 0){
				ordered[[ceiling(nrow(dfFin)/2)+i-0.5*i]]<-dfFin[i,]
			}else{
				ordered[[ceiling(nrow(dfFin)/2)-i+ceiling(0.5*i)]]<-dfFin[i,]
			}
		}
		random<-rbinom(1, nrow(dfFin), randomization)
		if(random < 1){
			random<-1
		}
		ordered<-do.call(rbind, ordered)
		crFin<-ordered$Challenge[random]
		crMult<-floor(spellLevel/2-1)
		if(crFin == 0 || crFin == 0.125 || crFin == 0.25){
			numFin<-8*crMult
		}else if(crFin == 0.5){
			numFin<-4*crMult
		}else if(crFin == 1){
			numFin<-2*crMult
		}else{
			numFin<-1*crMult
		}
		print(paste("You summon ", numFin, ":", sep=""))
		return(ordered[random,])
	}else if(fullList == T){
		if(maxList != 0 && maxList < nrow(dfFin)){
			dfFin<-dfFin[1:maxList,]
		}else if(maxList != 0){
			print("There were fewer options than your maximum.")
		}
		if(nrow(dfFin) > 1){
			print(paste("You have ", nrow(dfFin), " options:", sep=""))
		}else{
			print(paste("You have ", nrow(dfFin), " option:", sep=""))
		}
		return(dfFin)
	}else{
		random<-sample(1:nrow(dfFin), 1)
		crFin<-dfFin$Challenge[random]
		crMult<-floor(spellLevel/2-1)
		if(crFin == 0 || crFin == 0.125 || crFin == 0.25){
			numFin<-8*crMult
		}else if(crFin == 0.5){
			numFin<-4*crMult
		}else if(crFin == 1){
			numFin<-2*crMult
		}else{
			numFin<-1*crMult
		}
		print(paste("You summon ", numFin, ":", sep=""))
		return(dfFin[random,])
	}

}
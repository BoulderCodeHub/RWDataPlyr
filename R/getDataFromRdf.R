
processSlots <- function(slotsAnnualize, rdf, rdfName, traceSpace)
{
	ann <- slotsAnnualize[2]
	thresh <- as.numeric(slotsAnnualize[3])
	thresh[is.na(thresh)] = 1 # can use thresh as a scale also.  If it is not specified,
	# then multiply by 1.
	slot <- slotsAnnualize[1]

	if(!(slot %in% listSlots(rdf))){
		stop(paste("slot:", slot, "not found in rdf:", rdfName))
	}
	slot <- rdfSlotToMatrix(rdf, slot)
	
	startData <- strsplit(rdf$runs[[1]]$start, '-')[[1]]
	endData <- strsplit(rdf$runs[[1]]$end, '-')[[1]]

	yy <- seq(as.numeric(startData[1]), as.numeric(endData[1]), 1)
	
	# XXX
	# Need to add other summerization methods to this area
	# XXX
	# now summarize in some way
	if(ann == 'AnnMin'){
		slot <- apply(slot, 2, returnMinAnn) # minimum annual value
		rownames(slot) <- yy
	} else if(ann == 'EOCY'){
		slot <- slot[seq(12, nrow(slot), 12),] 
		slot[is.nan(slot)] <- 0
		slot <- slot * thresh
		rownames(slot) <- yy
	} else if(ann == 'AnnMax'){
		slot <- apply(slot, 2, returnMaxAnn) # minimum annual value
		rownames(slot) <- yy
	} else if(ann == 'AnnualSum'){
		slot <- sumMonth2Annual(slot,thresh)
		rownames(slot) <- yy
	} else if(ann == 'AnnMinLTE'){
		slot <- apply(slot, 2, returnMinAnn) # minimum annual value
		slot[slot <= thresh] <- 1
		slot[slot > thresh] <- 0
		slot <- slot * 100
		rownames(slot) <- yy
	} else if(ann == 'Monthly'){
		# XXX
		# need to update to use time series from zoo library
		yy <- matrix(t(matrix(rep(yy, 12),ncol = 12, byrow = F)), ncol = 1, byrow = F)
		rownames(slot) <- paste(rep(month.abb, nrow(slot)/12),yy,sep = '-')
		slot <- slot*thresh
	} else if(ann == 'WYMinLTE'){
		slot <- rbind(slot[1,],slot[1,],slot[1,],slot)
		slot <- slot[1:(nrow(slot)-3),]
		slot <- apply(slot, 2, returnMinAnn) # minimum annual value
		slot[slot <= thresh] <- 1
		slot[slot > thresh] <- 0
		slot <- slot * 100
		rownames(slot) <- yy
	} else{
		stop('invalid ann variable')
	}
	
	if(traceSpace){
		colnames(slot) <- paste('Trace',1:ncol(slot))
	} else{
		colnames(slot) <- paste('Trace',1:ncol(slot),sep = '')
	}
	if(ann != 'Monthly'){
		slot <- melt(slot, value.name = 'Value', varnames = c('Year','Trace'))
		slot <- cbind(slot, rep(paste(slotsAnnualize[1],ann,thresh,sep = '_'),nrow(slot)))
		colnames(slot)[ncol(slot)] <- 'Variable'
		slot <- subset(slot,select = c(Trace, Year, Variable, Value))
	} else{
		slot <- melt(slot, value.name = 'Value', varnames = c('Month','Trace'))
		mm <- simplify2array(strsplit(as.character(slot$Month), '-'))
		slot$Month <- mm[1,]
		slot$Variable <- rep(paste(slotsAnnualize[1],ann,thresh,sep = '_'),nrow(slot))
		slot$Year <- mm[2,]
		#colnames(slot)[(ncol(slot)-1):ncol(slot)] <- c('Variable','Year')
		slot <- subset(slot,select = c(Trace, Month, Year, Variable, Value))
	}
	slot
}

getSlots <- function(slotsAndRdf, scenPath, traceSpace)
{
	slotsAnnualize <- rbind(slotsAndRdf$slots, slotsAndRdf$annualize)
	rdf <- slotsAndRdf$rdf
	rdf <- read.rdf(paste(scenPath,'/',rdf,sep = ''))

	#print(paste('padding first three months of',slot,
			#'with January data to make Water Year based computation.\nPlease ensure this is an appropriate assumption'))
	#flush.console()
	allSlots <- apply(slotsAnnualize, 2, processSlots, rdf, slotsAndRdf$rdf, traceSpace)
	allSlots <- do.call(rbind, lapply(allSlots, function(X) X))
	allSlots
}

getAndProcessAllSlots <- function(scenPath, slotsAndRdf, tags, traceSpace)
{
	sPath <- scenPath[1]
	sName <- scenPath[2]
	zz <- lapply(slotsAndRdf, getSlots, sPath, traceSpace)

	allRes <- do.call(rbind, lapply(zz, function(X) X))
	nn = colnames(allRes)

	allRes$Scenario <- rep(sName, nrow(allRes))
	allRes <- subset(allRes, select=c('Scenario', nn))
	# tag for drought contingency modeling
	if(tags == 'DCP'){
		scenFull <- strsplit(sPath,'/')[[1]]
		scenFull <- scenFull[length(scenFull)]
		
		dryOrWet15 <- readDryWetFile(scenFull, '15To19')
		dryOrWet20 <-readDryWetFile(scenFull, '20To26')
		vulnT <- readVulnFile(scenFull)
		d15 <- match(allRes$Trace, names(dryOrWet15))
		d15 <- dryOrWet15[d15]
		d20 <- match(allRes$Trace, names(dryOrWet20))
		d20 <- dryOrWet20[d20]
		vv <- match(allRes$Trace, names(vulnT))
		vv <- vulnT[vv]

		allRes$DryOrWet2015To2019 <- simplify2array(d15)
		allRes$DryOrWet2020To2026 <- simplify2array(d20)
		allRes$VulnTrace <- simplify2array(vv)
		
		# create a separate column for supply, one for Scenario, and one for hyd-Scenario
		tmp <- as.character(allRes$Scenario)
		allRes$Hyd_Scen <- allRes$Scenario
		onlyScen <- getScenNoSupply(scenFull)
		onlyHyd <- getScenHydrology(scenFull)
		allRes$Scenario <- rep(onlyScen, nrow(allRes))
		allRes$Hydrology <- rep(onlyHyd, nrow(allRes))
	}
	allRes
}

getDataForAllScens <- function(scenFolders, scenNames, slotsAndRdf, scenPath, oFile, 
	tags = '', traceSpace = TRUE)
{

	scenPath = paste(scenPath,'/',scenFolders,sep = '')
	scen = cbind(scenPath, scenNames)
	zz = apply(scen, 1, getAndProcessAllSlots, slotsAndRdf, tags, traceSpace)
	zz <- do.call(rbind, lapply(zz, function(X) X))
	
	write.table(as.matrix(zz), oFile, row.names = F, sep = '\t')
}

# takes an existing data file, and edits it
changeDataFile <- function(iFile, varName, newVarName = varName, oldVals, newVals)
{
	zz = read.table(iFile, header = T)
	tmp = as.matrix(subset(zz, select = varName))
	for(i in 1:length(oldVals)){
		tmp[tmp == oldVals[i]] = newVals[i]
	}

	ii = match(varName, colnames(zz))
	colnames(zz)[ii] = newVarName
	zz[,ii] = tmp
	write.table(as.matrix(zz), iFile, row.names = F, sep = '\t')
} 
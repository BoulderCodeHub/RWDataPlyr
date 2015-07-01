#' Aggregate the slot data.
#' 
#' \code{processSlots} gets slot data from a rdf list and aggregates it as specified.
#' 
#' @param slotsAnnualize A string vector with three entries.  \code{slotsAnnualize[1]} is the
#' slot to process. \code{slotsAnnualize[2]} is the aggregation method to use. 
#' \code{slotsAnnualize[3]} is the threshold or scaling factor to use. \code{slotsAnnualize[4]}
#' is the variable name to use. If \code{slotsAnnualize[4]} is \code{NA}, then the variable
#' name is constructed as \code{slotsAnnualize[1]}_\code{slotsAnnualize[2]}_\code{slotsAnnualize[3]}.
#' @param rdf The rdf list returned by \code{\link{read.rdf}} to get the slot data from.  
#' @param rdfName String of the rdf name.
#' @return A data frame table with the aggregated slot data.
processSlots <- function(slotsAnnualize, rdf, rdfName)
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
		slot <- slot * thresh
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
	} else if(ann == 'AnnualRaw'){
    rownames(slot) <- yy
    slot <- slot*thresh
	} else{
		stop('Invalid aggregation method variable')
	}
	
	
	colnames(slot) <- 1:ncol(slot)
	
	if(ann != 'Monthly'){
		slot <- reshape2::melt(slot, value.name = 'Value', varnames = c('Year','Trace'))
		slot <- cbind(slot, rep(ifelse(is.na(slotsAnnualize[4]),paste(slotsAnnualize[1],ann,thresh,sep = '_'),
                                   slotsAnnualize[4]),nrow(slot)))
		colnames(slot)[ncol(slot)] <- 'Variable'
		slot <- subset(slot,select = c(Trace, Year, Variable, Value))
	} else{
		slot <- reshape2::melt(slot, value.name = 'Value', varnames = c('Month','Trace'))
		mm <- simplify2array(strsplit(as.character(slot$Month), '-'))
		slot$Month <- mm[1,]
		slot$Variable <- rep(ifelse(is.na(slotsAnnualize[4]),paste(slotsAnnualize[1],ann,thresh,sep = '_'),
                            slotsAnnualize[4]),nrow(slot))
		slot$Year <- mm[2,]
		#colnames(slot)[(ncol(slot)-1):ncol(slot)] <- c('Variable','Year')
		slot <- subset(slot,select = c(Trace, Month, Year, Variable, Value))
	}
	slot
}

#' Get and aggregate data from a single rdf file.
#' 
#' \code{getSlots} gets all of the slots contained in a single rdf file and aggregates them
#' as specified by the summary functions in \code{slotsAndRdf}. 
#' 
#' @inheritParams getAndProcessAllSlots
#' 
getSlots <- function(slotsAndRdf, scenPath)
{
	slotsAnnualize <- rbind(slotsAndRdf$slots, slotsAndRdf$annualize, slotsAndRdf$varNames)
	rdf <- slotsAndRdf$rdf
	rdf <- read.rdf(paste(scenPath,'/',rdf,sep = ''))

	#print(paste('padding first three months of',slot,
			#'with January data to make Water Year based computation.\nPlease ensure this is an appropriate assumption'))
	#flush.console()
	allSlots <- apply(slotsAnnualize, 2, processSlots, rdf, slotsAndRdf$rdf)
	allSlots <- do.call(rbind, lapply(allSlots, function(X) X))
	allSlots
}

#' Get and aggregate data from rdf file(s) for one scenario.
#' 
#' \code{getAndProcessAllSlots} gets data for a single scenario.  The slots from each
#' rdf are processed and aggregated together.
#' 
#' @param scenPath A relative or absolute path to the scenario folder.
#' @inheritParams getDataForAllScens
#' @seealso \code{\link{getDataForAllScens}}
getAndProcessAllSlots <- function(scenPath, slotsAndRdf)
{
	sPath <- scenPath[1]
	sName <- scenPath[2]
	zz <- lapply(slotsAndRdf, getSlots, sPath)

	allRes <- do.call(rbind, lapply(zz, function(X) X))
	nn = colnames(allRes)

	allRes$Scenario <- rep(sName, nrow(allRes))
	allRes <- subset(allRes, select=c('Scenario', nn))
	
	allRes
}

#' Get data from an rdf file(s) for multiple scenarios and save it as a data table.
#' 
#' \code{getDataForAllScens} gets slot data from multiple rdf files and for multiple scenarios.
#' The slot data can be aggregated in multiple ways (see \code{\link{createSlotAggList}}). Slot data is then converted to a molten data frame using \code{reshaphe2::melt}
#' and saved as a txt file for later use.
#' 
#' @param scenFolders A string vector containing the folder names (scenarios) that the rdf files
#' are saved in.
#' @param scenNames A string vector containing the scenario names.  This should be the same length
#' as \code{scenFolders}. The scenario names are used as attributes to the data in the "Scenario"
#' column.
#' @param slotAggList The slot aggregation list. A list containing the slots that will be 
#' imported and aggregated, the aggregation method(s) to use, and the rdf files that 
#' contain the slots. See \code{\link{createSlotAggList}}.
#' @param scenPath An absolute or relative path to the folder containing \code{scenFolders}.
#' @param oFile An absolute or relative path with the file name of the location the table will
#' be saved to.
#' @param retFile If \code{TRUE}, the data frame will be saved to \code{oFile} and returned. 
#' If \code{FALSE}, the data frame will only be saved to \code{oFile}.
#' @return If \code{retFile} is \code{TRUE}, a dataframe, otherwise nothing is returned.
#' 
#' @seealso \code{\link{createSlotAggList}}
getDataForAllScens <- function(scenFolders, scenNames, slotAggList, scenPath, oFile, retFile = FALSE)
{

	scenPath = paste(scenPath,'/',scenFolders,sep = '')
	scen = cbind(scenPath, scenNames)
	zz = apply(scen, 1, getAndProcessAllSlots, slotAggList)
	zz <- do.call(rbind, lapply(zz, function(X) X))
	
	write.table(as.matrix(zz), oFile, row.names = F, sep = '\t')
	if(retFile){
	  zz
	}
}


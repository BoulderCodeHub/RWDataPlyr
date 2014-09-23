# -----------------------------------------------------------------------------
# 								listSlots
# -----------------------------------------------------------------------------
#' List all slots contained in an rdf file (list returned by read.rdf)
#' @param rdf list returned by \code{\link{read.rdf}}
#' @return A vector of strings.  Each string is a slot contained in the rdf list.
#' @examples
#' zz <- read.rdf('KeySlots.rdf')
#' listSlots(zz)
listSlots <- function(rdf) 
{
  r <- names(rdf$runs[[1]]$objects)
  r
}

# ----------------------------------------------------------------------------
# **************************  rdfSlotToMatrix  *******************************
# ----------------------------------------------------------------------------

#' Takes a list created by \code{\link{read.rdf}} and converts the nested slot values over
#' multiple traces into a matrix with rows indexing through time and columns 
#' indexing over traces
#' @param rdf list returned by \code{\link{read.rdf}}
#' @param slot string of slot name that exists in \code{rdf} that will be converted to a matrix
#' @examples
#' zz <- read.rdf('KeySlots.rdf')
#' pe <- rdfSlotToMatrix(zz, 'Powell.Pool Elevation')
rdfSlotToMatrix <- function(rdf, slot)
{
	res = c()
  # check to see if the slot exists in the rdf, if it does not exit
  if(!(slot %in% listSlots(rdf)))
     stop(paste(slot,'not found in rdf:',deparse(substitute(rdf))))
     
	for(i in 1:as.numeric(rdf$meta$number_of_runs)){
		res = cbind(res, rdf$runs[[i]]$objects[[slot]]$values)
	}
	
	res
}

# ----------------------------------------------------------------------------
# **************************  sumMonth2Annual  *******************************
# ----------------------------------------------------------------------------
#' Sums monthly trace data into annual values.
#' 
#' @param matrixToSum The monthly trace data (months by traces) that will be summed.
#' @param multFactor A factor the annual sum will be multiplied by.  Can be used to convert from flow to volume.
#' @return The annual sums as a matrix (years by traces).
#' @examples
#' zz <- read.rdf('KeySlots.rdf')
#' zz <- rdfSlotToMatrix(zz, 'Powell.Outflow')
#' annualRelease <- sumMonth2Annual(zz) # returns in original units, e.g., acre-ft
#' annualRelease <- sumMonth2Annual(zz,.001) # returns in scaled units, e.g., kaf
sumMonth2Annual <- function(matrixToSum, multFactor = 1)
# the multiplying factor can be used if the matrix is in units of flow and the 
# result should be in units of volume, or to scale all results in another manor
{
	numTS = length(matrixToSum[,1])
	mySeq = seq(from = 0, to = numTS, by = 12)
	resMatrix = c()
	for(i in 1:(numTS/12)){
		resMatrix = rbind(resMatrix, apply(as.matrix(matrixToSum[(mySeq[i]+1):mySeq[i+1],]), 2, sum) * multFactor)
	}
	resMatrix
}


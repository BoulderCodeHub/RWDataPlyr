# -----------------------------------------------------------------------------
# 								listSlots
# -----------------------------------------------------------------------------
#' List all slots contained in an rdf file.
#' 
#' \code{listSlots} returns a list of all of the slots contained within the rdf list.
#' 
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
#' Get one slot out of an rdf list and put it in matrix form.
#' 
#' \code{rdfSlotToMatrix} Takes a list created by \code{\link{read.rdf}} and converts the nested slot values over
#' multiple traces into a matrix with rows indexing through time and columns 
#' indexing over traces.
#' 
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
#' \code{sumMonth2Annual} takes a matrix containing monthly data (months by traces), 
#' sum the monthly data into annual data.  Returns a years by traces matrix.
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

# ----------------------------------------------------------------------------
# **************************  rdfSlotToXTS  **********************************
# ----------------------------------------------------------------------------
#' Get one slot out of an rdf list and put it in an XTS array.
#' 
#' \code{rdfSlotToXTS} Takes a list created by \code{\link{read.rdf}} and convert 
#' the nested slot values over the multiple traces into an XTS array 
#' indexing over traces.
#' 
#' @param rdf list returned by \code{\link{read.rdf}}
#' @param slot string of slot name that exists in \code{rdf} that will be converted to a matrix
#' @examples
#' zz <- read.rdf('KeySlots.rdf')
#' pe <- rdfSlotToXTS(zz, 'Powell.Pool Elevation')
rdfSlotToXTS <- function(rdf, slot)
{
  # check to see if the slot exists in the rdf, if it does not exit
  if(!(slot %in% listSlots(rdf)))
    stop(paste(slot,'not found in rdf:',deparse(substitute(rdf))))
  # Get date-times from rdf
  tArray <- rdf$runs[[1]]$times
  # OPERATIONS IN ORDER OF EXECUTION
  # 1. rdfSlotToMatrix - read data for 'slot' string given 'rdf' file
  # 2. cbind - combine datetime and data arrays
  # 3. data.frame - define R dataframe for conversion to XTS
  # 4. read.zoo - convert dataframe to zoo matrix
  # 5. as.xts - convert zoo matrix to XTS
  # 6. Storage.mode() - convert char values in the XTS matrix to numeric
  rdfXTS <- as.xts(read.zoo(data.frame(cbind(tArray,rdfSlotToMatrix(rdf, slot)))))
  storage.mode(rdfXTS) <- "numeric"
  rdfXTS
}
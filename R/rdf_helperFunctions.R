# -----------------------------------------------------------------------------
# 								listSlots
# -----------------------------------------------------------------------------
#' list all slots contained in an rdf file (list returned by read.rdf)
#' @param rdf list returned by read.rdf
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

# Takes a list created by read.rdf and converts the nested slot values over
# multiple traces into a matrix with rows indexing through time and columns 
# indexing over runs
rdfSlotToMatrix <- function(rdf, slot)
{
	res = c()
	for(i in 1:as.numeric(rdf$meta$number_of_runs)){
		res = cbind(res, rdf$runs[[i]]$objects[[slot]]$values)
	}
	
	res
}

# ----------------------------------------------------------------------------
# **************************  sumMonth2Annual  *******************************
# ----------------------------------------------------------------------------

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


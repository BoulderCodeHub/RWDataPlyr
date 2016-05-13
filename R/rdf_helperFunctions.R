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
  # check to see if the slot exists in the rdf, if it does not exit error out
  if(!(slot %in% listSlots(rdf)))
     stop(paste(slot,'not found in rdf:',deparse(substitute(rdf))))
     
	nn <- as.numeric(rdf$meta$number_of_runs)
	res <- do.call(cbind, lapply(1:nn, function(xx) rdf$runs[[xx]]$objects[[slot]]$values))
	
	res
}


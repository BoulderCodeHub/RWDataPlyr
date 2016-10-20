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
#' listSlots(keyRdf)
#' 
#' @export
#' 
listSlots <- function(rdf) 
{
  warning('listSlots is depreciated. Use getSlotsInRdf instead.')
  r <- names(rdf$runs[[1]]$objects)
  r
}

# -----------------------------------------------------------------------------
# 								getSlotsInRdf
# -----------------------------------------------------------------------------
#' Returns all slots contained in an rdf file.
#' 
#' \code{getSlotsInRdf} returns a list of all of the slots contained within the rdf list.
#' 
#' @param rdf list returned by \code{\link{read.rdf}}
#' @return A vector of strings.  Each string is a slot contained in the rdf list.
#' @examples
#' getSlotsInRdf(keyRdf)
#' 
#' @export
#' 
getSlotsInRdf <- function(rdf)
{
  names(rdf$runs[[1]]$objects)
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
#' pe <- rdfSlotToMatrix(keyRdf, 'Powell.Pool Elevation')
#' 
#' @export
#' 
rdfSlotToMatrix <- function(rdf, slot)
{
  # check to see if the slot exists in the rdf, if it does not exit error out
  if(!(slot %in% getSlotsInRdf(rdf)))
     stop(paste(slot,'not found in rdf:',deparse(substitute(rdf))))
     
	nn <- as.numeric(rdf$meta$number_of_runs)
	res <- do.call(cbind, lapply(1:nn, function(xx) rdf$runs[[xx]]$objects[[slot]]$values))
	
	res
}

# ----------------------------------------------------------------------------
# **************************  getTimeSpan  *******************************
# ----------------------------------------------------------------------------
#' Returns the run period from an rdf. 
#' 
#' \code{getTimeSpan} Takes a list created by \code{\link{read.rdf}} and returns
#' the run period from the
#' 
#' @param rdf list returned by \code{\link{read.rdf}}
#' @return A named string vector with two elements. The first element, named 'start',
#' includes the start date of the simulation. The second element, named 'end',
#' includes the end date of the simulation.
#' @examples
#' getTimeSpan(keyRdf)
#' 
#' @export

getTimeSpan <- function(rdf)
{
  c('start' = rdf$runs[[1]]$start, 'end' = rdf$runs[[1]]$end)
}


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

# ----------------------------------------------------------------------------
# **************************  rdfSlotToXTS  **********************************
# ----------------------------------------------------------------------------
#' Get one slot out of an rdf list and put it in an XTS object
#' 
#' \code{rdfSlotToXTS} Takes a list created by \code{\link{read.rdf}} and convert 
#' the nested slot values over the multiple traces into an XTS array 
#' indexing over traces.
#' 
#' @param rdf list returned by \code{\link{read.rdf}}
#' @param slot string of slot name that exists in \code{rdf} that will be converted to a matrix
#' @return an XTS object with the selected slot data
#' @examples
#' \dontrun{
#' pe <- rdfSlotToXTS(keyRdf, 'Powell.Pool Elevation')
#' }

rdfSlotToXTS <- function(rdf, slot)
{
  # check to see if the slot exists in the rdf, if it does not exit
  if(!(slot %in% getSlotsInRdf(rdf)))
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
  rdfXTS <- xts::as.xts(zoo::read.zoo(data.frame(cbind(tArray,rdfSlotToMatrix(rdf, slot)))))
  storage.mode(rdfXTS) <- "numeric"
  runNames <- c()
  for (ithRun in c(1:as.numeric(rdf$meta$number_of_runs))){
    runNames <- c(runNames, paste('Trace',ithRun,sep=""))
  }
  names(rdfXTS) <- runNames
  rdfXTS
}

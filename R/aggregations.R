#' Calculate the flow-weighted average annual concentration.
#'
#' Given mass and flow at the monthly basis, the flow-weighted average annual concentration is 
#' returned. Mass and flow should be monthly data and should be for only a single trace, 
#' or for one year of one trace. Expect flow to be in acre-ft/month and mass to be in tons.
#' Return value will be in mg/L
#' 
#' @param mass A vector of one trace worth of data, or one year of one trace. Units should be in tons.
#' @param flow A vector of one trace worth of data, or one year of one trace. Units should be in acre-ft/month.
#' @return A vector of yearly data of the flow-weighted average annual concentration. Units will
#' be mg/L.
#' @examples
#' \dontrun{
#' flow <- rdfSlotToMatrix(rdf,'Powell.Outflow')
#' mass <- rdfSlotToMatrix(rdf,'Powell.Outflow Salt Mass')
#' fwaacT1 <- flowWeightedAvgAnnConc(mass[,1], flow[,1]) # repeat for other traces.
#' }
#' @seealso
#' \code{\link{rdfSlotToMatrix}}
#' 
#' @export
#' 
flowWeightedAvgAnnConc <- function(mass, flow)
{
	if(length(mass)%%12 != 0 | length(flow)%%12 != 0)
		stop('Data passed to flowWeightedAvgAnnConc is not divisible by 12')
		
	# move into a years x months matrix
	mass <- matrix(mass, ncol = 12, byrow = T)
	flow <- matrix(flow, ncol = 12, byrow = T)
	
	mass.annAvg <- apply(mass, 1, sum)/12
	flow.annAvg <- apply(flow, 1, sum)/12 # now essentially a volume
	
	conc <- mass.annAvg/flow.annAvg*735.466642
	
	return(conc)
}

returnMinAnn <- function(traceVal)
{
	tmp <- matrix(traceVal, ncol = 12, byrow = T)
	tmp <- apply(tmp, 1, min)
	return(tmp)
}

#' Find the minimum annual value for all years and traces.
#' 
#' @param xx A matrix (months by traces) such as that returned by \code{\link{rdfSlotToMatrix}}.
#' Will error if the number of rows in xx is not divisible by 12, i.e., the data must be monthly
#' for a full consecutive year.
#' @return A matrix (years by traces) with the maximum annual value for each year and trace.
#' @examples
#' pe <- rdfSlotToMatrix(keyRdf,'Powell.Pool Elevation')
#' peMax <- getMinAnnValue(pe)
#' @seealso
#' \code{\link{getMaxAnnValue}}
#' \code{\link{rdfSlotToMatrix}}
#' 
#' @export
#' 
getMinAnnValue <- function(xx)
{
	minAnn <- apply(xx, 2, returnMinAnn)
	return(minAnn)
}

returnMaxAnn <- function(traceVal)
{
	tmp <- matrix(traceVal, ncol = 12, byrow = T)
	tmp <- apply(tmp, 1, max)
	return(tmp)
}

#' Find the maximum annual value for all years and traces.
#' 
#' @param xx A matrix (months by traces) such as that returned by \code{\link{rdfSlotToMatrix}}.
#' Will error if the number of rows in xx is not divisible by 12, i.e., the data must be monthly
#' for a full consecutive year.
#' @return A matrix (years by traces) with the maximum annual value for each year and trace.
#' @examples
#' pe <- rdfSlotToMatrix(keyRdf,'Powell.Pool Elevation')
#' peMax <- getMaxAnnValue(pe)
#' @seealso
#' \code{\link{getMinAnnValue}}
#' \code{\link{rdfSlotToMatrix}}
#' 
#' @export
#' 
getMaxAnnValue <- function(xx)
{
	maxAnn <- apply(xx, 2, returnMaxAnn)
	return(maxAnn)
}

#' Sums monthly trace data into annual values.
#' 
#' \code{sumMonth2Annual} takes a matrix containing monthly data (months by traces), 
#' sum the monthly data into annual data.  Returns a years by traces matrix.
#' 
#' @param matrixToSum The monthly trace data (months by traces) that will be summed.
#' @param multFactor A factor the annual sum will be multiplied by.  Can be used to 
#' convert from flow to volume,  or to scale all results in another manor.
#' @return The annual sums as a matrix (years by traces).
#' @examples
#' zz <- rdfSlotToMatrix(keyRdf, 'Powell.Outflow')
#' annualTotVal <- sumMonth2Annual(zz) # returns in original units, e.g., acre-ft
#' annualTotVal <- sumMonth2Annual(zz,.001) # returns in scaled units, e.g., kaf
#' 
#' @export
#' 
sumMonth2Annual <- function(matrixToSum, multFactor = 1) {
  # take each column, make it a matrix of years by 
  res <- do.call(
    cbind, 
    lapply(
      1:ncol(matrixToSum), 
      function(xx) apply(matrix(matrixToSum[,xx],ncol = 12, byrow = T), 1, sum)
    )
  )
  colnames(res) <- colnames(matrixToSum)
  res * multFactor
}

#' Simple aggregation functions for monthly matrix data
#' 
#' `rwslot_annual_sum()` takes a matrix containing monthly data (months by 
#' traces), and sums the monthly data into annual data.  Returns a years by 
#' traces matrix.
#' 
#' @param rwslot A matrix (months by traces) such as that returned by 
#'   [rdf_get_slot()]. Function will error if the number of rows in `rwslot` is 
#'   not divisible by 12, i.e., the data must be monthly for a full consecutive 
#'   year.
#'   
#' @param multFactor A factor the annual sum will be multiplied by.  Can be used 
#'   to convert from flow to volume, or to scale all results in another manor.
#'   
#' @return `rwslot_annual_sum()` returns the annual sum as a matrix (years by 
#'   traces).
#'   
#' @seealso [rdf_get_slot()]
#' 
#' @examples
#' zz <- rdf_get_slot(keyRdf, 'Powell.Outflow')
#' 
#' # returns in original units, e.g., acre-ft
#' annualTotVal <- rwslot_annual_sum(zz)
#' 
#' # returns in scaled units, e.g., kaf
#' annualTotVal <- rwslot_annual_sum(zz, 0.001) 
#' 
#' @rdname rwslot_aggs
#' @export
rwslot_annual_sum <- function(rwslot, multFactor = 1) {
  # take each column, make it a matrix of years by 
  res <- do.call(
    cbind, 
    lapply(
      1:ncol(rwslot), 
      function(xx) apply(matrix(rwslot[,xx],ncol = 12, byrow = T), 1, sum)
    )
  )
  colnames(res) <- colnames(rwslot)
  res * multFactor
}

#' @describeIn rwslot_aggs Deprecated version of `rwslot_annual_sum()`.
#' @export
sumMonth2Annual <- function(rwslot, multFactor = 1) 
{
  .Deprecated("rwslot_annual_sum()")
  rwslot_annual_sum(rwslot, multFactor)
}

#' `getMinAnnValue()` finds the minimum annual value for all years and traces.
#' 
#' @inheritParams rwslot_annual_sum
#' 
#' @return `getMinAnnValue()` returns a matrix (years by traces) with the 
#'   maximum annual value for each year and trace.
#'   
#' @examples
#' pe <- rdf_get_slot(keyRdf,'Mead.Pool Elevation')
#' peMax <- getMinAnnValue(pe)
#' 
#' @rdname rwslot_aggs
#' @export
getMinAnnValue <- function(rwslot)
{
	apply(rwslot, 2, returnMinAnn)
}

returnMinAnn <- function(traceVal)
{
  tmp <- matrix(traceVal, ncol = 12, byrow = T)
  tmp <- apply(tmp, 1, min)
  return(tmp)
}

#' `getMaxAnnValue()` finds the maximum annual value for all years and traces.
#' 
#' @inheritParams rwslot_annual_sum
#' 
#' @return `getMaxAnnValue()` returns a matrix (years by traces) with the 
#'   maximum annual value for each year and trace.
#' @examples
#' pe <- rdf_get_slot(keyRdf,'Mead.Pool Elevation')
#' peMax <- getMaxAnnValue(pe)
#' 
#' @rdname rwslot_aggs
#' @export
getMaxAnnValue <- function(rwslot)
{
	maxAnn <- apply(rwslot, 2, returnMaxAnn)
	return(maxAnn)
}

returnMaxAnn <- function(traceVal)
{
  tmp <- matrix(traceVal, ncol = 12, byrow = T)
  tmp <- apply(tmp, 1, max)
  return(tmp)
}

#' `flowWeightedAvgAnnConc()` calculates the flow-weighted average annual 
#' concentration. Given mass and flow at the monthly basis, the flow-weighted 
#' average annual concentration is returned. Mass and flow should be monthly 
#' data and should be for only a single trace, or for one year of one trace. 
#' Expect flow to be in acre-ft/month and mass to be in tons. Return value will 
#' be in mg/L.
#' 
#' @param mass A vector of one trace worth of data, or one year of one trace. 
#'   Units should be in tons.
#' @param flow A vector of one trace worth of data, or one year of one trace. 
#'   Units should be in acre-ft/month.

#' @return `flowWeightedAvgAnnConc()` returns a vector of yearly data of the 
#'   flow-weighted average annual concentration. Units will be mg/L.
#'   
#' @examples
#' \dontrun{
#' flow <- rdf_get_slot(rdf,'Powell.Outflow')
#' mass <- rdf_get_slot(rdf,'Powell.Outflow Salt Mass')
#' # repeat for other traces as necessary:
#' fwaacT1 <- flowWeightedAvgAnnConc(mass[,1], flow[,1]) 
#' }
#' 
#' @rdname rwslot_aggs
#' @export
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

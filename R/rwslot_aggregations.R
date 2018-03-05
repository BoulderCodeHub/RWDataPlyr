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
      seq_len(ncol(rwslot)),
      function(xx) apply(matrix(rwslot[,xx],ncol = 12, byrow = TRUE), 1, sum)
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

#' `rwslot_annual_min()` finds the minimum annual value for all years and traces.
#' 
#' @inheritParams rwslot_annual_sum
#' 
#' @return `rwslot_annual_min()` returns a matrix (years by traces) with the 
#'   maximum annual value for each year and trace.
#'   
#' @examples
#' pe <- rdf_get_slot(keyRdf,'Mead.Pool Elevation')
#' peMax <- rwslot_annual_min(pe)
#' 
#' @rdname rwslot_aggs
#' @export
rwslot_annual_min <- function(rwslot)
{
	apply(rwslot, 2, trace_min_ann)
}

#' @export
#' @rdname rwslot_aggs
getMinAnnValue <- function(rwslot)
{
  .Deprecated("rwslot_annual_min()")
  rwslot_annual_min(rwslot)
}

trace_min_ann <- function(traceVal)
{
  tmp <- matrix(traceVal, ncol = 12, byrow = TRUE)
  apply(tmp, 1, min)
}

#' `rwslot_annual_max()` finds the maximum annual value for all years and traces.
#' 
#' @inheritParams rwslot_annual_sum
#' 
#' @return `rwslot_annual_max()` returns a matrix (years by traces) with the 
#'   maximum annual value for each year and trace.
#' @examples
#' pe <- rdf_get_slot(keyRdf,'Mead.Pool Elevation')
#' peMax <- rwslot_annual_max(pe)
#' 
#' @rdname rwslot_aggs
#' @export
rwslot_annual_max <- function(rwslot)
{
  apply(rwslot, 2, trace_max_ann)
}

#' @export
#' @rdname rwslot_aggs
getMaxAnnValue <- function(rwslot)
{
  .Deprecated("rwslot_annual_max()")
  rwslot_annual_max(rwslot)
}

trace_max_ann <- function(traceVal)
{
  tmp <- matrix(traceVal, ncol = 12, byrow = TRUE)
  
  apply(tmp, 1, max)
}

#' `rwslot_fwaac()` calculates the flow-weighted average annual 
#' concentration (fwaac). Given mass and flow at the monthly basis, the 
#' flow-weighted average annual concentration is computed. `mass` and `flow` 
#' should be monthly data. `rwslot_fwaac()` expects flow to be in acre-ft/month 
#' and mass to be in tons; however, there are no checks to ensure this is true.
#' Return value will be in mg/L.
#' 
#' @param mass A matrix (months by traces), such as that returned by 
#'   [rdf_get_slot()], of mass in tons.
#' @param flow A matrix (months by traces), such as that returned by 
#'   [rdf_get_slot()], of flow in acre-ft/month.

#' @return `rwslot_fwaac()` returns a matrix of yearly data of the 
#'   flow-weighted average annual concentration. Units are mg/L.
#'   
#' @examples
#' \dontrun{
#' flow <- rdf_get_slot(rdf,'Powell.Outflow')
#' mass <- rdf_get_slot(rdf,'Powell.Outflow Salt Mass')
#' # repeat for other traces as necessary:
#' fwaac <- rwslot_fwaac(mass, flow) 
#' }
#' 
#' @rdname rwslot_aggs
#' @export
rwslot_fwaac <- function(mass, flow)
{
  if (!identical(dim(mass), dim(flow))) {
    stop("In `rwslot_fwaac()`, the dimensions of `flow` and `mass` must match.")
  }
  
  if (!is_full_monthly(nrow(mass))) {
    stop("In `rwslot_fwaac()`, `mass` and `flow` are not divisible by 12.")
  }
  
  nyear <- nrow(mass)/12
  
  vapply(
    seq_len(ncol(mass)), 
    function(x) trace_fwaac(mass[,x], flow[,x]), 
    FUN.VALUE = numeric(nyear)
  )
}

trace_fwaac <- function(mass, flow)
{
  if (!is_full_monthly(length(mass)) || !is_full_monthly(length(flow))) {
    stop('Data passed to `trace_fwaac()` is not divisible by 12')
  }
  
  # move into a years x months matrix
  mass <- matrix(mass, ncol = 12, byrow = TRUE)
  flow <- matrix(flow, ncol = 12, byrow = TRUE)
  
  mass.annAvg <- apply(mass, 1, sum)/12
  flow.annAvg <- apply(flow, 1, sum)/12 # now essentially a volume
  
  conc <- mass.annAvg/flow.annAvg*735.466642
  
  conc
}

#' is_full_monthly assumes that data is monthly if it is divisible by 12
#' @noRd
is_full_monthly <- function(x)
{
  x%%12 == 0
}

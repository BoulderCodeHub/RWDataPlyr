
########################################################################################

# RETURNS XTS WATER YEAR ENDPOINTS FOR AGGREGATION CALCULATIONS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
getWyEndpoints <- function(rdfXTS)
{
  tVals <- index(rdfXTS[.indexmon(rdfXTS) %in% 8]) 
  ep <-  c(0, which(index(rdfXTS) %in% tVals)) 
  return(ep)
}

# RETURNS XTS CALENDAR YEAR ENDPOINTS FOR AGGREGATION CALCULATIONS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
getCyEndpoints <- function(rdfXTS)
{
  tVals <- index(rdfXTS[.indexmon(rdfXTS) %in% 11]) 
  ep <-  c(0, which(index(rdfXTS) %in% tVals)) 
  return(ep)
}

# RETURNS VALUES FOR A MONTH FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# month <- desired month from 1 to 12
getTraceMonthVal <- function(rdfXTS, month)
{
  # CHECK FOR A VALID MONTH
  if (month <= 0 || month > 12)
    stop(paste(month, " is not a valid month. Use a month from 1 to 12", sep=""))
  # GET VALUES OF EACH TRACE BY MONTH INDEX
  outXTS <- rdfXTS[.indexmon(rdfXTS) == (month - 1)]
  return(outXTS)
}

# RETURNS THE AVERAGE ANNUAL VALUE FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# yearType <- 'WY' or 'CY' for Water Year (Oct-1 to Sep-30) or Calendar Year (Jan-1 to Dec-31)
getTraceAvg <- function(rdfXTS, yearType)
{
  if (yearType == "WY")
    ep <- getWyEndpoints(rdfXTS)
  else
    ep <- getCyEndpoints(rdfXTS)
  # GET CY ANNUAL AVERAGE BY TRACE 
  outXTS <- period.apply(rdfXTS, ep, mean)
  return(outXTS)
}

# RETURNS THE ANNUAL SUM FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# yearType <- 'WY' or 'CY' for Water Year (Oct-1 to Sep-30) or Calendar Year (Jan-1 to Dec-31)
getTraceSum <- function(rdfXTS, yearType)
{
  if (yearType == "WY")
    ep <- getWyEndpoints(rdfXTS)
  else
    ep <- getCyEndpoints(rdfXTS)
  # GET CY ANNUAL SUMS BY TRACE 
  outXTS <- period.apply(rdfXTS, ep, colSums)
  return(outXTS)
}

# RETURNS THE ANNUAL MIN FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# yearType <- 'WY' or 'CY' for Water Year (Oct-1 to Sep-30) or Calendar Year (Jan-1 to Dec-31)
getTraceMin <- function(rdfXTS, yearType)
{
  if (yearType == "WY")
    ep <- getWyEndpoints(rdfXTS)
  else
    ep <- getCyEndpoints(rdfXTS)
  # GET CY ANNUAL MIN BY TRACE 
  outXTS <- period.apply(rdfXTS, ep, function(x) apply(x, 2, min))
  return(outXTS)
}

# RETURNS THE ANNUAL MAX FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# yearType <- 'WY' or 'CY' for Water Year (Oct-1 to Sep-30) or Calendar Year (Jan-1 to Dec-31)
getTraceMax <- function(rdfXTS, yearType)
{
  if (yearType == "WY")
    ep <- getWyEndpoints(rdfXTS)
  else
    ep <- getCyEndpoints(rdfXTS)
  # GET CY ANNUAL MAX BY TRACE 
  outXTS <- apply.yearly(rdfXTS, ep, function(x) apply(x, 2, max))
  return(outXTS)
}

# RETURNS THE VALUES FOR THE ENTIRE ARRAY AT DEFINED EXCEEDANCE LEVELS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# pctlLevels <- c(0.10, 0.50, 0.90) for the 10-50-90 exceedance levels
getArrayPctl <- function(rdfXTS, pctlLevels)
{
  # DEFINE PERCENTILE VALUES OF INTEREST
  toPctls <- function(rdfXTS) quantile(rdfXTS, pctlLevels)
  # DEFINE TIME STEP OF THE INPUT DATA
  tStep <- paste(periodicity(rdfXTS)$label,"s",sep="")
  # GET DATA INDICES
  ep <- endpoints(rdfXTS,tStep)
  # PERFORM STATS
  outXTS <- period.apply(rdfXTS, ep, toPctls)
  return(outXTS)
}

# RETURNS A PERCENT OF TRACES THAT MEET A CONDITION FOR THE ENTIRE ARRAY
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# valueIn <- numeric to make the comparison against
# comparison <- GT for greater-than or LT for less-than comparison against valueIn
getArrayThresholdExceedance <- function(rdfXTS, valueIn, comparison)
{
  # DETERMINE COMPARISON TYPE AND GET A BOOLEAN ARRAY OF VALUES THAT MEET THE THRESHOLD
  if (comparison == "GT")
    boolArray <- rdfXTS > valueIn  
  else if (comparison == "LT")
    boolArray <- rdfXTS < valueIn 
  else
    stop(paste(comparison, " is not a valid input. Use GT for greater than or LT for less than", sep=""))
  # GET A COUNT OF TRUE VALUES AT EACH COLUMN FOR EACH ROW
  trueCount <- xts(rowSums(boolArray),index(boolArray))
  # GET THE TOTAL COUNT OF COLUMNS
  totalCount <- length(dimnames(boolArray)[[2]])
  # RETURN PERCENTAGE OF VALUES THAT MEET THE COMPARISON TYPE
  return(trueCount/totalCount * 100)
}


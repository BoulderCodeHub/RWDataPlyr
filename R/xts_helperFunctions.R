

# RETURNS THE VALUES FOR THE ENTIRE ARRAY AT DEFINED EXCEEDANCE LEVELS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# pctlLevels <- c(0.10, 0.50, 0.90) for the 10-50-90 exceedance levels
getArrayPercentilesByMonth <- function(rdfXTS, pctlLevels)
{
  # DEFINE PERCENTILE VALUES OF INTEREST
  toPctls <- function(rdfXTS) quantile(rdfXTS, pctlLevels)
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY MONTH
  montPctlXts <- apply.monthly(rdfXTS,toPctls)
  return(montPctlXts)
}

# RETURNS THE EOCY VALUE FOR THE ENTIRE ARRAY AT DEFINED EXCEEDANCE LEVELS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# pctlLevels <- c(0.10, 0.50, 0.90) for the 10-50-90 exceedance levels
getArrayPercentilesByEOCY <- function(rdfXTS, pctlLevels)
{
  # DEFINE PERCENTILE VALUES OF INTEREST
  toPctls <- function(rdfXTS) quantile(rdfXTS, pctlLevels)
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY EOCY
  eocyPctlXts <- apply.yearly(rdfXTS[endpoints(rdfXTS, on="years", k=1)],toPctls)
  return(eocyPctlXts)
}

# RETURNS VALUES FOR A MONTH FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# month <- desired month from 1 to 12
getTraceValuesByMonth <- function(rdfXTS, month)
{
  # CHECK FOR A VALID MONTH
  if (month <= 0 || month > 12)
    stop(paste(month, " is not a valid month. Use a month from 1 to 12", sep=""))
  # GET VALUES OF EACH TRACE BY MONTH INDEX
  traceValsXts <- rdfXTS[.indexmon(rdfXTS) == (month - 1)]
  return(traceValsXts)
}

# RETURNS THE AVERAGE ANNUAL VALUE FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
getTraceAnnualAvgByCY <- function(rdfXTS)
{
  # GET CY ANNUAL AVERAGE BY TRACE 
  annlSumsXts <- apply.yearly(rdfXTS,mean)*12
  return(annlSumsXts)
}

# RETURNS THE ANNUAL SUM FOR EACH TRACE
# rdfXTS <- xts array returned by rdfSlotToMatrix()
getTraceAnnualSumsByCY <- function(rdfXTS)
{
  # GET CY ANNUAL SUMS BY TRACE 
  annlSumsXts <- apply.yearly(rdfXTS,colSums)
  return(annlSumsXts)
}

# RETURNS ENTIRE ARRAY'S ANNUAL SUM FOR EACH TRACE AT DEFINED EXCEEDANCE LEVELS
# rdfXTS <- xts array returned by rdfSlotToMatrix()
# pctlLevels <- c(0.10, 0.50, 0.90) for the 10-50-90 exceedance levels
getArrayPercentilesByAnnualSumsByEOCY <- function(rdfXTS, pctlLevels)
{
  # DEFINE PERCENTILE VALUES OF INTEREST
  toPctls <- function(rdfXTS) quantile(rdfXTS, pctlLevels)
  # GET CY ANNUAL SUMS BY TRACE 
  annlSumsXts <- getTraceAnnualSumsByCY(rdfXTS)
  # GET PERCENTILE VALUES OF ENTIRE ARRAY BY CY ANNUAL SUMS 
  annlSumsPctlXts <- apply.yearly(annlSumsXts,toPctls)
  return(annlSumsPctlXts)
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


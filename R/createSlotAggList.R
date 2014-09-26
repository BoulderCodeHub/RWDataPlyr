#' Creates a list for use by \code{\link{getDataForAllScens}}.
#' 
#' \code{createSlotAggList} creates a list that specifies for each slot: which rdf file to find each slot in, 
#' how to aggregate and process the slot data, and any thresholds or scaling factors. The function
#' can either read in a csv file or start from an Nx4 string matrix.
#' 
#' The csv file and the matrix should be in the form of an Nx4 matrix.  Each row is a single
#' slot, aggregation, and threshold combination.  If you want to compare a single slot value to 
#' multiple thresholds, it needs to have one row for each threshold. The first column is the 
#' rdf the slot is found in.  The second column is the slot name.  The third column is the
#' aggregation method that will be applied to the slot (see below for a list of the aggregation
#' methods).  The fourth column is a scaling factor or threshold to compare the slot data to. 
#' Below is an example table. All values should be strings except for \code{NA}.
#' 
#' \tabular{cccc}{
#' \strong{rdf} \tab \strong{Slot} \tab \strong{Aggregation Method} \tab 
#' \strong{Threshold or Scaling Factor}\cr
#' 'KeySlots.rdf' \tab 'Powell.Pool Elevation' \tab 'EOCY' \tab NA\cr
#' 'KeySlots.rdf' \tab 'Mead.Pool Elevation' \tab 'AnnMinLTE' \tab '1100'\cr
#' 'KeySlots.rdf' \tab 'Mead.Pool Elevation' \tab 'AnnMinLTE' \tab '1060'\cr
#' 'Other.rdf' \tab 'Powell.Outflow' \tab 'AnnualSum' \tab '0.001'\cr
#' }
#' 
#' The above table lists each slot, the rdf the slot is saved in, the summary function, and 
#' the threshold to be used to scale the data by or compare the data to. The threshold and
#' scaling factors are described in more detail below. For example, the first row will result
#' in compiling all end-of-December values for Powell's pool elevation.  The data will not be 
#' scaled, and is found in KeySlots.rdf. The second row will find the annual minimum Mead pool
#' elevation and see if it is less than or equal to 1,100' feet in the second line and less
#' than or equal to 1,060' feet in the third row. To scale the data by a value less than 1, 
#' use decimals rather than fractions, as shown in the fourth row.
#' 
#' The available aggregation methods are as follows. The behaviour of the "Threshold or scaling
#' factor are described and a bold \strong{"Threshold"} or \strong{"Scaled"} indicate 
#' which is used by the aggregation method. For scaling factors, a value of "NA" will not scale
#' the data.
#' \describe{
#'  \item{'EOCY'}{End-of-calendar year values are reported and \strong{scaled}. Any values that are NaNs are changed
#'  to 0s.}
#'  \item{'AnnMax'}{Returns the maximum annual \strong{scaled} value.}
#'  \item{'AnnSum'}{Returns the annual \strong{scaled} sum.}
#'  \item{'AnnMinLTE'}{Checks to see if the annual minimum value is less than or equal to a 
#'  \strong{threshold.} Returns 100 if it is less than or equal to the \strong{threshold} and 0
#'  otherwise.}
#'  \item{'WYMinLTE'}{Checks to see if the minimum water year value is less than or equal to a 
#'  \strong{threshold.} Returns 100 if it is less than or equal to the \strong{threshold} and 0
#'  otherwise. The water year is defined as October through September of the next year. For the
#'  first year, only January through September are evaluated as RiveWare does not typcially 
#'  export pre-simulation data.}
#'  \item{'Monthly'}{Returns the monthly \strong{scaled} data.}
#' }
#' 
#' @param iData Either an Nx4 string matrix or a string with an absolute or relative path to a csv
#' file.
#' @return A multi level list.
#' @examples
#' createSlotAggList('test.csv')
#' @seealso \code{\link{getDataForAllScens}}
createSlotAggList <- function(iData)
{
  if(!is.matrix(iData)){
    if(!file.exists(iData)){
      stop(paste(iData,'does not exist.'))
    }
    
    iData <- as.matrix(read.csv(iData,header = F))
  }
  
  sl <- list() #slot list
  # create one entry for each unique rdf
  rdfs <- levels(as.factor(iData[,1]))
  
  for(i in 1:length(rdfs)){
    tmp <- matrix(iData[which(rdfs[i]==iData[,1]),],ncol = dim(iData)[2])
    sl[[i]] <- list()
    sl[[i]]$rdf <- rdfs[i]
    sl[[i]]$slots <- tmp[,2]
    sl[[i]]$annualize <- matrix(t(tmp[,3:4]),nrow = 2)
  }
  
  sl
}
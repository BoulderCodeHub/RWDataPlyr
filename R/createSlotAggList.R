#' Creates a list for use by \code{\link{getDataForAllScens}}.
#' 
#' \code{createSlotAggList} creates a list that specifies for each slot: which rdf file to find each slot in, 
#' how to aggregate and process the slot data, and any thresholds or scaling factors. The function
#' can either read in a csv file or start from an Nx4 or Nx5 string matrix (the 5th column is optional).
#' 
#' The csv file and the matrix should be in the form of an Nx4 or Nx5 matrix.  Each row is a single
#' slot, aggregation, and threshold combination.  If you want to compare a single slot value to 
#' multiple thresholds, it needs to have one row for each threshold. The first column is the 
#' rdf the slot is found in.  The second column is the slot name.  The third column is the
#' aggregation method that will be applied to the slot (see below for a list of the aggregation
#' methods).  The fourth column is a scaling factor or threshold to compare the slot data to.
#' The fifth column is an optional column; if specified, the 5th column will be used for the
#' variable name for the data table created by \code{getDataForAllScens}. If it is not specified
#' the variable name will be created by concatenating the slot, aggregation method, and 
#' threshold/scaling factor using '_' to seperate the columns. 
#' Below is an example table. All values should be strings except for \code{NA}.
#' 
#' \tabular{ccccc}{
#' \strong{rdf} \tab \strong{Slot} \tab \strong{Aggregation Method} \tab 
#' \strong{Threshold or Scaling Factor} \tab \strong{Variable Name (optional)}\cr
#' 'KeySlots.rdf' \tab 'Powell.Pool Elevation' \tab 'EOCY' \tab NA \tab Powell EOCY Elevation\cr
#' 'KeySlots.rdf' \tab 'Mead.Pool Elevation' \tab 'AnnMinLTE' \tab '1100' \tab Mead < 1,100\cr
#' 'KeySlots.rdf' \tab 'Mead.Pool Elevation' \tab 'AnnMinLTE' \tab '1060' \tab Mead < 1,060\cr
#' 'Other.rdf' \tab 'Powell.Outflow' \tab 'AnnualSum' \tab '0.001' \tab Powell Annual Release\cr
#' }
#' 
#' The above table lists each slot, the rdf the slot is saved in, the summary function,  
#' the threshold to be used to scale the data by or compare the data to, and an optional 
#' specified variable name. The threshold and
#' scaling factors are described in more detail below. For example, the first row will result
#' in compiling all end-of-December values for Powell's pool elevation.  The data will not be 
#' scaled, and is found in KeySlots.rdf. The second row will find the annual minimum Mead pool
#' elevation and see if it is less than or equal to 1,100' feet in the second line and less
#' than or equal to 1,060' feet in the third row. To scale the data by a value less than 1, 
#' use decimals rather than fractions, as shown in the fourth row. If the Variable Name column
#' was not specified, the variable name for the first row would be \code{Powell.Pool Elevation_EOCY_1}
#' as \code{NA} is replaced with a 1 when construcing the variable names.
#' 
#' The available aggregation methods are as follows. The behaviour of the "Threshold or scaling
#' factor are described and a bold \strong{"Threshold"} or \strong{"Scaled"} indicate 
#' which is used by the aggregation method. For scaling factors, a value of "NA" will not scale
#' the data.
#' \describe{
#'  \item{'AnnMin'}{Returns the minimum annual \strong{scaled} value.}
#'  \item{'AnnMax'}{Returns the maximum annual \strong{scaled} value.}
#'  \item{'AnnualSum'}{Returns the annual \strong{scaled} sum.}
#'  \item{'AnnMinLTE'}{Checks to see if the annual minimum value is less than or equal to a 
#'  \strong{threshold.} Returns 100 if it is less than or equal to the \strong{threshold} and 0
#'  otherwise.}
#'  \item{'AnnualRaw'}{Returns the annual \strong{scaled} data. This aggregation method should 
#'  only be used if the rdf file contains only annual data. For rdf files that include 
#'  monthly data and only an annual value is desired, the EOCY aggregation method should be 
#'  used. This differs from the Monthly aggregation method, only in the timestep naming.}
#'  \item{'BOCY'}{Beginning-of-calendar year values are reported and \strong{scaled}.
#'  Any values that are NaNs are changed to 0s.}
#'  \item{'EOCY'}{End-of-calendar year values are reported and \strong{scaled}. 
#'  Any values that are NaNs are changed to 0s.}
#'  \item{'EOCYGTE'}{Checks to see if the end-of-calendar year values are greater than or equal
#'  to a \strong{threshold}. Returns 100 if it is greater than or equal to the \strong{threshold} and
#'  0 otherwise.}
#'  \item{'EOCYLTE'}{Checks to see if the end-of-calendar year values are less than or equal
#'  to a \strong{threshold}. Returns 100 if it is less than or equal to the \strong{threshold} and
#'  0 otherwise.}
#'  \item{'EOWY'}{End-of-water year values are reported and \strong{scaled}. 
#'  Any values that are NaNs are changed to 0s.}
#'  \item{'Monthly'}{Returns the monthly \strong{scaled} data.}
#'  \item{'WYMaxLTE'}{Checks to see if the maximum water year value is less than or equal to a 
#'  \strong{threshold.} Returns 100 if it is less than or equal to the \strong{threshold} and 0
#'  otherwise. This can be used to determine if an entire water year is below 
#'  a \strong{threshold}. The water year is defined as October through September 
#'  of the next year. For the first year, only January through September are 
#'  evaluated as RiveWare does not typcially export pre-simulation data.}
#'  \item{'WYMinLTE'}{Checks to see if the minimum water year value is less than or equal to a 
#'  \strong{threshold.} Returns 100 if it is less than or equal to the \strong{threshold} and 0
#'  otherwise. The water year is defined as October through September of the next year. For the
#'  first year, only January through September are evaluated as RiveWare does not typcially 
#'  export pre-simulation data.}
#'  
#' }
#' 
#' @param iData Either an Nx4 string matrix or a string with an absolute or relative path to a csv
#' file.
#' @return A multi level list.
#' @examples
#' createSlotAggList(system.file('extdata','SlotAggTable.csv',package = 'RWDataPlyr'))
#' @seealso \code{\link{getDataForAllScens}}
#' 
#' @export
#' 
createSlotAggList <- function(iData)
{
  if(!is.matrix(iData)){
    if(length(iData) > 1){
      if(length(iData) %% 4 == 0){
        warning("Attempting to convert iData to a N x 4 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
        iData <- matrix(iData, ncol = 4, byrow = T)
      } else if (length(iData) %% 5 == 0){
        warning("Attempting to convert iData to a N x 5 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
        iData <- matrix(iData, ncol = 5, byrow = T)
      } else{
        stop("iData is not a matrix, nor can it be converted to an Nx4 or Nx5 matrix")
      }
    } else if(!file.exists(iData)){
      stop(paste(iData,'does not exist.'))
    } else{
      iData <- as.matrix(utils::read.csv(iData,header = F))
    }
  } else{
    # it is a matrix
    # if it is a matrix, make sure it has 4 or 5 columns
    if(ncol(iData) != 4 & ncol(iData) != 5) {
      stop("iData is a matrix with ", ncol(iData), " columns. There should either be 4 or 5 columns.")
    }
  }
  
  # check and see if alternative variable names have been added
  altNames <- ncol(iData) == 5
  
  sl <- list() #slot list
  # create one entry for each unique rdf
  rdfs <- levels(as.factor(iData[,1]))
  
  for(i in 1:length(rdfs)){
    tmp <- matrix(iData[which(rdfs[i]==iData[,1]),],ncol = dim(iData)[2])
    sl[[i]] <- list()
    sl[[i]]$rdf <- rdfs[i]
    sl[[i]]$slots <- tmp[,2]
    sl[[i]]$annualize <- matrix(t(tmp[,3:4]),nrow = 2)
    if(altNames){
      sl[[i]]$varNames <- tmp[,5]      
    } else{
      sl[[i]]$varNames <- rep(NA,nrow(tmp))
    }
  }
  
  sl
}
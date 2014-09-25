#' Creates a list for use by \code{\link{getDataForAllScens}}.
#' 
#' \code{createSlotAggList} creates a list that specifies for each slot: which rdf file to find each slot in, 
#' how to aggregate and process the slot data, and any thresholds or scaling factors. The function
#' can either read in a csv file or start from a Nx4 string matrix.
#' 
#' The csv file and the matrix should be in the form of a Nx4 matrix.  Each row is a single
#' slot, aggregation, and threshold combination.  If you want to compare a single slot value to 
#' multiple thresholds, it needs to have one row for each threshold. The first column is the 
#' rdf the slot is found in.  The second column is the slot name.  The third column is the
#' aggregation method that will be applied to the slot (see below for a list of the aggregation
#' methods).  The fourth column is a scaling factor or threshold to compare the slot data to. 
#' Below is an example table.
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
#' The above table... DESCRIBE WHAT EACH LINE DOES.
#' 
#' The available aggregation methods are:
#' \describe{
#'  \item{'EOCY'}{End of Calendar Year}
#'  \item{'AnnMinLTE'}{Checks to see if the annual minimum value is less than or equal to a threshold.}
#'  \item{'others'}{will be filled in later.}
#' }
#' 
#' Then describe how "Threshold and Scaling Factor" behave for each aggregation method.
#' 
#' @param iData Either a Nx4 string matrix or a string with an absolute or relative path to a csv
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
#' RWDataPlot.
#' RWDataPlot: Read, manipulate, and plot data from RiverWareTM (riverware.org), that
#' is saved as an rdf (RiverWare data format) file.
#' 
#' RWDataPlot provides a tool to read data generated from RiverWare(TM) (riverware.org) in 
#' the form of rdf files into R.  Common aggregation functions, e.g., annual minimum values, 
#' are included.  Additionally, creating standard 'long' format data tables with common "scenario" 
#' attributes for the Burea of Reclamation's CRSS results are provided, along with generalized, 
#' common, plotting functions. The get and tag data functions are written to expect "Scenario" 
#' based output, i.e., output that is saved in different scenario folders, such as those generated 
#' using RiverSMART(TM).
#' 
#' \strong{As of 2015-07-02, no plotting functions exist in the package.} The data frame returned
#' by \code{getDataForAllScens} is in a format to easily plot in \code{ggplot2}.
#' 
#' To learn more about RWDataPlot, start with the vignette:
#' \code{browseVignettes(package = 'RWDataPlot')}
#' 
#' @name RWDataPlot
#' @docType package
NULL

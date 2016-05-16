#' Example rdf file with monthly data.
#'
#' An example of an rdf file that has already been read into R via 
#' \link{read.rdf}. This example contains 39 slots, at the monthly 
#' timestep for 11 years and 25 runs. Slots include pool elevation, 
#' flow, and flags. Use this with \link{getSlotsInRdf} or 
#' \link{rdfSlotToMatrix} to use the data.
#'
#' @format A multi level list. \code{keyRdf$meta} provides a description
#' of the RiverWare run used to generate this data.
#' 
#' @source Bureau of Reclamation, 2016
"keyRdf"

#' Example rdf file with annual data.
#'
#' An example of an rdf file that has already been read into R via 
#' \link{read.rdf}. This example contains 23 slots, at the annual 
#' timestep for 11 years and 25 runs. Slots only include  
#' flags. Use this with \link{getSlotsInRdf} or 
#' \link{rdfSlotToMatrix} to use the data.
#'
#' @format A multi level list. \code{sysRdf$meta} provides a description
#' of the RiverWare run used to generate this data.
#' 
#' @source Bureau of Reclamation, 2016
"sysRdf"


#' create a data frame from one of the objects in one trace of the rdf list
#' 
#' @noRd

rdfObjectToTbl <- function(rdfObject, timeSteps)
{
  ot <- rdfObject$object_type
  on <- rdfObject$object_name
  sn <- rdfObject$slot_name
  os <- paste(on, sn, sep = ".")
  units <- rdfObject$units
  scale <- as.numeric(rdfObject$scale)
  value <- rdfObject$values * scale
  
  tbl <- data.frame("Timestep" = timeSteps) %>%
    dplyr::mutate(
      ObjectName = on,
      SlotName = sn,
      ObjectType = ot,
      ObjectSlot = os,
      Value = value,
      Unit = units
    )

  tbl
}


#' for a single run/trace, take the list and create a data frame from it

#' "loop" through all objects within 1 trace of data
#' @noRd

rdfTraceToTbl <- function(rdfTrace, traceNum) 
{
  trace <- as.integer(rdfTrace$idx_sequential)
  timeSteps <- rdfTrace$times
  ruleSet <- rdfTrace$rule_set
  slotSet <- rdfTrace$slot_set # the input DMI on the input tab
  
  # ***
  # maybe check if time_step_unit is year, and if so, change the timestep label
  # maybe change to yearmon here?
  # ***
  
  # for all objects, call getObjectData for rdfTrace$objects
  tbl <- do.call(
    rbind, 
    lapply(rdfTrace$objects, rdfObjectToTbl, timeSteps = timeSteps)
  ) %>%
    dplyr::mutate(
      TraceNumber = traceNum, 
      RulesetFileName = ruleSet, 
      InputDMIName = slotSet
    ) %>%
    dplyr::mutate_at(.vars = "Timestep", .funs = as.character)
  
  tbl
}

#' Convert RDF to a Tibble
#' 
#' \code{rdfToTbl} converts an rdf list to a tibble (data.frame).
#' 
#' The rdf list is converted to a "long" data frame, and then converted to a 
#' \code{\link[tibble]{tibble}}. All of the \code{meta} entries into the rdf list
#' are stored as attribures in the returned tibble. 
#' 
#' @param rdf An rdf list returned from \code{\link{read.rdf2}}.
#' @param scenario An optional parameter, that if it is not \code{NULL} (default)
#' will be added to the tibble as another variable. Typically a string, but it is
#' not coerced to a string.
#' 
#' @return A tibble with additional attributes from the rdf list
#' 
#' @examples 
#' t1 <- rdfToTbl(keyRdf)
#' t2 <- rdfToTbl(sysRdf, scenario = "ISM1988_2014,2007Dems,IG,2002")
#' 
#' @seealso \code{\link{read.rdf2}}, \code{\link{read.rdf}}
#' 
#' @export

rdfToTbl <- function(rdf, scenario = NULL)
{
  # rdf[["meta"]] contains meta data
  # rdf[["runs"]] will be the length of rdf[[1]]$number_of_runs
  atts <- rdf$meta
  nRun <- as.integer(rdf$meta$number_of_runs)
  
  tbl <- do.call(
    rbind, 
    lapply(1:nRun, function(x) rdfTraceToTbl(rdf[[2]][[x]], traceNum = x))
  )
  
  if(!is.null(scenario))
    tbl$Scenario <- scenario
  
  structure(
    tibble::as_tibble(tbl),
    "MRMName" = atts$name,
    "owner" = atts$owner,
    "description" = atts$description,
    "createDate" = atts$create_date,
    "numberOfTraces" = atts$numer_of_runs
  )
}

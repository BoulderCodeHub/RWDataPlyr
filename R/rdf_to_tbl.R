
#' Convert RDF to a Tibble
#' 
#' `rw_rdf_to_tbl()` converts an rdf list to a tibble (data.frame).
#' 
#' The rdf list is converted to a "long" data frame, and then converted to a 
#' [tibble::tibble()]. All of the `meta` entries into the rdf list
#' are stored as attribures in the returned tibble. These attributes are:
#' "mrm_config_name", "owner", "description", "create_date", and "n_traces".
#' 
#' @param rdf An rdf list returned from [read.rdf()].
#' @param scenario An optional parameter, that if it is not `NULL` (default)
#'   will be added to the tibble as another variable. Typically a string, but it 
#'   is not coerced to a string.
#' @param keep_cols Either boolean, or a character vector of column names to 
#'   keep in the returned tibble. The "Timestep", "TraceNumber", "ObjectSlot", 
#'   and "Value" columns are always returned. `keep_cols` allows 
#'   the user to include other columns that are not always required, e.g., 
#'   "Object" and "Slot" instead of only the "ObjectSlot" column. If `keep_cols`
#'   is `FALSE`, then only the above 5 columns are returned. If `keep_cols` is
#'   `TRUE`, then all columns are returned. Finally, if `keep_cols` is a 
#'   character vector, then the above 5 columns are returned, along with any of
#'   those specified in `keep_cols`; if they are not found, a warning will post.
#'   If `scenario` is specified, it will also always be returned.
#' @param add_ym Boolean that controls whether or not `Year` and `Month` columns
#'   are addded to the returned tibble. If `TRUE` (default), they will be added, 
#'   and if `FALSE` they will not be added. They are constructed from the dates 
#'   in the `Timestep` column.
#' 
#' @return A tibble with additional attributes from the rdf list
#' 
#' @examples 
#' rdftbl <- rw_rdf_to_tbl(keyRdf)
#' # same as previous, except you do not want "Year" and "Month" columns
#' rdftbl <- rw_rdf_to_tbl(keyRdf, add_ym = FALSE)
#' # but you do want to keep the object name seperately:
#' rdftbl <- rw_rdf_to_tbl(keyRdf, add_ym = FALSE, keep_cols = "Object")
#' rdftbl <- rw_rdf_to_tbl(sysRdf, scenario = "ISM1988_2014,2007Dems,IG,2002")
#' 
#' @seealso [read.rdf()]
#' 
#' @export

rw_rdf_to_tbl <- function(rdf, scenario = NULL, keep_cols = FALSE, add_ym = TRUE)
{
  stopifnot(
    (is.logical(keep_cols) && !is.na(keep_cols) && length(keep_cols == 1)) || 
    (is.character(keep_cols) && length(keep_cols) > 0)
  )
  
  stopifnot(is.logical(add_ym) && !is.na(add_ym) && length(add_ym) == 1)
  
  # this function only reads in one scenario, so should only have one scenario name
  stopifnot(is.null(scenario) || length(scenario) == 1)
  
  # rdf[["meta"]] contains meta data
  # rdf[["runs"]] will be the length of rdf[[1]]$number_of_runs
  atts <- rdf$meta
  nRun <- as.integer(rdf$meta$number_of_runs)
  
  tbl <- do.call(
    rbind, 
    lapply(1:nRun, function(x) rdf_trace_to_tbl(rdf[[2]][[x]], traceNum = x))
  )
  
  if ((is.logical(keep_cols) && !keep_cols) || is.character(keep_cols))
    tbl <- select_rdftbl_cols(tbl, keep_cols)
  
  if (!is.null(scenario))
    tbl$Scenario <- scenario
  
  if (add_ym)
    tbl <- add_ym_to_rdftbl(tbl)
    
  structure(
    tibble::as_tibble(tbl),
    "mrm_config_name" = atts$name,
    "owner" = atts$owner,
    "description" = atts$description,
    "create_date" = atts$create_date,
    "n_traces" = nRun
  )
}


#' create a data frame from one of the objects in one trace of the rdf list
#' 
#' @noRd

rdf_object_to_tbl <- function(rdfObject, timeSteps)
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

rdf_trace_to_tbl <- function(rdfTrace, traceNum) 
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
    lapply(rdfTrace$objects, rdf_object_to_tbl, timeSteps = timeSteps)
  ) %>%
    dplyr::mutate(
      TraceNumber = traceNum, 
      RulesetFileName = ruleSet, 
      InputDMIName = slotSet
    ) %>%
    dplyr::mutate_at(.vars = "Timestep", .funs = as.character)
  
  tbl
}

#' Standard (required) Column Names for RDF Tables that will always be returned
#' by `rw_rdf_to_tbl()`
#' 
#' @noRd

req_rwtbl_cols <- function()
{
  c("Timestep","TraceNumber", "ObjectSlot", "Value")
}

#' Select a subset of the columns in the rdftbl, as specified by `keep_cols`
#' 
#' @noRd

select_rdftbl_cols <- function(rdftbl, keep_cols)
{
  if (is.logical(keep_cols))
  {
    select_cols <- req_rwtbl_cols()
  } else {
    # check that the user specified keep_cols are actually columns in rdftbl
    colsInTbl <- (keep_cols %in% colnames(rdftbl))
    missing_cols <- keep_cols[!colsInTbl]
    if (length(missing_cols) > 0)
      warning(
        "The following columns specified by 'keep_cols' were not found in the rwtbl:\n",
        "    ", toString(missing_cols)
      )
    select_cols <- c(req_rwtbl_cols(), keep_cols[colsInTbl])
  }
  
  rdftbl %>%
    dplyr::select_at(.vars = select_cols)
}

#' Adds "Year", and "Month" columns to the tbl, which are constructed from 
#' the "Timestep" column.
#' @noRd

add_ym_to_rdftbl <- function(tbl)
{
  other_cols <- colnames(tbl)[colnames(tbl) != "Timestep"]
  tbl <- tbl %>%
    dplyr::mutate_at("Timestep", dplyr::funs("ym" = zoo::as.yearmon)) %>%
    dplyr::mutate_at(
      "ym", 
      dplyr::funs("Year" = ym_get_year, "Month" = ym_get_month_str)) %>%
    dplyr::select_at(c("Timestep", "Year", "Month", other_cols))
}


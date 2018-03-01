
#' List the slot names in a tbl_df
#' 
#' `rwtbl_slot_names()` lists all of the slot names found in a `tbl_df` object 
#' containing RiverWare output data.
#' 
#' Given a `tbl_df` object that is returned by [rw_rdf_to_tbl()] or 
#' [rw_read_rw_csv()], return all of the Object.Slot names found in the data 
#' frame. These are the unique full slot names found in the `ObjectSlot` 
#' column.
#' 
#' @param rwtbl A `tbl_df` object with RiverWare output. Must contain the 
#'   `ObjectSlot` column.
#' 
#' @seealso [rw_rdf_to_tbl()], [rw_read_rw_csv()]
#' 
#' @examples 
#' rwtbl <- rw_rdf_to_tbl(keyRdf)
#' rwtbl_slot_names(rwtbl)
#' 
#' @export
rwtbl_slot_names <- function(rwtbl)
{
  if ("ObjectSlot" %in% names(rwtbl)) {
    varname <- "ObjectSlot"
  } else {
    stop("Invalid `tbl_df` passed to `rdftbl_slot_names()`.\n",
         "It does not have an `ObjectSlot` column")
  }
  
  as.character(levels(as.factor(rwtbl[[varname]])))
}

#' Map a variable name to the RiverWare slot name
#' 
#' `rwtbl_var_to_slot()` provides the RiverWare slot name that was used to 
#' create the specified variable name (`varname`). If `varname` is not found in
#' `rwtblsmmry`, a warning message is posted.
#' 
#' @param rwtblsmmry A tbl_df of summarizes RiverWare data; likely output from
#'   [rw_scen_aggregate()].
#' 
#' @param varname A vector of variable names to map to slot names.
#' 
#' @return A character vector of the found slot names. `character(0)` if no 
#'   variable names were found.
#'   
#' @examples 
#' rwtbl_var_to_slot(scen_data, "peLt1000")
#' rwtbl_var_to_slot(scen_data, c("peLt1000", "peEocy"))
#' 
#' @export
rwtbl_var_to_slot <- function(rwtblsmmry, varname)
{
  if (!("Variable" %in% names(rwtblsmmry))) {
    stop("Invalid `tbl_df` passed to `rwtbl_var_to_slot()`.\n",
         "It does not have a `Variable` column")
  } 
  
  rwa <- attr(rwtblsmmry, "rwd_agg")
  
  if (is.null(rwa))
  {
    stop("Invalid `tbl_df` passed to `rwtbl_var_to_slot()`.\n",
         "It does not have a `rwd_agg` attribute.")
  }
  
  slot_names <- rwa$slot[match(varname, rwa$variable)]
  
  if (anyNA(slot_names)) {
    tmp <- varname[is.na(slot_names)]
    warning("The following variables were not found in the rwtbl:\n",
            paste(paste("    ", tmp), collapse = "\n"),
            call. = FALSE)
    
    slot_names <- slot_names[!is.na(slot_names)]
  }
  
  slot_names
}

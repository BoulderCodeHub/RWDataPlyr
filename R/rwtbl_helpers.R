
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

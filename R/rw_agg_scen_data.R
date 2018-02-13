# can probably start using slot_agg_matrix that just reads that in directly 
# and does a few checks; then find unique files; read those in, and then 
# just do a row-wise apply of each row; don't think we need all the crazy list
# levels

# summary:
# which.min, which.max
# Summary S4 generics that work: min, max, sum, prod, mean, median
# no reason that a custom function won't work here

#' `rwtbl_summarise_all()` processes a `slot_agg_matrix` for a single scenario.
#' 
#' @param slot_agg_matrix A slot_agg_matrix specifying the rdfs, slot, and 
#'   aggregation methods to use. See XXX.
#' @param scen_dir The top level directory that contains the rdf files.
#' @inheritParams rw_rdf_to_tbl
#' 
#' @export

rwtbl_summarise_all <- function(slot_agg_matrix, 
                                scen_dir = ".",
                                scenario = NULL,
                                keep_cols = FALSE)
{
  # get unique rdf files
  rdfs <- unique(slot_agg_matrix$file)
  rdf_files <- file.path(scen_dir, rdfs)
  rdfs_exist <- file.exists(rdf_files)
  
  # verify rdfs exist
  if (!any(rdfs_exist)) {
    stop(
      "The following rdfs were not found in ", normalizePath(scen_dir), ":\n",
      toString(rdfs[!rdfs_exist]), 
      call. = FALSE
    )
  }
  
  rdf_len <- seq_len(length(rdfs))
  
  rwtblsummry <- lapply(
    rdf_len,
    function(x){
      # call rwtbl_apply_sam for each unique rdf
      # seperate sam into one sam for each rdf;
      # read the rdf, then apply the sam to that rdf
      
      rwtbl <- rw_rdf_to_tbl(
        read.rdf(rdf_files[x]), 
        scenario = scenario, 
        keep_cols = keep_cols, 
        add_ym = TRUE
      ) 
      
      tmp_sam <- slot_agg_matrix[slot_agg_matrix$file == rdfs[x],]
      
      rwtbl_apply_sam(rwtbl, tmp_sam)
    }
  )
  
  rwtblsummry <- dplyr::bind_rows(rwtblsummry)
  
  # save the sam as an attribute
  structure(
    rwtblsummry,
    "slot_agg_matrix" = slot_agg_matrix
  )
}

rwtbl_apply_sam <- function(rwtbl, slot_agg_matrix)
{
  sam_rows <- seq_len(nrow(slot_agg_matrix))
  rwtblsmmry <- lapply(
    sam_rows, 
    function(x) rwtbl_apply_sar(rwtbl, slot_agg_matrix[x,])
  )
  
  dplyr::bind_rows(rwtblsmmry)
}

rwtbl_apply_sar <- function(rwtbl, slot_agg_row)
{
  apply_period(rwtbl, slot_agg_row) %>%
    apply_summary(slot_agg_row) %>%
    apply_eval(slot_agg_row) %>%
    add_month_to_annual() %>%
    add_var_drop_objectslot(slot_agg_row)
}

add_var_drop_objectslot <- function(rwtbl, slot_agg_row)
{
  var_map <- slot_agg_row$variable
  names(var_map) <- slot_agg_row$slot
  
  rwtbl %>%
    dplyr::mutate_at("ObjectSlot", dplyr::funs("Variable" = var_map[.])) %>%
    dplyr::select(-dplyr::matches("ObjectSlot"))
}

#' If the data had dropped the Month column, add in December so that it is 
#' considered annual data.
#' 
#' @noRd

add_month_to_annual <- function(rwtbl)
{
  cols <- colnames(rwtbl)
  dec_fun <- function(x) "December"
  if (!"Month" %in% cols){
    rwtbl <- rwtbl %>%
      dplyr::mutate_at("Year", dplyr::funs("Month" = dec_fun)) %>%
      dplyr::select_at(c("Year", "Month", cols[cols != "Year"]))
  }
  rwtbl
}


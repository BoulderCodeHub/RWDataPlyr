
#' Aggregate RiverWare output for a single scenario
#' 
#' `rwtbl_aggregate()` aggregates a single scenario of data by processing a 
#' [rwd_agg] object. The user specifies the [rwd_agg], which 
#' determines the slots that are aggregated, and how they are aggregated. See
#' [rwd_agg] for more details on how it should be specified.
#' 
#' @param slot_agg_matrix A [rwd_agg] object specifying the rdfs, slots, and 
#'   aggregation methods to use.
#' @param scen_dir The top level directory that contains the rdf files.
#' @inheritParams rw_rdf_to_tbl
#' 
#' @export

rwtbl_aggregate <- function(slot_agg_matrix, 
                            scen_dir = ".",
                            scenario = NULL,
                            keep_cols = FALSE)
{
  if (!is_rwd_agg(slot_agg_matrix))
    stop("`slot_agg_matrix` passed to `rwtbl_aggregate()` is not a `rwd_agg`")
  
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
  
  rwtblsmmry <- lapply(
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
  
  rwtbl_atts <- lapply(rdf_len, function(x) rwtbl_get_atts(rwtblsmmry[[x]]))
  names(rwtbl_atts) <- rdfs
  
  rwtblsmmry <- dplyr::bind_rows(rwtblsmmry)
  
  cols <- colnames(rwtblsmmry)
  cols <- cols[!(cols %in% c("Variable", "Value"))]
  rwtblsmmry <- rwtblsmmry %>% 
    dplyr::select(dplyr::one_of(cols, "Variable", "Value"))
  
  # save the sam as an attribute
  structure(
    rwtblsmmry,
    "slot_agg_matrix" = slot_agg_matrix,
    "rdf_atts" = rwtbl_atts
  )
}

#' Apply all of the slot aggregations to a single rdf file
#' @noRd

rwtbl_apply_sam <- function(rwtbl, slot_agg_matrix)
{
  # if rwd_agg uses the "all" keyword, need to construct the full rwd_agg
  if (slot_agg_matrix$slot == "all") {
    slot_agg_matrix <- rwd_agg_build_all(slot_agg_matrix$file, rwtbl)
  }
  
  sam_rows <- seq_len(nrow(slot_agg_matrix))
  rwtblsmmry <- lapply(
    sam_rows, 
    function(x) rwtbl_apply_sar(rwtbl, slot_agg_matrix[x,])
  )
  
  rwtblsmmry <- dplyr::bind_rows(rwtblsmmry)
  
  attributes(rwtblsmmry) <- c(attributes(rwtblsmmry), rwtbl_get_atts(rwtbl))
  
  rwtblsmmry
}

#' Apply a single slot agg matrix's row (sar)
#' @noRd

rwtbl_apply_sar <- function(rwtbl, slot_agg_row)
{
  apply_period(rwtbl, slot_agg_row) %>%
    apply_summary(slot_agg_row) %>%
    apply_eval(slot_agg_row) %>%
    add_month_to_annual() %>%
    add_var_drop_objectslot(slot_agg_row)
}

#' Add in a `Variable` columns and drop the `ObjectSlot` column
#' @noRd

add_var_drop_objectslot <- function(rwtbl, slot_agg_row)
{
  var_map <- slot_agg_row$variable
  names(var_map) <- slot_agg_row$slot
  tmp_groups <- dplyr::group_vars(rwtbl)
  tmp_groups <- tmp_groups[tmp_groups != "ObjectSlot"]
  
  rwtbl %>%
    dplyr::group_by_at(tmp_groups) %>%
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



#' Aggregate RiverWare output for a single scenario
#' 
#' `rwtbl_aggregate()` aggregates a single scenario of data by processing a 
#' [rwd_agg] object. The user specifies the [rwd_agg], which 
#' determines the slots that are aggregated, and how they are aggregated. See
#' [rwd_agg] for more details on how it should be specified.
#' 
#' @param slot_agg_matrix A [rwd_agg] object specifying the rdfs, slots, and 
#'   aggregation methods to use.
#' @param rdf_dir The top level directory that contains the rdf files.
#' @inheritParams rw_rdf_to_tbl
#' @param nans_are Either "0" or "error". If "0", then `NaN`s in the rwtbl are
#'   treated as 0s. If "error", then any `NaN`s will cause an error in this 
#'   function.
#' 
#' @export

rwtbl_aggregate <- function(slot_agg_matrix, 
                            rdf_dir = ".",
                            scenario = NULL,
                            keep_cols = FALSE,
                            nans_are = "0")
{
  if (!is_rwd_agg(slot_agg_matrix))
    stop("`slot_agg_matrix` passed to `rwtbl_aggregate()` is not a `rwd_agg`")
  
  nans_are <- match.arg(nans_are, choices = c("0", "error"))
  
  # get unique rdf files
  rdfs <- unique(slot_agg_matrix$file)
  rdf_files <- file.path(rdf_dir, rdfs)
  rdfs_exist <- file.exists(rdf_files)
  
  # verify rdfs exist
  if (!any(rdfs_exist)) {
    stop(
      "The following rdfs were not found in ", normalizePath(rdf_dir), ":\n",
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
      ) %>%
        check_nans(nans_are, rdf_file = rdf_files[x])
      
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
  
  scen_folder <- data.frame(
    "scenario" = ifelse(is.null(scenario), NA_character_, scenario), 
    "folder" = normalizePath(rdf_dir)
  )
  
  # save the sam as an attribute
  structure(
    rwtblsmmry,
    "slot_agg_matrix" = slot_agg_matrix,
    "rdf_atts" = rwtbl_atts,
    "scen_folder" = scen_folder
  )
}

#' Apply all of the slot aggregations to a single rdf file
#' @noRd

rwtbl_apply_sam <- function(rwtbl, slot_agg_matrix)
{
  # if rwd_agg uses the "all" keyword, need to construct the full rwd_agg
  # need to determine if only the all key word exists, or if there is one 
  # row that is all, but then there are summary rows
  if ("all" %in% slot_agg_matrix$slot) {
    if (nrow(slot_agg_matrix) == 1) {
      slot_agg_matrix <- rwd_agg_build_all(rwtbl, slot_agg_matrix$file)
    } else {
      # must have other summary rows, so remove all row and combine with the 
      # "all" rows
      slot_agg_matrix <- rbind(
        slot_agg_matrix[slot_agg_matrix$slot != "all",],
        rwd_agg_build_all(
          rwtbl,
          slot_agg_matrix$file[slot_agg_matrix$slot == "all"]
        )
      )
    }
    
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

#' Check if there are any NaNs in the `rwtbl`, and either convert to 0s or 
#' throw an error.
#' 
#' @inheritParams rwtbl_aggregate
#' @param rdf_file The rdf file name as a character.
#' 
#' @noRd
check_nans <- function(rwtbl, nans_are, rdf_file)
{
  nans_are <- match.arg(nans_are, choices = c("0", "error"))
  
  if (any(is.nan(rwtbl$Value))) {
    if (nans_are == "error") {
      slots <- rwtbl %>%
        dplyr::filter_at("Value", dplyr::all_vars(is.nan(.)))
      
      slots <- unique(slots$ObjectSlot)
      nSlots <- length(slots)
      slotM <- ""
      if (length(slots) > 10) {
        slots <- slots[1:10]
        slotM <- "...\n(Only the first 10 slots containing `NaN`s are printed.)"
      }
      
      stop(
        "`NaN`s were found in ", rdf_file, 
        " and `nans_are` treated as an error.\n",
        "`NaN`s were found in ", nSlots, " slots:\n",
        paste(slots, collapse = "\n"),
        "\n", slotM,
        call. = FALSE
      )
      
    } else {
      # convert any NaNs to 0
      rwtbl <- rwtbl %>%
        dplyr::mutate_at("Value", nan_to_zero)
    }
  }
  
  rwtbl
}

#' Convert NaNs to 0
#' @noRd
nan_to_zero <- function(x) {
  x[is.nan(x)] <- 0
  x
}



period_filter_group <- function(tbl, period)
{
  # CY:  group_by year
  # WY: convert to WY,  group by year
  # EOCY: filter only December, group by year
  # EOWY: filter only September, group by year
  # "March": filter only March, group by year
  # custom: summer, filter June, July, and August
  # asis: monthly stays monthly, annual stays annual; group by month and year
  
  if (!(period %in% c(month.name, "asis"))) {
    # convert to appropriate function, and then chuck that function exists
    if (!exists(period, mode = "function"))
        stop("specified `period`: ", period, " does not match expected values or existing functions.\n",
             "   Please see ?XXX for help.", call. = FALSE)
    
    period_filter <- eval(parse(text = paste0(period, "()")))
 
    if (!is.character(period_filter) && !is_custom_period_fun(period_filter)) {
      stop("Function `", period, "()` must return either a function or character vector.")
    }
  } else if (period %in% month.name) {
    period_filter <- period
  }
  
  if (period == "asis") {
    tbl <- tbl %>%
      dplyr::group_by_at(c("Year", "Month"))
  } else if (is_custom_period_fun(period_filter)) {
    # list(fun = wy_convert, filter_months = month.name, group_tbl = c("Year"))
    tbl <- period_filter$fun(tbl) %>%
      dplyr::filter_at("Month", dplyr::any_vars(. == period_filter$filter_months)) %>%
      dplyr::group_by_at(period_filter$group_tbl)
  } else {
    tbl <- tbl %>%
      dplyr::filter_at("Month", dplyr::any_vars(. == period_filter)) %>%
      dplyr::group_by_at("Year")
  }
  
  tbl
}

apply_period <- function(tbl, slot_agg_row)
{
  # check that it has Year and Month columns

  # filter based on slot
  tbl <- dplyr::filter_at(tbl, "ObjectSlot", dplyr::any_vars(. == slot_agg_row$slot))
  
  # filter and group for period
  tbl <- period_filter_group(tbl, period = slot_agg_row$period)
  
  # ** need to consider how to group/not lose the other column names that were 
  # included in tbl
  tbl
}

# can probably start using slot_agg_matrix that just reads that in directly 
# and does a few checks; then find unique files; read those in, and then 
# just do a row-wise apply of each row; don't think we need all the crazy list
# levels

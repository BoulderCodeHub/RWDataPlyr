
eocy <- function() "December"

eowy <- function() "September"

cy <- function() month.name

wy <- function()
{
  wy_convert <- function(rwtbl)
  {
    tmp <- rwtbl %>%
      dplyr::mutate(
        "Year" := ym_get_wateryear(zoo::as.yearmon(.data[["Timestep"]]))
      )
        
    # drop if WY contains less than 6 months of data for the year
    cols <- names(tmp)
    cols <- cols[!(cols %in% c("Timestep", "Month", "Value"))]
    keep_yrs <- unique(
      (tmp %>%
         dplyr::group_by(dplyr::across(dplyr::all_of(cols))) %>%
         dplyr::tally() %>%
         dplyr::filter(.data[["n"]] > getOption("rwdataplyr.wy_month_tol"))
      )$Year
    )
    
    dplyr::filter(tmp, .data[["Year"]] %in% keep_yrs)
  }
  
  list(fun = wy_convert, filter_months = month.name, group_tbl = c("Year"))
}

is_custom_period_fun <- function(x)
{

  is.list(x) && length(x) == 3 && 
    all(names(x) %in% c("fun", "filter_months", "group_tbl")) &&
    is.function(x$fun) && all(x$filter_months %in% month.name)
}

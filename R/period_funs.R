
eocy <- function() "December"

eowy <- function() "September"

cy <- function() month.name

wy <- function()
{
  list(fun = wy_convert, filter_months = month.name, group_tbl = c("Year"))
}

wy_convert <- function(tbl)
{
  tbl %>%
    dplyr::mutate(ym = zoo::as.yearmon(Timestep)) %>%
    dplyr::mutate(Year = getWYFromYearmon(ym)) %>%
    dplyr::select(-ym)
}

is_custom_period_fun <- function(x)
{
  is.list(x) && length(x) == 3 && 
    names(x) == c("fun", "filter_months", "group_tbl") &&
    is.function(x$fun) && all(x$filter_months %in% month.name)
}

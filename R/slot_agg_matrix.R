
#' `slot_agg_matrix`
#' 
#' checks: if CY or WY, summary "none" probably doesn't make sense
#' if time is raw, then summary must be none
#' 
#' create ann attribute called "all"; if it is true, then after the get data function
#' is called, it will call another function to recreate the slotaggm with all the 
#' appropriate values; when the default constructor is called with 
#' c("KeySlots.rdf" = "all", "SystemCondtions.rdf" = all), it will only fill in the
#' first two columns, and then plac NA's everywhere else; skipping all other 
#' relivant checks.
#' 
#' summary:
#' which.min, which.max
#' Summary S4 generics that work: min, max, sum, prod, mean, median
#' no reason that a custom function won't work here


slot_agg_matrix <- function()
{
  structure(NULL, class = "slot_agg_matrix")
}

is.slot_agg_matrix <- function(slot_agg_matrix) TRUE

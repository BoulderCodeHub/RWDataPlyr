
#' Class to specify how to aggregate RiverWare data
#' 
#' `rwd_agg()` creates a RiverWare data aggregator (`rwd_agg`) object, which 
#' lets users specify how specific RiverWare slots should be aggregated.
#' 
#' `rwd_agg` objects can be created in two ways:
#' 1. By providing a data.frame, with the following expected columns `file`, 
#'   `slot`, `period`, `summary`, `eval`, `t_s`, and `variable`. Each row in the
#'   data.frame should include all of the information for how each individual
#'   slot will be aggregated. See the *Aggregation Specification* section below
#'   for details on how to specify each column. 
#' 2. By providing a vector of rdf files. If specified in this manor, all of the
#'   slots in each rdf file will be read in to a `rwtbl`, but will not be 
#'   aggregated/summarized.
#'   
#' @section Aggregation Specification:
#' 
#' In order to specify how each slot should be aggregated, each column should 
#' include specific keywords, which are described below. It is up to the user
#' to specify which rdf file contains each slot. In a general case, the user
#' specifies the `slot` that is found in a specific rdf file (`file`). A 
#' `summary` function is applied to a subset `period` of the `slot`, and then
#' compared (`eval`) to a threshold (`t_s`) and saved as the `variable`.
#' - *file:* specifies the rdf file that contains the slot.
#' - *slot:* the full RiverWare slot name.
#' - *period:* the period that the slot should be summarized over. This should 
#'   either be a function name, a full month name (found in [month.name]), or 
#'   the keyword "asis". 
#'   If specifying a function, only the function name should be specified. This
#'   package provides the following functions: `cy()`, `wy()`, `eocy()`, and 
#'   `eowy()`. `cy()` indicates the data will be summarized over the calendar
#'   year, i.e., January - December, while `wy()` summarizes over the water 
#'   year, i.e., October - September. `eocy()` selects the end of the calendar
#'   year, and `eowy()` selects the end of the water year. When specified in 
#'   the `slot_agg` object, leave off the `()`, that is only specify the 
#'   function name. This can also be a user specified custom function; see the
#'   *Custom Period Functions* section. If the keyword "asis" is specified, the
#'   data is returned for its native timestep, i.e, monthly data
#'   will return monthly data and annual data will return annual.
#' - *summary:* the summary function that should be applied to the period 
#'   specified as a function name, or `NA`. If the `period` specified is "asis"
#'   or returns only one month, e.g., `eocy()`, then the summary should be `NA`.
#'   The summary function should only return one value; for that reason, most
#'   of the `Summary` [S4groupGeneric]s work. Notably, `range()` will not
#'   since it returns two values. There is no reason that a custom function
#'   will not work here, but it has not been tested and implemented. 
#' - *eval:* the comparison operator to use ([S4groupGeneric] - `Compare`). If
#'   no comparison is desired, then `NA` should be used. If `eval` is specified
#'   the value returned from applying the `summary` to the `period` will be 
#'   compared to the threshold specified by `t_s`. The results of the comparison
#'   are returned as 0 and 1 instead of `TRUE` and `FALSE`.
#' - *t_s:* either the threshold to be compared to if `eval` is not `NA` or a 
#'   value to scale the result by, e.g,. 0.001 to convert from acre-ft to 
#'   thousand acre-ft. 
#' - *variable:* the variable name that will be used to identify the results
#'   of applying the period, summary, comparison/scaling to. All variable names
#'   should be unique.
#' 
#' For example, to determine if the minimum water year elevation at Lake Powell
#' is below elevation 3550 feet, the following would be specified:
#' ```
#' data.frame(
#'   file = "KeySlots.rdf",
#'   slot = "Powell.Pool Elevation",
#'   period = "wy",
#'   summary = "min",
#'   eval = "<",
#'   t_s = 3550,
#'   variable = "powellLt3550"
#' )
#' ```
#'   
#' @section Custom Period Functions:
#' 
#' Users can specify custom period functions to make it easier to group months
#' together in custom ways. For example a function could return all of the 
#' summer months, or the more complicated case groups months across different
#' calendar years together. In fact, `wy()` is an example of a function that 
#' does this; another example might be grouping December - February together 
#' for winter months. 
#' 
#' The custom period function should return a list with three elements:
#' - `fun` - a function that will modify a rwtbl and properly determine the 
#'   new `Year`s based on the custom period.
#' - `filter_months` - the months that should be grouped together.
#' - `group_tbl` - how to group the returned rwtbl; likely either `c("Year")` or
#'   `c("Year", "Month")`
#'   
#' See the XXX vignette for example implementations of both the summer and 
#' winter custom functions described above.
#' 
#' @param x A data.frame with required column names and valid entries; see 
#'   *Details* and *Aggregation Specification* sections.
#' @param rdfs A vector of rdf names; see *Details* and 
#'   *Aggregation Specification* sections.
#'   
#' @examples 
#' # determine if Powell's minimum water year elevation is < 3550'
#' rwd_agg(
#'   data.frame(
#'     file = "KeySlots.rdf",
#'     slot = "Powell.Pool Elevation",
#'     period = "wy",
#'     summary = "min",
#'     eval = "<",
#'     t_s = 3550,
#'     variable = "powellLt3550"
#'   )
#' )
#' 
#' # get all the monthly slots in KeySlots.rdf
#' rw_agg(rdfs = "KeySlots.rdf")
#'
#' @export

rwd_agg <- function(x = NULL, rdfs = NULL)
{
  if (!missing(x) & !missing(rdfs))
    stop("When creating a `rwd_agg`, specify either `x` or `rdfs`, not both.")
 
  if (missing(x)){
    file_col <- rdfs
    slot_col <- rep("all", length(rdfs))
    x <- data.frame(
      "file" = file_col,
      "slot" = slot_col,
      "period" = NA,
      "summary" = NA,
      "eval" = NA,
      "t_s" = NA,
      "variable" = NA,
      stringsAsFactors = FALSE
    )
  }
  
  validate_rwd_agg(new_rwd_agg(x))
}

new_rwd_agg <- function(x)
{
  stopifnot(is.data.frame(x))
  structure(x, class = c("rwd_agg", "data.frame"))
}

validate_rwd_agg <- function(x)
{
  if (!is.data.frame(x))
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  cols <- c("file", "slot", "period", "summary", "eval", "t_s", "variable")
  if (ncol(x) != 7 | !all(colnames(x) == cols)) {
    stop("The `colnames(x)` must be exactly:", paste(cols, collapse = ", "))
  }
  
  # all columns should not be factors
  if ("factor" %in% simplify2array(lapply(1:7, function(cc) typeof(x[[cc]])))) {
    stop("No columns should be factors.")
  }
  
  # check valid file extensions (for now only rdfs)
  if (!all(tools::file_ext(x$file) %in% rwd_ext)) {
    stop("All `file` extensions should be: ", paste(rwd_ext, collapse = ","))
  }
  
  # all variables should be unique
  if (length(unique(x$variable)) != nrow(x)) {
    stop("All `variable`s should be unique.")
  }
  
  # if period is "asis", then summary must be none
  check_period_asis(x)
  
  # checks: if CY or WY, summary NA probably doesn't make sense
  check_period_wy_cy(x)
  
  # check the eval and t_s columns
  lapply(seq_len(nrow(x)), function(rr) check_eval_and_t_s(x[rr,]))
  
  x
}

#' Returns all of the file extensions that rwd_agg and the aggregate functions
#' can handle. Will eventually include .csv and .nc.
#' @noRd

rwd_ext <- c("rdf")

#' Test if the object is a rwd_agg
#'
#' @param x An object
#' 
#' @return `TRUE` if the object inherits from the `rwd_agg` class.
#' @export
is_rwd_agg <- function(x) inherits(x, "rwd_agg")

#' @rdname is_slot_agg_list
#' @export
is.rwd_agg <- is_rwd_agg

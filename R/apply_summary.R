
# which.min, which.max
# Summary S4 generics that work: min, max, sum, prod, 
# how to get range to work or fail (Summary S4 generic that doesn't work)
# no reason that a custom function won't work here
# how to specify na.rm = TRUE??
# 

apply_summary <- function(rwtbl, slot_agg_row)
{
  if (is.null(groups(rwtbl)))
    stop("rwtbl should already have groups when `apply_summary()` is called")
  
  # slot_agg_row should either by NA, or a string for an existing "summary" 
  # type function
  
  if (!is.na(slot_agg_row$summary))
    rwtbl <- summary_summarise(rwtbl, slot_agg_row$summary)
  
  rwtbl
}

summary_summarise <- function(rwtbl, sam_summary)
{
  if (!exists(sam_summary, mode = "function"))
    stop("specified `summary`: ", sam_summary, 
         " does not match existing functions.\n",
         "   Please see ?XXX for help.", call. = FALSE)
  
  smry_fun <- tryCatch(
    eval(parse(text = sam_summary)), 
    error = function(c) -1
  )

  check_summary_function(smry_fun, sam_summary)
  
  # ** need to consider how to group/not lose the other column names that were 
  # included in rwtbl
  
  rwtbl %>%
    dplyr::summarise_at("Value", .funs = dplyr::funs("Value" = smry_fun(.)))
}

check_summary_function <- function(smry_fun, sam_summary)
{
  ftxt <- paste0("`", sam_summary, "()` ")
  
  if (is.numeric(smry_fun) && smry_fun == 1)
    stop(ftxt, "exists, but could not be evaluated.", call. = FALSE)
  
  s1 <- tryCatch(smry_fun(5), error = function(c) "error")
  s2 <- tryCatch(smry_fun(0:12), error = function(c) "error")
  s3 <- tryCatch(smry_fun(-24), error = function(c) "error")
  s4 <- tryCatch(smry_fun(-34:1), error = function(c) "error")
  sL <- list(s1, s2, s3, s4)
  
  areErrors <- simplify2array(lapply(1:4, function(x) {
    (is.character(sL[[x]]) && sL[[x]] == "error")
  }))
  
  if (any(areErrors))
    stop(ftxt, "resulted in an error for the simple test cases.")
  
  areErrors <- simplify2array(lapply(1:4, function(x) length(sL[[x]]) != 1))
  
  if (any(areErrors))
    stop(ftxt, "returns more than 1 value for a vector")
}

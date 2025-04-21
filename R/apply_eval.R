
#' Given the `eval` and `t_s` columns, evaluate the comparison threshold or
#' multiply by the scalilng factor if now comparison threshold exists. If 
#' using a comparison threshold, will return either 0 or 1 instead of FALSE or
#' TRUE.
#' 
#' @noRd

apply_eval <- function(rwtbl, slot_agg_row)
{
  check_eval_and_t_s(slot_agg_row)
  
  eval_fun <- slot_agg_row$eval
  eval_fun <- gsub("\\s+", "", eval_fun) # remove white space
  t_s <- slot_agg_row$t_s
  
  if (is.na(eval_fun) & !is.na(t_s))
    eval_fun <- "*"
  
  if (!is.na(eval_fun) & !is.na(t_s)) {
    if (eval_fun %in% c(methods::getGroupMembers("Compare"), "*")) {
      rwtbl <- rwtbl %>%
        dplyr::mutate_at(
          "Value", 
          list(~as.numeric(eval(parse(text = paste(., eval_fun, t_s)))))
        )
    } else {
      # it is one of the between operators: [], [), (], ()
      vals <- strsplit(t_s, '-')[[1]]
      num1 <- as.numeric(vals[1])
      num2 <- as.numeric(vals[2])
      o1 <- substr(eval_fun, 1, 1)
      o2 <- substr(eval_fun, 2, 2)
      o1 <- if_else(o1 == "[", ">=", ">")
      o2 <- if_else(o2 == "]", "<=", "<")
      
      rwtbl <- rwtbl %>%
        dplyr::mutate_at(
          "Value", 
          list(~as.numeric(eval(parse(
            text = paste(., o1, num1, '&', ., o2, num2)
          ))))
        )
    }
  }
  
  rwtbl
}

#' Ensure that `eval` and `t_s` columns in a slot agg matrix are valid
#'
#' @noRd

check_eval_and_t_s <- function(slot_agg_row)
{
  # eval column should be NA or "<", "<=", ">", ">=", "!=", "=="
  
  eval_col <- slot_agg_row$eval
  eval_col <- gsub("\\s+", "", eval_col) # remove white space
  
  btwn_evals <- c("[]", "[)", "(]", "()")
  valid_evals <- c(methods::getGroupMembers("Compare"), btwn_evals)
   
  if (!is.na(eval_col) & !(eval_col %in% valid_evals)) {
    stop(
      "'", eval_col, "' is not a valid `eval` value.\n",
      "The `eval` column in the slot agg matrix should either be\n",
      "`NA`, one of the 'Compare' S4 group generics (See ?S4groupGeneric) or:",
      "[], [), (], ().",
      call. = FALSE
    )
  }
  
  # if eval is NA, then t_s can either be na or a numeric; 
  # if eval is a Compare generic, then t_s must be a numeric
  # if eval is the custom between operators, then it should be number-number
  
  t_s <- slot_agg_row$t_s
  # TODO: add in a strsplit(.,'-') to handle the 2 numbers for the between 
  # operator only. Probably have it check if it is one of the between operators
  # first
  
  if (eval_col %in% btwn_evals) {
    t_s <- tryCatch(
      as.numeric(strsplit(t_s,'-')[[1]]),
      error = function(c) NaN, 
      warning = function(c) NaN
    )
    
    if (is.nan(t_s) | length(t_s) != 2) {
      stop(
        "'", slot_agg_row$t_s, "' is not a valid `t_s` value.\n",
        "For between operators for the eval column, i.e., [], [), (], or (),\n",
        "the `t_s` column in the slot agg matrix must be number-number, e.g., \n",
        "1000-1020."
      )
    }
  } else {
    if (!is.na(t_s)) {
      t_s <- tryCatch(
        as.numeric(t_s), 
        error = function(c) NaN, 
        warning = function(c) NaN
      )
    }
  }
  
  if (is.nan(t_s))
    stop(
      "'", slot_agg_row$t_s, "' is not a valid `t_s` value.\n",
      "The `t_s` column in the slot agg matrix should either be\n",
      "`NA` or a numerical value.", 
      call. = FALSE
    )
  
  if (!is.na(eval_col) & is.na(t_s))
    stop("When the `eval` column is a comparison function, the `t_s` column\n",
         "must be a numerical value.", call. = FALSE)
  
  invisible(slot_agg_row)
}

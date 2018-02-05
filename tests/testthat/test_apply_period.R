
context("check the period_apply function")
library(dplyr)

# file             slot                 period   summary  eval  t_s  variable
slot_agg_matrix <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "min", "<=", "1000", "peLt1000",  
  "KeySlots.rdf", "Mead.Pool Elevation", "eocy", NA, "none", "NA", "peEocy",
  "KeySlots.rdf", "Powell.Outflow", "July", NA, "none", "NA", "julyRel",
  "KeySlots.rdf", "Powell.Outflow", "asis", NA, "<=", "400000", "pMonthlyLt400k",
  "KeySlots.rdf", "Powell.Outflow", "wy", "sum", "none", ".001", "pwyRel"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(slot_agg_matrix) <- c("file", "slot", "period", "summary", "eval", "t_s", "variable")

rwtbl <- rw_rdf_to_tbl(keyRdf)

# check pre-specified periods --------------------------
test_that("period_apply works with pre-specified periods", {
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[1,]),
    filter(rwtbl, Month %in% month.name, ObjectSlot == slot_agg_matrix[1,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[2,]),
    filter(rwtbl, Month %in% "December", ObjectSlot == slot_agg_matrix[2,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[3,]),
    filter(rwtbl, Month %in% "July", ObjectSlot == slot_agg_matrix[3,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[4,]),
    filter(rwtbl, ObjectSlot == slot_agg_matrix[4,]$slot) %>%
      group_by(Year, Month)
  )
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[5,]),
    filter(rwtbl, ObjectSlot == slot_agg_matrix[5,]$slot) %>%
      mutate(ym = zoo::as.yearmon(Timestep)) %>%
      mutate(Year = getWYFromYearmon(ym)) %>%
      select(-ym) %>%
      group_by(Year)
  )
})

# check custom functions -------------------------------
slot_agg_matrix <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "summer", "max", ">=", "1100", "peGt1100",
  "KeySlots.rdf", "Powell.Outflow", "djf", "sum", "none", "none", "djrRel"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(slot_agg_matrix) <- c("file", "slot", "period", "summary", "eval", "t_s", "variable")

# **** add these to vignette

# globally defined, b/c user would be adding it to the global workspace
summer <<- function()
{
  list(
    fun = function(x) x, 
    filter_months = c("July", "August", "May"), 
    group_tbl = c("Year")
  )
}

# the previous year to go with Jan and Feb of the current year
djf <<- function()
{
  djf_convert <- function(rwtbl)
  {
    rwtbl %>%
      dplyr::mutate_at("Timestep", .funs = dplyr::funs("ym" = zoo::as.yearmon)) %>%
      # can use the getWYFromYearmon b/c djf are all in same water year
      dplyr::mutate_at("ym", .funs = dplyr::funs("Year" = getWYFromYearmon)) %>%
      dplyr::select(-dplyr::one_of("ym"))
  }
  
  list(fun = djf_convert, filter_months = month.name[c(12, 1, 2)], group_tbl = c("Year"))
  
}

on.exit(rm(summer, djf, envir = globalenv()))

test_that("custom functions work for period_apply", {
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[1,]),
    rwtbl %>% 
      filter(ObjectSlot == slot_agg_matrix[1,]$slot, 
             Month %in% summer()$filter_months) %>%
      group_by(Year)
  )
  
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[2,]),
    rwtbl %>% 
      filter(ObjectSlot == slot_agg_matrix[2,]$slot, 
             Month %in% djf()$filter_months) %>%
      mutate(ym = zoo::as.yearmon(Timestep)) %>%
      mutate(Year = getWYFromYearmon(ym)) %>%
      select(-ym) %>%
      group_by(Year)
  )
})

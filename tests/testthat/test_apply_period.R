
context("check the period_apply function")
library(dplyr)

# file             slot                 period   summary  eval  t_s  variable
slot_agg_matrix <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "sum", "<=", "1000", "peLt1000",  
  "KeySlots.rdf", "Mead.Pool Elevation", "eocy", "none", "none", "NA", "peEocy",
  "KeySlots.rdf", "Powell.Outflow", "July", "none", "none", "NA", "julyRel",
  "KeySlots.rdf", "Powell.Outflow", "asis", "none", "<=", "400000", "pMonthlyLt400k",
  "KeySlots.rdf", "Powell.Outflow", "wy", "sum", "none", ".001", "pwyRel"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(slot_agg_matrix) <- c("file", "slot", "period", "summary", "eval", "t_s", "variable")

tbl <- rw_rdf_to_tbl(keyRdf)

# check pre-specified periods --------------------------
test_that("period_apply works with pre-specified periods", {
  expect_identical(
    RWDataPlyr:::apply_period(tbl, slot_agg_matrix[1,]),
    filter(tbl, Month %in% month.name, ObjectSlot == slot_agg_matrix[1,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(tbl, slot_agg_matrix[2,]),
    filter(tbl, Month %in% "December", ObjectSlot == slot_agg_matrix[2,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(tbl, slot_agg_matrix[3,]),
    filter(tbl, Month %in% "July", ObjectSlot == slot_agg_matrix[3,]$slot) %>%
      group_by(Year)
  )
  expect_identical(
    RWDataPlyr:::apply_period(tbl, slot_agg_matrix[4,]),
    filter(tbl, ObjectSlot == slot_agg_matrix[4,]$slot) %>%
      group_by(Year, Month)
  )
  expect_identical(
    RWDataPlyr:::apply_period(tbl, slot_agg_matrix[5,]),
    filter(tbl, ObjectSlot == slot_agg_matrix[5,]$slot) %>%
      mutate(ym = zoo::as.yearmon(Timestep)) %>%
      mutate(Year = getWYFromYearmon(ym)) %>%
      select(-ym) %>%
      group_by(Year)
  )
})

# check custom functions -------------------------------
# **** add these to vignette
summer <- function()
{
  c("July", "August", "May")
}

# **** need to consider how this would work since you would want December of
# the previous year to go with Jan and Feb of the current year
djf <- function()
{
  c("December", "January", "February")
  
  # # ensure that it still has the appropriate attributes --------------
}

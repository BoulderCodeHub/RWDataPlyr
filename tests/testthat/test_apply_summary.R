context("check the apply_summary function")
library(dplyr)

# file             slot                 period   summary  eval  t_s  variable
slot_agg_matrix <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "min", "<=", "1000", "peLt1000",  
  "KeySlots.rdf", "Mead.Pool Elevation", "eocy", NA, "none", "NA", "peEocy",
  "KeySlots.rdf", "Powell.Outflow", "July", NA, "none", "NA", "julyRel",
  "KeySlots.rdf", "Powell.Outflow", "asis", NA, "<=", "400000", "pMonthlyLt400k",
  "KeySlots.rdf", "Powell.Outflow", "wy", "sum", "none", ".001", "pwyRel"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(slot_agg_matrix) <- c("file", "slot", "period", "summary", "eval", 
                               "t_s", "variable")

rwtbl <- rw_rdf_to_tbl(keyRdf)

# check apply_summary --------------------------
test_that("apply_summary works as expected", {
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[1,]) %>%
      RWDataPlyr:::apply_summary(slot_agg_matrix[1,]),
    rwtbl %>%
      filter(Month %in% month.name, ObjectSlot == slot_agg_matrix[1,]$slot) %>%
      group_by(Year, TraceNumber, ObjectSlot) %>%
      summarise(Value = min(Value))
  )
  
  expect_equal(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[2,]) %>%
      RWDataPlyr:::apply_summary(slot_agg_matrix[2,]),
    rwtbl %>%
      filter(Month %in% "December", ObjectSlot == slot_agg_matrix[2,]$slot) %>%
      select(-Timestep, -Month) %>%
      group_by(Year, TraceNumber)
      
  )
  
  expect_equal(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[3,]) %>%
      RWDataPlyr:::apply_summary(slot_agg_matrix[3,]),
    rwtbl %>%
      filter(Month %in% "July", ObjectSlot == slot_agg_matrix[3,]$slot) %>%
      group_by(Year, TraceNumber) %>%
      select(-Timestep, -Month)
  )
  
  expect_equal(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[4,]) %>%
      RWDataPlyr:::apply_summary(slot_agg_matrix[4,]),
    filter(rwtbl, ObjectSlot == slot_agg_matrix[4,]$slot) %>%
      group_by(Year, Month, TraceNumber) %>%
      select(-Timestep)
  )
  
  expect_identical(
    RWDataPlyr:::apply_period(rwtbl, slot_agg_matrix[5,]) %>%
      RWDataPlyr:::apply_summary(slot_agg_matrix[5,]),
    rwtbl %>%
      filter(ObjectSlot == slot_agg_matrix[5,]$slot) %>%
      mutate(ym = zoo::as.yearmon(Timestep)) %>%
      mutate(Year = getWYFromYearmon(ym)) %>%
      select(-ym) %>%
      group_by(Year, TraceNumber, ObjectSlot) %>%
      summarise(Value = sum(Value))
  )
})

# check apply_summary errors --------------------------
sam <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "range", "<=", "1000", "peLt1000",
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "blah", "<=", "1000", "peLt1000",
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "which", "<=", "1000", "peLt1000",
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "match", "<=", "1000", "peLt1000"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(sam) <- c("file", "slot", "period", "summary", "eval", 
                               "t_s", "variable")
test_that("apply_summary errors correctly", {
  expect_error(
    RWDataPlyr:::apply_period(rwtbl, sam[1,]) %>%
       RWDataPlyr:::apply_summary(sam[1,]),
    "`range()` returns more than 1 value for a vector",
    fixed = TRUE
  )
  
  expect_error(
    RWDataPlyr:::apply_period(rwtbl, sam[2,]) %>%
      RWDataPlyr:::apply_summary(sam[2,]),
    paste0("specified `summary`: blah does not match existing functions.\n",
      "   Please see ?XXX for help."),
    fixed = TRUE
  )
  
  expect_error(
    RWDataPlyr:::apply_period(rwtbl, sam[2,]) %>%
      ungroup() %>%
      RWDataPlyr:::apply_summary(sam[2,]),
    "rwtbl should already have groups when `apply_summary()` is called",
    fixed = TRUE
  )
  
  expect_error(
    RWDataPlyr:::apply_period(rwtbl, sam[3,]) %>%
      RWDataPlyr:::apply_summary(sam[3,]),
    paste0("`which()` resulted in an error for the simple test cases.\n",
      "Ensure that it only requires one vector as its arguement."),
    fixed = TRUE
  )
  
  expect_error(
    RWDataPlyr:::apply_period(rwtbl, sam[4,]) %>%
      RWDataPlyr:::apply_summary(sam[4,]),
    paste0("`match()` resulted in an error for the simple test cases.\n",
           "Ensure that it only requires one vector as its arguement."),
    fixed = TRUE
  )
})


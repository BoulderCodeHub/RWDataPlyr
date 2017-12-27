library(RWDataPlyr)
library(dplyr)

context("check that RWDataPlyr:::getSlots() works as intended")

sal <- createSlotAggList(matrix(
  c('KeySlots.rdf','Powell.Pool Elevation','EOCY',NA,"pe",
    "KeySlots.rdf", "no.Pool Elevation", "EOCY", NA, "no"),
  nrow = 2,
  byrow = TRUE
))[[1]]
df <- RWDataPlyr:::getSlots(
  sal, 
  scenPath = system.file("extdata/Scenario/DNF,CT,IG", package = "RWDataPlyr"),
  findAllSlots = FALSE
)

test_that("data is returned when one slot cannot be found", {
  expect_equal(dim(df), c(25*20 + 1, 4))
  expect_equal(dim(filter(df, Variable == "no")), c(1,4))
  expect_equal(filter(df, Variable == "no")$Trace, -99)
  expect_equal(filter(df, Variable == "no")$Year, -99)
  expect_equal(filter(df, Variable == "no")$Value, -99)
})

test_that("still get an error when second slot is the one that cannot be found", {
  expect_error(RWDataPlyr:::getSlots(
    sal, 
    scenPath = system.file("extdata/Scenario/DNF,CT,IG", package = "RWDataPlyr"),
    findAllSlots = TRUE
  ),
  paste("slot:", "no.Pool Elevation", "not found in rdf:", "KeySlots.rdf"))
})

sal <- createSlotAggList(matrix(
  c('KeySlots.rdf','Powell.Pool Elevation','EOCY',NA,"pe",
    "KeySlots.rdf", "no.Pool Elevation", "EOCY", NA, "no",
    "KeySlots.rdf", "nonono.Pool Elevation", "EOCY", NA, "no2"),
  nrow = 3,
  byrow = TRUE
))[[1]]
df <- RWDataPlyr:::getSlots(
  sal, 
  scenPath = system.file("extdata/Scenario/DNF,CT,IG", package = "RWDataPlyr"),
  findAllSlots = FALSE
)

test_that("getSlots works with multiple unfound slots", {
  expect_equal(dim(df), c(25 * 20 + 2, 4))
  expect_equal(dim(filter(df, Year == -99)), c(2,4))
})

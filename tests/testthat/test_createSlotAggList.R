library(RWDataPlyr)
context('check creation of slot aggregation list')

test_that("createSlotAggList returns proper errors", {
  expect_error(createSlotAggList(matrix(1:6, ncol = 3)), 
               "iData is a matrix with 3 columns. There should either be 4 or 5 columns.")
  expect_error(createSlotAggList(matrix(1:6, ncol = 6)), 
               "iData is a matrix with 6 columns. There should either be 4 or 5 columns.")
  expect_error(createSlotAggList(1:3), 
               "iData is not a matrix, nor can it be converted to an Nx4 or Nx5 matrix")
  expect_error(createSlotAggList(1:17),
               "iData is not a matrix, nor can it be converted to an Nx4 or Nx5 matrix")
  expect_warning(createSlotAggList(1:8),
                 "Attempting to convert iData to a N x 4 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:4),
                 "Attempting to convert iData to a N x 4 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:15),
                 "Attempting to convert iData to a N x 5 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:5),
                 "Attempting to convert iData to a N x 5 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_error(createSlotAggList(file.path("some", "crazy", "file.txt")),
               paste(file.path("some", "crazy", "file.txt"),'does not exist.'))
})


sal <- createSlotAggList(system.file("extdata", "SlotAggTable.csv", package = "RWDataPlyr"))
sam <- matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "EOCY", NA, "meadPe",
  "SystemConditions.rdf", "Shortage.ShortageFlag", "AnnualRaw", 100, "lbShort",
  "KeySlots.rdf", "Powell.Pool Elevation", "EOCY", NA, "powellPe"
), ncol = 5, byrow = TRUE)
sal2 <- createSlotAggList(sam)

test_that("format of createSlotAggList is correct", {
  expect_equal(length(sal), 1)
  expect_equal(length(sal[[1]]$rdf), 1)
  expect_equal(length(sal[[1]]$slots), 4)
  expect_equal(nrow(sal[[1]]$annualize), 2)
  expect_equal(ncol(sal[[1]]$annualize), 4)
  expect_equal(length(sal[[1]]$varNames), 4)
  expect_true(all(is.na(sal[[1]]$varNames)))
  
  expect_equal(length(sal2), 2)
  expect_equal(length(sal2[[1]]$rdf), 1)
  expect_equal(length(sal2[[1]]$slots), 2)
  expect_equal(nrow(sal2[[1]]$annualize), 2)
  expect_equal(ncol(sal2[[1]]$annualize), 2)
  expect_equal(length(sal2[[1]]$varNames), 2)
  expect_false(all(is.na(sal2[[1]]$varNames)))
  expect_equal(length(sal2[[2]]$rdf), 1)
  expect_equal(length(sal2[[2]]$slots), 1)
  expect_equal(nrow(sal2[[2]]$annualize), 2)
  expect_equal(ncol(sal2[[2]]$annualize), 1)
  expect_equal(length(sal2[[2]]$varNames), 1)
  expect_false(all(is.na(sal2[[2]]$varNames)))
})

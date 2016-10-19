library(RWDataPlot)
context('check rdfSlotToMatrix')

test_that("rdfSlotToMatrix gets correct dimensions for matrix", {
  expect_equal(dim(rdfSlotToMatrix(keyRdf,'Powell.Pool Elevation')),c(240,25))
  expect_equal(dim(rdfSlotToMatrix(sysRdf,'SummaryOutputData.LBShortageConditions')),c(20,25))
})

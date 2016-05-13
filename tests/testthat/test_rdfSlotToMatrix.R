library(RWDataPlot)
context('rdfSlotToMatrix')

test_that("rdfSlotToMatrix gets correct dimensions for matrix", {
  expect_equal(dim(rdfSlotToMatrix(keyRdf,'Powell.Pool Elevation')),c(132,25))
  expect_equal(dim(rdfSlotToMatrix(sysRdf,'SummaryOutputData.LBShortageConditions')),c(11,25))
})

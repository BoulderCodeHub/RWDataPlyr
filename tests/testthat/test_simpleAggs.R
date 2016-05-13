library(RWDataPlot)
context('check simple aggregation methods')

simpMat <- matrix(1:48, ncol = 2)

test_that("getAnnMinValue returns corect values", {
  expect_equal(getAnnMinValue(simpMat), matrix(c(1,13,25,37),ncol=2))
})

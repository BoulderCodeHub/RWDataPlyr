library(RWDataPlot)
context('check simple aggregation methods')

simpMat <- matrix(1:48, ncol = 2)
pe <- rdfSlotToMatrix(keyRdf, 'Powell.Pool Elevation')[1:36,1:4]

test_that("getMinAnnValue returns corect values", {
  expect_equal(getMinAnnValue(simpMat), matrix(c(1,13,25,37),ncol=2))
  expect_equal(getMinAnnValue(pe), rbind(apply(pe[1:12,],2,min), apply(pe[13:24,],2,min), 
                                         apply(pe[25:36,],2,min)))
})

test_that("getMaxAnnValue returns corect values", {
  expect_equal(getMaxAnnValue(simpMat), matrix(c(12,24,36,48),ncol=2))
  expect_equal(getMaxAnnValue(pe), rbind(apply(pe[1:12,],2,max), apply(pe[13:24,],2,max), 
                                         apply(pe[25:36,],2,max)))
})

test_that("sumMonth2Annual returns corect values", {
  expect_equal(sumMonth2Annual(simpMat), matrix(c(sum(1:12),sum(13:24),sum(25:36),sum(37:48)),ncol=2))
  expect_equal(sumMonth2Annual(pe), rbind(apply(pe[1:12,],2,sum), apply(pe[13:24,],2,sum), 
                                         apply(pe[25:36,],2,sum)))
})

flow <- rep(c(700,800,800,900,750,900),2)*1000
saltMass <- c(25,30,35,25,25,22,35,47,21,45,34,23)*1000

test_that("Flow weighted annual average concentration returns correct Values", {
  expect_equal(round(flowWeightedAvgAnnConc(saltMass, flow),5),27.82642)
  expect_error(flowWeightedAvgAnnConc(saltMass[1:11],flow), 
               'Data passed to flowWeightedAvgAnnConc is not divisible by 12')
  expect_error(flowWeightedAvgAnnConc(saltMass,flow[1:11]), 
               'Data passed to flowWeightedAvgAnnConc is not divisible by 12')
})

library(RWDataPlyr)
context('check rdf helper functions')

# the expected number of years and traces in the sample data in extdata/...
nYrs <- 5
nTraces <- 4

key <- read.rdf2(system.file("extdata/Scenario/T13,CT,IG", "KeySlots.rdf", package = "RWDataPlyr"))
sys <- read.rdf2(system.file("extdata/Scenario/T13,CT,IG", "SystemConditions.rdf", package = "RWDataPlyr"))

test_that("rdfSlotToMatrix gets correct dimensions for matrix", {
  expect_equal(dim(rdfSlotToMatrix(keyRdf,'Mead.Pool Elevation')),c(nYrs*12,nTraces))
  expect_equal(dim(rdfSlotToMatrix(sysRdf,'SummaryOutputData.LBShortageConditions')),c(nYrs,nTraces))
  expect_equal(dim(rdfSlotToMatrix(key,'Mead.Pool Elevation')),c(240,1))
  expect_equal(dim(rdfSlotToMatrix(sys,'SummaryOutputData.LBShortageConditions')),c(20,1))
})

test_that('getTimeSpan returns expected dates', {
  # keyRdf includes monthly data so it starts in January. 
  # sysRdf includes annual data, so it starts in December.
  expect_equal(getTimeSpan(keyRdf), c('start' = '2018-1-31 24:00', 'end' = '2022-12-31 24:00'))
  expect_equal(getTimeSpan(sysRdf), c('start' = '2018-12-31 24:00', 'end' = '2022-12-31 24:00'))
})

test_that('getTimeSpan returns correct length', {
  expect_equal(length(getTimeSpan(keyRdf)),2)
})

test_that('getSlotsInRdf returns expected slot names', {
  # use all and check if it's in the list. order does not matter.
  expect_equal(all(getSlotsInRdf(keyRdf) %in% c('Mead.Pool Elevation',
                                                'Powell.Outflow')),TRUE)
})

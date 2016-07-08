library(RWDataPlot)
context('check read.rdf2 matches read.rdf')

r1 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','KeySlots.rdf'),
                           package = 'RWDataPlot'))
r2 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','SystemConditions.rdf'),
                           package = 'RWDataPlot'))

r3 <- read.rdf2(system.file('extdata',file.path('Scenario','DNF,CT,IG','KeySlots.rdf'),
                           package = 'RWDataPlot'))
r4 <- read.rdf2(system.file('extdata',file.path('Scenario','DNF,CT,IG','SystemConditions.rdf'),
                           package = 'RWDataPlot'))

test_that('lists are identical', {
  expect_equal(r1,r1)
  expect_equal(r2,r2)
})

test_that('getting data matches', {
  expect_equal(rdfSlotToMatrix(r1, 'Mead.Pool Elevation'), 
               rdfSlotToMatrix(r3, 'Mead.Pool Elevation'))
  expect_equal(rdfSlotToMatrix(r1, "SummaryOutputData.UpperBalancingAbove823"), 
               rdfSlotToMatrix(r3, "SummaryOutputData.UpperBalancingAbove823"))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.LBShortageStep2"), 
               rdfSlotToMatrix(r4, "SummaryOutputData.LBShortageStep2"))
})

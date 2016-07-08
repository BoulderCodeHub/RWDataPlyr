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

test_that('list components match', {
  expect_equal(r1$meta, r3$meta)
  expect_equal(length(r1$runs), length(r1$runs))
  expect_equal(names(r1$runs[[1]]), names(r3$runs[[1]]))
  expect_equal(r1$runs[[1]]$start, r3$runs[[1]]$start)
  expect_equal(r1$runs[[1]]$end, r3$runs[[1]]$end)
  expect_equal(names(r1$runs[[1]]$objects), names(r3$runs[[1]]$objects))
  expect_equal(r1$runs[[1]]$objects[['Mead.Pool Elevation']]$values,
               r3$runs[[1]]$objects[['Mead.Pool Elevation']]$values)
})

test_that('getting data matches', {
  expect_equal(rdfSlotToMatrix(r1, 'Mead.Pool Elevation'), 
               rdfSlotToMatrix(r3, 'Mead.Pool Elevation'))
  expect_equal(rdfSlotToMatrix(r1, "SummaryOutputData.UpperBalancingAbove823"), 
               rdfSlotToMatrix(r3, "SummaryOutputData.UpperBalancingAbove823"))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.LBShortageStep2"), 
               rdfSlotToMatrix(r4, "SummaryOutputData.LBShortageStep2"))
})

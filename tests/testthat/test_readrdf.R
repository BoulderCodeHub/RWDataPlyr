library(RWDataPlot)
context('check read.rdf')

r1 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','KeySlots.rdf'),
                           package = 'RWDataPlot'))
r2 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','SystemConditions.rdf'),
                           package = 'RWDataPlot'))

test_that('read.rdf data are as expected', {
  expect_equal(rdfSlotToMatrix(r1, 'Powell.Pool Elevation'),
               rdfSlotToMatrix(keyRdf, 'Powell.Pool Elevation'))
  expect_equal(rdfSlotToMatrix(r1, 'Mead.Pool Elevation'),
               rdfSlotToMatrix(keyRdf, 'Mead.Pool Elevation'))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.LBShortageConditions"),
               rdfSlotToMatrix(sysRdf, "SummaryOutputData.LBShortageConditions"))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.MidElevationReleaseAt748"),
               rdfSlotToMatrix(sysRdf, "SummaryOutputData.MidElevationReleaseAt748"))
})

exptSlots <- c('Powell.Pool Elevation', 'Mead.Pool Elevation', 'Powell.Outflow')  

test_that('slots in rdf are those that are expected (getSlotsinRdf)', {
  expect_equal(length(getSlotsInRdf(keyRdf)),length(exptSlots))
  expect_equal(sum(getSlotsInRdf(keyRdf) %in% exptSlots), length(getSlotsInRdf(keyRdf)))
})


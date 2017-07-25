library(RWDataPlyr)
context('check read.rdf2')

r1 <- read.rdf2(system.file('extdata',file.path('Scenario','ISM1988_2014,2007Dems,IG,Most','KeySlots.rdf'),
                           package = 'RWDataPlyr'))
r2 <- read.rdf2(system.file('extdata',file.path('Scenario','ISM1988_2014,2007Dems,IG,Most','SystemConditions.rdf'),
                           package = 'RWDataPlyr'))

test_that('read.rdf data are as expected', {
  expect_equal(rdfSlotToMatrix(r1, 'Powell.Outflow'),
               rdfSlotToMatrix(keyRdf, 'Powell.Outflow'))
  expect_equal(rdfSlotToMatrix(r1, 'Mead.Pool Elevation'),
               rdfSlotToMatrix(keyRdf, 'Mead.Pool Elevation'))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.LBShortageConditions"),
               rdfSlotToMatrix(sysRdf, "SummaryOutputData.LBShortageConditions"))
  expect_equal(rdfSlotToMatrix(r2, "SummaryOutputData.MidElevationReleaseAt748"),
               rdfSlotToMatrix(sysRdf, "SummaryOutputData.MidElevationReleaseAt748"))
})

exptSlots <- c('Mead.Pool Elevation', 'Powell.Outflow') 

test_that('slots in rdf are those that are expected (getSlotsinRdf)', {
  expect_equal(length(getSlotsInRdf(keyRdf)),length(exptSlots))
  expect_equal(sum(getSlotsInRdf(keyRdf) %in% exptSlots), length(getSlotsInRdf(keyRdf)))
})


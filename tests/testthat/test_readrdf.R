library(RWDataPlot)
context('check read.rdf')

r1 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','KeySlots.rdf'),
                           package = 'RWDataPlot'))
r2 <- read.rdf(system.file('extdata',file.path('Scenario','DNF,CT,IG','SystemConditions.rdf'),
                           package = 'RWDataPlot'))

test_that("read.rdf format is unchanged", {
  expect_equal(r1, keyRdf)
  expect_equal(r2, sysRdf)
})

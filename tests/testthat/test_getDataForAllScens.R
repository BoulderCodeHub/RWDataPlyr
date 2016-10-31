library(RWDataPlyr)
context('check that getDataForAllScens works')

# get a specified set of slots and apply some aggregation method to them
scenNames <- scenFolders <- c('DNF,CT,IG') 

slotAggList <- createSlotAggList(system.file('extdata','SlotAggTable.csv',package = 'RWDataPlyr'))
scenPath <- system.file('extdata','Scenario/',package = 'RWDataPlyr')
oFile <- 'tmp.txt'
retFile <- TRUE # return the data, instead of only save it as a text file
keyData <- getDataForAllScens(scenFolders, scenNames, slotAggList, scenPath, oFile, retFile)

slotAggList <- list(list(rdf = 'KeySlots.rdf', slots = 'all'))
# will return monthly data for all slots in KeySlots.rdf
allData <- getDataForAllScens(scenFolders, scenNames, slotAggList, scenPath, oFile, retFile)

expectedSlotNames <- sort(paste(getSlotsInRdf(keyRdf),'Monthly','1',sep='_'))

test_that("getting all slot data from RDF does actually return all slots", {
  expect_equal(levels(as.factor(allData$Variable)),expectedSlotNames)
})

test_that("getting all slot data is the same as using a pre-configured slotAggList", {
  expect_equal(dplyr::filter(keyData, Variable == 'Powell.Pool Elevation_EOCY_1')$Value,
               dplyr::filter(allData, Variable == 'Powell.Pool Elevation_Monthly_1', Month == 'Dec')$Value)
  expect_equal(dplyr::filter(keyData, Variable == 'Mead.Pool Elevation_EOCY_1')$Value,
             dplyr::filter(allData, Variable == 'Mead.Pool Elevation_Monthly_1', Month == 'Dec')$Value)
})

test_that('file extension is checked', {
  expect_error(getDataForAllScens(scenFolders, scenNames, slotAggList, scenPath, 
                                  'tst.xyz', retFile),
               paste0('oFile has an invalid file exention.\n',
                       'getDataForAllScens does not know how to handle ".', 'xyz',
                       '" extensions.'))
  expect_error(getDataForAllScens(scenFolders, scenNames, slotAggList, scenPath, 
                                  'tst.cvs', retFile),
               paste0('oFile has an invalid file exention.\n',
                      'getDataForAllScens does not know how to handle ".', 'cvs',
                      '" extensions.'))
})

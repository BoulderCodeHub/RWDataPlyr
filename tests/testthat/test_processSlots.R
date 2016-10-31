library(RWDataPlyr)
context('check that processSlots works')

# check processSlots internal function using keyRdf and sysRdf

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                  'AnnualRaw',NA,'powellPE'),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

sla2 <- createSlotAggList(matrix(c('SystemConditions.rdf', "SummaryOutputData.LBShortageConditions",
                                  'EOCY',NA), nrow=1))[[1]]
sla2 <- rbind(sla2$slots, sla2$annualize, sla2$varNames)

# test 1 and 2
test_that("warnings are posted for agg method and data timestep mismatches", {
  expect_warning(
    RWDataPlyr:::processSlots(sla, keyRdf, 'KeySlots.rdf'),
    paste('User specified aggregation is "AnnualRaw", but the rdf contains monthly data.\n',
      'Will use EOCY aggregation instead. If other aggregation method is desired, please\n',
      'edit the slot agg list and call getDataForAllScens again.')
  )
  expect_warning(
    RWDataPlyr:::processSlots(sla2, sysRdf, 'SystemConditions.rdf'),
    paste('rdf contains annual data, but the aggregation method is not "AnnualRaw".\n',
     'Processing using "AnnualRaw" instead. Edit the slotAggList and call getDataForAllScens again, if necessary.')
    )
})

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                   'EOCY',NA),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

sla2 <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                   'EOCY',NA,'powellPE'),nrow = 1))[[1]]
sla2 <- rbind(sla2$slots, sla2$annualize, sla2$varNames)

df1 <- RWDataPlyr:::processSlots(sla, keyRdf, 'KeySlots.rdf')
df2 <- RWDataPlyr:::processSlots(sla2, keyRdf, 'KeySlots.rdf')

# test 3
test_that('variable name is constructed properly', {
  expect_equal(as.character(df1$Variable[1]),'Powell.Pool Elevation_EOCY_1')
})

# test 4
test_that('results match regardless of variable name', {
  expect_equal(df1$Value, df2$Value)
})

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                  'Weird',NA),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

sla2 <- createSlotAggList(matrix(c('KeySlots.rdf','Something.Pool Elevation',
                                  'EOCY',NA),nrow = 1))[[1]]
sla2 <- rbind(sla2$slots, sla2$annualize, sla2$varNames)

# test 5 and 6
test_that('process slots stops as expected', {
  expect_error(RWDataPlyr:::processSlots(sla, keyRdf, 'KeySlots.rdf'),
               'Invalid aggregation method variable')
  expect_error(RWDataPlyr:::processSlots(sla2, keyRdf, 'KeySlots.rdf'),
               'slot: Something.Pool Elevation not found in rdf: KeySlots.rdf')
})





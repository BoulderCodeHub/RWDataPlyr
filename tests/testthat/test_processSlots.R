library(RWDataPlot)
context('check that processSlots works')

# check processSlots internal function using keyRdf

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                  'AnnualRaw',NA,'powellPE'),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

test_that("warnings are posted for agg method and data timestep mismatches", {
  expect_warning(
    RWDataPlot:::processSlots(sla, keyRdf, 'KeySlots.rdf'),
    paste('User specified aggregation is "AnnualRaw", but the rdf contains monthly data.\n',
      'Will use EOCY aggregation instead. If other aggregation method is desired, please\n',
      'edit the slot agg list and call getDataForAllScens again.')
  )
  # need to test the warning method for calling a monthly aggregation method on 
  # annual data
})

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                   'EOCY',NA),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

sla2 <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                   'EOCY',NA,'powellPE'),nrow = 1))[[1]]
sla2 <- rbind(sla2$slots, sla2$annualize, sla2$varNames)

df1 <- RWDataPlot:::processSlots(sla, keyRdf, 'KeySlots.rdf')
df2 <- RWDataPlot:::processSlots(sla2, keyRdf, 'KeySlots.rdf')

test_that('variable name is constructed properly', {
  expect_equal(as.character(df1$Variable[1]),'Powell.Pool Elevation_EOCY_1')
})

test_that('results match regardless of variable name', {
  expect_equal(df1$Value, df2$Value)
})

sla <- createSlotAggList(matrix(c('KeySlots.rdf','Powell.Pool Elevation',
                                  'Weird',NA),nrow = 1))[[1]]
sla <- rbind(sla$slots, sla$annualize, sla$varNames)

sla2 <- createSlotAggList(matrix(c('KeySlots.rdf','Something.Pool Elevation',
                                  'EOCY',NA),nrow = 1))[[1]]
sla2 <- rbind(sla2$slots, sla2$annualize, sla2$varNames)

test_that('process slots stops as expected', {
  expect_error(RWDataPlot:::processSlots(sla, keyRdf, 'KeySlots.rdf'),
               'Invalid aggregation method variable')
  expect_error(RWDataPlot:::processSlots(sla2, keyRdf, 'KeySlots.rdf'),
               'slot: Something.Pool Elevation not found in rdf: KeySlots.rdf')
})

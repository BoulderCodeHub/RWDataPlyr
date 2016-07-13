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

exptSlots <- c("LBShort.AnnualMexicoShort", "LBShort.AnnualLBMexicoShort", "SummaryOutputData.LBNormalCondition",                    
  "SummaryOutputData.MidElevationReleaseAt823", "SummaryOutputData.LBShortageConditions", 
  "SummaryOutputData.UpperBalancingBelow823", "SummaryOutputData.LBSurplusConditions",                  
  "SummaryOutputData.LBShortageStep1", "SummaryOutputData.LBShortageStep2",                      
  "SummaryOutputData.EqualizationAt823", "SummaryOutputData.UpperBalancingAbove823",               
  "SummaryOutputData.LBShortageStep3", "SummaryOutputData.UpperBalancingAt823",                  
  "SummaryOutputData.MidElevationReleaseAt748", "SummaryOutputData.EqualizationAbove823",                 
  "SummaryOutputData.LowerBalancingAbove823", "SummaryOutputData.LowerBalancingBelow823",               
  "SummaryOutputData.LowerBalancingAt823", "SummaryOutputData.LBFloodControlSurplus",                
  "PowellForecastReleaseData.RemainingWYReleaseConstrained", "PowellForecastReleaseData.ForecastWYRelease",
  "PowellForecastReleaseData.RemainingWYReleaseForecast", "PowellForecastReleaseData.ForecastWYReleaseConstrained",
  "TotVal.Powell", "Surplus.SurplusFlag", "Surplus.Flood Control Surplus Flag", "LBParameters.Adjusted Dec Elevation",                    
  "LBParameters.ProtectionScenario", "LBParameters.ProtectElevation", "LBParameters.AdjBasedOnPowellRel",                     
  "LBParameters.IID Annual Schedule", "LBParameters.ProtectionVolume", "LBParameters.IID Annual Depletion",                    
  "Shortage.ShortageFlag", "PowellOperation.Powell10YearWYRelease", "PowellOperation.PowellWYRelease",                        
  "Mead.Pool Elevation", "Powell.Pool Elevation", "LBSavings.Inflow")  

test_that('slots in rdf are those that are expected (getSlotsinRdf)', {
  expect_equal(length(getSlotsInRdf(keyRdf)),length(exptSlots))
  expect_equal(sum(getSlotsInRdf(keyRdf) %in% exptSlots), length(getSlotsInRdf(keyRdf)))
  expect_equal(listSlots(sysRdf), getSlotsInRdf(sysRdf))
})


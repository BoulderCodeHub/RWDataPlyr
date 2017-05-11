library(RWDataPlyr)
library(dplyr)
library(tidyr)

context('check RWDataPlyr:::processSlots aggregation computations')

# -----------------------------------
# hand compute values
# keyRdf exists as data in package
mReg <- rdfSlotToMatrix(keyRdf, "Mead.Pool Elevation")
pReg <- rdfSlotToMatrix(keyRdf, "Powell.Pool Elevation")
lbShort <- as.data.frame(rdfSlotToMatrix(sysRdf, "SummaryOutputData.LBShortageConditions"))
cNames <- 1:ncol(mReg)
colnames(mReg) <- cNames
colnames(pReg) <- cNames
colnames(lbShort) <- cNames

mEocy <- as.data.frame(mReg[seq(12,nrow(mReg), 12),])
pMin <- as.data.frame(getMinAnnValue(pReg))
pEowy <- as.data.frame(pReg[seq(9,nrow(pReg),12),])
mBocy <- as.data.frame(mReg[seq(1, nrow(mReg), 12),])
mMax <- as.data.frame(getMaxAnnValue(mReg))
mSum <- as.data.frame(sumMonth2Annual(mReg))
p3575 <- as.data.frame((getMinAnnValue(pReg) <= 3575) * 100)

pPad <- rbind(pReg[1,], pReg[1,], pReg[1,], pReg)[1:nrow(pReg),]
p3525 <- as.data.frame((getMinAnnValue(pPad) <= 3525) * 100)

p3800 <- as.data.frame((getMaxAnnValue(pPad) <= 3800) * 100)

m1050 <- as.data.frame((mReg[seq(12,nrow(mReg),12),] <= 1050) * 100)

m1100 <- as.data.frame((mReg[seq(12,nrow(mReg),12),] >= 1100) * 100)


filterVarToMatrix <- function(zz, var, scen = "DNF,CT,IG") {
  zz %>% 
    filter(Variable == var, Scenario == scen) %>%
    select(-Scenario, -Variable) %>%
    arrange(Year) %>%
    spread(Trace, Value) %>%
    select(-Year)
}

# ---------------------------------------
# create a sam that tests every one of the agg methods
# **** need to add one of the annual raw from monthly and 
#      one of the annual raw from annual
sal <- createSlotAggList(matrix(c(
  "KeySlots.rdf", "Powell.Pool Elevation", "AnnMin", NA, "powellMin",
  "KeySlots.rdf", "Powell.Pool Elevation", "EOWY", NA, "powellEowy",
  "KeySlots.rdf", "Mead.Pool Elevation", "EOCY", NA, "meadPe",
  "KeySlots.rdf", "Mead.Pool Elevation", "BOCY", NA, "meadBocy",
  "KeySlots.rdf", "Mead.Pool Elevation", "AnnMax", NA, "meadMax",
  "KeySlots.rdf", "Mead.Pool Elevation", "AnnualSum", NA, "meadSum",
  "KeySlots.rdf", "Powell.Pool Elevation", "AnnMinLTE", 3575, "powellLt3575",
  "KeySlots.rdf", "Powell.Pool Elevation", "WYMinLTE", 3525, "powellLt3525",
  "KeySlots.rdf", "Powell.Pool Elevation", "WYMaxLTE", 3800, "powellLt3800",
  "KeySlots.rdf", "Mead.Pool Elevation", "EOCYLTE", 1050, "meadLt1050",
  "KeySlots.rdf", "Mead.Pool Elevation", "EOCYGTE", 1100, "meadGt1100",
  "KeySlots.rdf", "Mead.Pool Elevation", "AnnualRaw", NA, "meadPe2",
  "SystemConditions.rdf", "SummaryOutputData.LBShortageConditions", "AnnualRaw", NA, "lbShort"), 
  ncol = 5, byrow = TRUE
))

# for now have to process monthly seperately
salMonthly <- createSlotAggList(matrix(c(
  "KeySlots.rdf", "Powell.Pool Elevation", "Monthly", 10, "powellMothly10",
  "KeySlots.rdf", "Mead.Pool Elevation", "Monthly", .001, "meadMonthly001"),
  ncol = 5, byrow = TRUE
))

zz <- getDataForAllScens(
  scenFolders = "DNF,CT,IG", 
  scenNames = "DNF,CT,IG", 
  slotAggList = sal, 
  scenPath = system.file('extdata','Scenario/',package = 'RWDataPlyr'),
  oFile = "tmp.feather",
  retFile = TRUE
) 

zzMonthly <- getDataForAllScens(
  scenFolders = "DNF,CT,IG", 
  scenNames = "DNF,CT,IG", 
  slotAggList = salMonthly,
  scenPath = system.file('extdata','Scenario/',package = 'RWDataPlyr'),
  oFile = "tmp.feather",
  retFile = TRUE
) %>%
  mutate(monthNum = match(Month, month.abb))

# compare the results computed by getDataForAllScen -> processSlots
# to those computed by hand using rdfSlotToMatrix
test_that("processSlots monthly to annual aggregation methods work", {
  expect_equal(filterVarToMatrix(zz, "powellMin"), pMin)
  expect_equal(filterVarToMatrix(zz, "powellEowy"), pEowy)
  expect_equal(filterVarToMatrix(zz, "meadPe"), mEocy)
  expect_equal(filterVarToMatrix(zz, "meadBocy"), mBocy)
  expect_equal(filterVarToMatrix(zz, "meadMax"), mMax)
  expect_equal(filterVarToMatrix(zz, "meadSum"), mSum)
  expect_equal(filterVarToMatrix(zz, "powellLt3575"), p3575)
  expect_equal(filterVarToMatrix(zz, "powellLt3525"), p3525)
  expect_equal(filterVarToMatrix(zz, "meadLt1050"), m1050)
  expect_equal(filterVarToMatrix(zz, "meadGt1100"), m1100)
  expect_equal(filterVarToMatrix(zz, "powellMin"), pMin)
  expect_equal(filterVarToMatrix(zz, "meadPe2"), mEocy)
  expect_equal(filterVarToMatrix(zz, "meadPe2"), filterVarToMatrix(zz, "meadPe"))
  expect_equal(filterVarToMatrix(zz, "lbShort"), lbShort)
  expect_equal({
    zzMonthly %>% 
      filter(Variable == "powellMothly10", Scenario == "DNF,CT,IG") %>%
      select(-Scenario, -Variable, -Month) %>%
      arrange(Year,monthNum) %>%
      spread(Trace, Value) %>%
      select(-Year, -monthNum)
  }, as.data.frame(pReg * 10))
  
  expect_equal({
    zzMonthly %>% 
      filter(Variable == "meadMonthly001", Scenario == "DNF,CT,IG") %>%
      select(-Scenario, -Variable, -Month) %>%
      arrange(Year,monthNum) %>%
      spread(Trace, Value) %>%
      select(-Year, -monthNum)
  }, as.data.frame(mReg / 1000))
})

# **** and check all of them for a single trace

on.exit(file.remove("tmp.feather"))
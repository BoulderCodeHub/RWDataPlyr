library(RWDataPlyr)
library(dplyr)
library(tidyr)

context('check RWDataPlyr:::processSlots aggregation computations')

# keyRdf exists as data in package
mEocy <- rdfSlotToMatrix(keyRdf, "Mead.Pool Elevation")
mEocy <- as.data.frame(mEocy[seq(12,nrow(mEocy), 12),])
colnames(mEocy) <- 1:ncol(mEocy)


# **** create a sam that tests every one of the agg methods
sal <- createSlotAggList(matrix(c(
  "KeySlots.Rdf", "Mead.Pool Elevation", "EOCY", NA, "meadPe"
), ncol = 5))

zz <- getDataForAllScens(
  scenFolders = "DNF,CT,IG", 
  scenNames = "DNF,CT,IG", 
  slotAggList = sal, 
  scenPath = system.file('extdata','Scenario/',package = 'RWDataPlyr'),
  oFile = "tmp.feather",
  retFile = TRUE
)

test_that("processSlots monthly to annual aggregation methods work", {
  expect_equal({
    zz %>% 
      filter(Variable == "meadPe", Scenario == "DNF,CT,IG") %>%
      select(-Scenario, -Variable) %>%
      arrange(Year) %>%
      spread(Trace, Value) %>%
      select(-Year)
  }, mEocy)
})

# **** also need to check the scaling

on.exit(file.remove("tmp.feather"))

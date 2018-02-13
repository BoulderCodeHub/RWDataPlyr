context("test rwtbl_summarise_all")
library(dplyr)

# file             slot                 period   summary  eval  t_s  variable
sam <- data.frame(matrix(c(
  "KeySlots.rdf", "Mead.Pool Elevation", "cy", "min", "<=", "1000", "peLt1000",  
  "KeySlots.rdf", "Mead.Pool Elevation", "eocy", NA, NA, NA, "peEocy",
  "KeySlots.rdf", "Powell.Outflow", "July", NA, NA, NA, "julyRel",
  "KeySlots.rdf", "Powell.Outflow", "asis", NA, ">", "400000", "pMonthlyGt400k",
  "KeySlots.rdf", "Powell.Outflow", "wy", "sum", NA, ".001", "pwyRel",
  "SystemConditions.rdf", "SummaryOutputData.LBShortageConditions", "asis", NA, NA, NA, "short",
  "SystemConditions.rdf", "SummaryOutputData.UpperBalancingAbove823", "asis", NA, NA, NA, "ueb823"
), ncol = 7, byrow = TRUE), stringsAsFactors = FALSE)

colnames(sam) <- c("file", "slot", "period", "summary", "eval", 
                               "t_s", "variable")
dnf2002_dir <- system.file(
  "extdata/Scenario/ISM1988_2014,2007Dems,IG,2002", 
  package = "RWDataPlyr"
)
test_that("rwtbl_summarise_all works, initially", {
  expect_s3_class(
    tmp <- rwtbl_summarise_all(
      sam,
      scen_dir = dnf2002_dir,
      scenario = "DNF2002",
      keep_cols = FALSE
    ),
    c("tbl_df")
  )
})


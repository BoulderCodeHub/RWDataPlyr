context("test rwtbl_aggregate()")
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
sam <- rwd_agg(sam)
  
dnfmost_dir <- system.file(
  "extdata/Scenario/ISM1988_2014,2007Dems,IG,Most", 
  package = "RWDataPlyr"
)

keyrwtbl <- rw_rdf_to_tbl(keyRdf)
sysrwtbl <- rw_rdf_to_tbl(sysRdf)
  
test_that("test rwtbl_aggregate() structure", {
  expect_s3_class(
    tmp <- rwtbl_aggregate(
      sam,
      scen_dir = dnfmost_dir,
      scenario = "DNFMost",
      keep_cols = FALSE
    ),
    c("tbl_df")
  )
  
  # check attribute has the sam
  expect_identical(
    attributes(tmp)$slot_agg_matrix, sam
  )
  
  # check that it contains the same attributes as keyRdf
  expect_identical(
    attributes(tmp)$rdf_atts$KeySlots.rdf$mrm_config_name, 
    attributes(keyrwtbl)$mrm_config_name
  )
  expect_identical(
    attributes(tmp)$rdf_atts$KeySlots.rdf$owner, 
    attributes(keyrwtbl)$owner
  )
  expect_identical(
    attributes(tmp)$rdf_atts$KeySlots.rdf$create_date, 
    attributes(keyrwtbl)$create_date
  )
  expect_identical(
    attributes(tmp)$rdf_atts$KeySlots.rdf$description, 
    attributes(keyrwtbl)$description
  )
  expect_identical(
    attributes(tmp)$rdf_atts$KeySlots.rdf$n_traces, 
    attributes(keyrwtbl)$n_traces
  )
  # and check the sysRdf
  expect_identical(
    attributes(tmp)$rdf_atts$SystemConditions.rdf$mrm_config_name, 
    attributes(sysrwtbl)$mrm_config_name
  )
  expect_identical(
    attributes(tmp)$rdf_atts$SystemConditions.rdf$owner, 
    attributes(sysrwtbl)$owner
  )
  expect_identical(
    attributes(tmp)$rdf_atts$SystemConditions.rdf$create_date, 
    attributes(sysrwtbl)$create_date
  )
  expect_identical(
    attributes(tmp)$rdf_atts$SystemConditions.rdf$description, 
    attributes(sysrwtbl)$description
  )
  expect_identical(
    attributes(tmp)$rdf_atts$SystemConditions.rdf$n_traces, 
    attributes(sysrwtbl)$n_traces
  )
  # check that it contains all of the above variables
  expect_true(
    all(as.character(levels(as.factor(tmp$Variable))) %in% sam$variable)
  )
  expect_true(
    all(sam$variable %in% as.character(levels(as.factor(tmp$Variable))))
  )
  # check that it contains expected colnames
  expect_equal(
    colnames(tmp), 
    c("Year", "Month", "TraceNumber", "Scenario", "Variable", "Value")
  )
  # check that annual slots only containd December; 
  # check that monthly slots contains all months
  expect_true(all(
    (tmp %>% 
      filter(
        Variable %in% 
          c("peLt1000", "peEocy", "julyRel", "pwyRel", "short", "ueb823")
      ))$Month %in%
      "December"
  ))
  
  expect_true(all(
    month.name %in% (filter(tmp, Variable %in% c("pMonthlyGt400k")))$Month
  ))
})


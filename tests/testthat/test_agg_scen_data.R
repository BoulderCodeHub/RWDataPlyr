context("test rwtbl_aggregate()")
library(dplyr)

sam <- rwd_agg(read.csv(
  system.file("extdata/rwd_agg_files/passing_aggs.csv", package = "RWDataPlyr"), 
  stringsAsFactors = FALSE
))
  
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

# check that the "all" keyword gets all the data --------------------------
test_that("'all' keyword gets all data", {
  expect_is(
    tmp <- rwtbl_aggregate(
      rwd_agg(rdfs = "KeySlots.rdf"),
      scen_dir = dnfmost_dir,
      scenario = "DNFMost",
      keep_cols = FALSE
    ),
    c("tbl_df")
  )
  expect_true(all(as.character(levels(as.factor(tmp$variable))) %in% c("powell_outflow", "mead_pe")))
})



context("test rwtbl_aggregate()")
library(dplyr)

ra1 <- rwd_agg(read.csv(
  system.file("extdata/rwd_agg_files/passing_aggs.csv", package = "RWDataPlyr"), 
  stringsAsFactors = FALSE
))
  
dnfmost_dir <- system.file(
  "extdata/Scenario/ISM1988_2014,2007Dems,IG,Most", 
  package = "RWDataPlyr"
)

keyrwtbl <- rw_rdf_to_tbl(keyRdf)
sysrwtbl <- rw_rdf_to_tbl(sysRdf)

# test rwtbl_aggregate() structure -------------------------
test_that("test rwtbl_aggregate() structure", {
  expect_s3_class(
    tmp <- rwtbl_aggregate(
      ra1,
      scen_dir = dnfmost_dir,
      scenario = "DNFMost",
      keep_cols = FALSE
    ),
    c("tbl_df", "data.frame")
  )
  
  # check attribute has the ra1
  expect_identical(
    attributes(tmp)$slot_agg_matrix, ra1
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
    all(as.character(levels(as.factor(tmp$Variable))) %in% ra1$variable)
  )
  expect_true(
    all(ra1$variable %in% as.character(levels(as.factor(tmp$Variable))))
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
  expect_true(all(as.character(levels(as.factor(tmp$Variable))) %in% 
                    c("powell_outflow", "mead_pe")))
  
  # check function when the rwd_agg includes "all" and summary slots from the
  # same rdf
  expect_is(
    tmp2 <- rwtbl_aggregate(
      rbind(ra1, rwd_agg(rdfs = "KeySlots.rdf")),
      scen_dir = dnfmost_dir,
      scenario = "DNFMost",
      keep_cols = FALSE
    ),
    c("tbl_df")
  )
  expect_true(all(as.character(levels(as.factor(tmp$Variable))) %in%
                    c("powell_outflow", "mead_pe", ra1$variable)))
  
  # compare the two data, they should be the same
  expect_equal(
    tmp %>% arrange(Year, Month, TraceNumber, Scenario, Variable),
    tmp2 %>% 
      filter(Variable %in% c("powell_outflow", "mead_pe")) %>%
      arrange(Year, Month, TraceNumber, Scenario, Variable)
  )
})

# check handling NaNs -----------------------------------
ss <- c("Shortage.ShortageFlag", "Coordinated Operation.ReducedReleaseFlag")
flags_rdf <- rw_rdf_to_tbl(read.rdf(file.path(dnfmost_dir, "Flags.rdf"))) %>%
  filter(ObjectSlot %in% ss)
short_ra <- rwd_agg(data.frame(
  file = "Flags.rdf", 
  slot = ss, 
  period = "asis", 
  summary = NA, 
  eval = NA, 
  t_s = NA, 
  variable = c("short_flag", "rdc_flag"),
  stringsAsFactors = FALSE
))

test_that("`NaN`s are treated properly in `rwtbl_aggregate()`", {
  expect_error(rwtbl_aggregate(
    rwd_agg(rdfs = "Flags.rdf"), 
    scen_dir = dnfmost_dir, 
    nans_are = "error"
  ))
  
  expect_error(rwtbl_aggregate(
    short_ra, scen_dir = dnfmost_dir, nans_are = "error"
  ))
  
  expect_is(
    t1 <- rwtbl_aggregate(short_ra, scen_dir = dnfmost_dir, nans_are = "0"),
    "tbl_df"
  )
  # flags that have no NaNs should not be affected
  expect_equal(
    (t1 %>% filter(Variable == "rdc_flag"))$Value,
    (flags_rdf %>% filter(ObjectSlot == ss[2]))$Value
  )
  # converting to 0s should not affect the sum
  expect_equal(
    (t1 %>%
       filter(Variable == "short_flag") %>%
       group_by(Year, Month, TraceNumber) %>%
       summarise(Value = sum(Value)))$Value,
    (flags_rdf %>%
       filter(ObjectSlot == ss[1]) %>%
       group_by(Year, Month, TraceNumber) %>%
       summarise(Value = sum(Value, na.rm = TRUE)))$Value
  )
})

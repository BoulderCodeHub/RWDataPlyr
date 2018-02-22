context("test `rw_scen_aggregate()`")

# rw_scen_aggregate <- function(scenarios, 
#                               agg, 
#                               scen_dir = ".", 
#                               nans_are = "0",
#                               keep_cols = FALSE,
#                               file = NULL, 
#                               scen_names = NULL)

rwa <- rwd_agg(read.csv(
  system.file("extdata/rwd_agg_files/passing_aggs.csv", package = "RWDataPlyr"), 
  stringsAsFactors = FALSE
))

scens1 <- c("ISM1988_2014,2007Dems,IG,2002", "ISM1988_2014,2007Dems,IG,Most")
scens2 <- c(scens1[1], "nonExisting")
scens3 <- scens1
scenNames <- c("2002", "Most")
names(scens3) <- scenNames
scenPath <- system.file("extdata/Scenario", package = "RWDataPlyr")

test_that("`rw_scen_aggregate()` arguments verify correctly", {
  expect_error(
    rw_scen_aggregate(scens2, rwa, scenPath),
    paste0("The following scenario directories do not exist:\n",
           file.path(normalizePath(scenPath), "nonExisting")
    ),
    fixed = TRUE
  )
  expect_error(rw_scen_aggregate(scens1, rwa, "~/apath/that/surely/doesnt/exist"))
  expect_error(
    rw_scen_aggregate(scens1, rwa, c(".", "~")),
    "`scen_dir` should only have a length of 1.",
    fixed = TRUE
  )
  expect_error(
    rw_scen_aggregate(scens1, as.data.frame(rwa), scenPath),
    "In `rw_scen_aggregate()`, `agg` must be a `rwd_agg` object.",
    fixed = TRUE
  )
  expect_error(
    rw_scen_aggregate(scens3, rwa, scenPath, scen_names = scenNames),
    "In `rw_scen_aggregate()`, `scenarios` have `names()`, so `scen_names` should not be specified.",
    fixed = TRUE
  )
  expect_error(rw_scen_aggregate(scens3, rwa, scenPath, errors_are = "zeros"))
  expect_error(
    rw_scen_aggregate(scens3, rwa, scenPath, file = c("this.csv", "that.feather")),
    "In `rw_scen_aggregate()`, `file` should have a length of 1.",
    fixed = TRUE
  )
  expect_error(
    rw_scen_aggregate(scens3, rwa, scenPath, file = c("this.that")),
    "In `rw_scen_aggregate()`, `file` should have a .csv, .feather, or .txt extension.",
    fixed = TRUE
  )
  expect_error(
    rw_scen_aggregate(scens3, rwa, scenPath, file = c("an/invalid/loc/ofile.csv")),
    "In `rw_scen_aggregate()`, `file` should point to a valid location.",
    fixed = TRUE
  )
})
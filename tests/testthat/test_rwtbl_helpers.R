context("Test `rwtbl_slot_names()` function")

rwtbl <- rw_rdf_to_tbl(keyRdf)

test_that("`rwtbl_slot_names()` finds correct slots", {
  expect_type(rwslots <- rwtbl_slot_names(rwtbl), "character")

  expect_true(all(rwslots %in% c("Mead.Pool Elevation", "Powell.Outflow")))
  
  expect_true(all(c("Mead.Pool Elevation", "Powell.Outflow") %in% rwslots))
  
  expect_setequal(rwslots, getSlotsInRdf(keyRdf))
})

rwa <- as.rwd_agg(read.csv(
  system.file("extdata/rwd_agg_files/passing_aggs.csv", package = "RWDataPlyr"),
  stringsAsFactors = FALSE
))

test_that("`rwtbl_slot_names()` fails for other data frames", {
  errMsg <- paste0(
    "Invalid `tbl_df` passed to `rdftbl_slot_names()`.\n",
    "It does not have an `ObjectSlot` column"
  )
  expect_error(rwtbl_slot_names(mtcars), errMsg, fixed = TRUE)
  
  # should fail for summarized RW data
  expect_error(
    rwtbl_slot_names(rwtbl_aggregate(
      rwa[1,],
      system.file(
        "extdata/Scenario/ISM1988_2014,2007Dems,IG,Most/", 
        package = "RWDataPlyr"
      )
    )),
    errMsg,
    fixed = TRUE
  )
})

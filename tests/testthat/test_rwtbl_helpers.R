library(dplyr)
# rwtbl_slot_names() ---------------------
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

# rwtbl_var_to_slot() ---------------------
context("Test `rwtbl_var_to_slot()`")

test_that("`rwtbl_var_to_slot()` errors correctly", {
  expect_error(
    rwtbl_var_to_slot(mtcars, "peLt1000"), 
    paste0("Invalid `tbl_df` passed to `rwtbl_var_to_slot()`.\n",
           "It does not have a `Variable` column"),
    fixed = TRUE
  )
  expect_error(
    rwtbl_var_to_slot(mutate(mtcars, Variable = rownames(mtcars)), "peLt1000"),
    paste0("Invalid `tbl_df` passed to `rwtbl_var_to_slot()`.\n",
           "It does not have a `rwd_agg` attribute."),
    fixed = TRUE
  )
})
rwa <- rwd_agg(read.csv(
  system.file("extdata/rwd_agg_files/passing_aggs.csv", package = "RWDataPlyr"), 
  stringsAsFactors = FALSE)
)
test_that("`rwtbl_var_to_slot()` works", {
  expect_type(tmp <- rwtbl_var_to_slot(scen_data, rwa$variable), "character")
  expect_length(tmp, nrow(rwa))
  expect_setequal(tmp, rwa$slot)
  
  expect_type(tmp <- rwtbl_var_to_slot(scen_data, rwa$variable[1]), "character")
  expect_length(tmp, 1)
  expect_setequal(tmp, rwa$slot[1])
  
  expect_type(
    expect_warning(
      tmp <- rwtbl_var_to_slot(scen_data, c(rwa$variable[1:2], "okey")), 
      paste0("The following variables were not found in the rwtbl:\n",
             paste(paste("    ", "okey"), collapse = "\n"))
    ),
    "character"
  )
  expect_length(tmp, 2)
  expect_setequal(tmp, rwa$slot[1:2])
  
  expect_type(
    expect_warning(
      tmp <- rwtbl_var_to_slot(scen_data, c("noway", "okey")), 
      paste0("The following variables were not found in the rwtbl:\n",
             paste(paste("    ", c("noway", "okey")), collapse = "\n"))
    ),
    "character"
  )
  expect_length(tmp, 0)
})

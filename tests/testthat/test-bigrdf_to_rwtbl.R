test_that("bigrdf_to_rwtbl works", {
  expect_r6_class(x <- bigrdf_to_rwtbl('../rdfs/series.rdf'), 'FileSystemDataset')
  expect_equivalent(dplyr::collect(x), rdf_to_rwtbl2('../rdfs/series.rdf'))
  
  expect_r6_class(
    x <- bigrdf_to_rwtbl('../rdfs/series.rdf', scenario = "TST", add_ym = TRUE), 
    'FileSystemDataset'
  )
  cc_order <- c("Timestep", "Year", "Month", "Value", "ObjectSlot", "Scenario", 
                "TraceNumber")
  expect_equivalent(
    dplyr::collect(x), 
    # they return in different orders so change order so they match
    rdf_to_rwtbl2('../rdfs/series.rdf', scenario = "TST", add_ym = TRUE) |>
      dplyr::select(dplyr::all_of(cc_order))
  )
  expect_true(!is.null(attr(x, "base_dir")))
  
  # create new temporary folder and move x there
  new_path <- tempfile()
  dir.create(new_path)
  
  expect_r6_class(
    x2 <- bigrdf_save(x, new_path, remove_uid = TRUE), 
    'FileSystemDataset'
  )
  expect_true(dir.exists(file.path(new_path, 'series')))
  
})

library(RWDataPlyr)
context('check creation of slot aggregation list')

test_that("createSlotAggList returns proper errors", {
  expect_error(createSlotAggList(matrix(1:6, ncol = 3)), 
               "iData is a matrix with 3 columns. There should either be 4 or 5 columns.")
  expect_error(createSlotAggList(matrix(1:6, ncol = 6)), 
               "iData is a matrix with 6 columns. There should either be 4 or 5 columns.")
  expect_error(createSlotAggList(1:3), 
               "iData is not a matrix, nor can it be converted to an Nx4 or Nx5 matrix")
  expect_error(createSlotAggList(1:17),
               "iData is not a matrix, nor can it be converted to an Nx4 or Nx5 matrix")
  expect_warning(createSlotAggList(1:8),
                 "Attempting to convert iData to a N x 4 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:4),
                 "Attempting to convert iData to a N x 4 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:15),
                 "Attempting to convert iData to a N x 5 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_warning(createSlotAggList(1:5),
                 "Attempting to convert iData to a N x 5 matrix. Results may be unexpected. Probably better to stop and pass a matrix to createSlotAggList.")
  expect_error(createSlotAggList(file.path("some", "crazy", "file.txt")),
               paste(file.path("some", "crazy", "file.txt"),'does not exist.'))
})

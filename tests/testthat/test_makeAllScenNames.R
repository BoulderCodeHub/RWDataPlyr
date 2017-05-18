library(RWDataPlyr)
context('check makeAllScenNames')

test_that("length and dimensions are correct", {
  expect_equal(length(makeAllScenNames(c("a", "b"), c("a", "b"))), 4)
  expect_equal(length(makeAllScenNames(c("a"), "b")), 1)
  expect_equal(length(makeAllScenNames(1:4, 1:6, 1:4, 1:30, sep = "_")), 4*6*4*30)
  expect_null(dim(makeAllScenNames(1:3, 1:2, 1:6)))
  expect_equal(
    length(strsplit(makeAllScenNames(1:4, 1:6, 1:4, 1:30)[1], ",", fixed = TRUE)[[1]]),
    4
  )
  expect_equal(
    length(strsplit(makeAllScenNames(1:4, 1:6, 1:4, sep = "_")[1], ",", fixed = TRUE)[[1]]),
    1
  )
})

test_that("scenario names are correct", {
  expect_equal(makeAllScenNames(c("a", "b"), c("a", "b"))[1], "a,a")
  expect_true(all(c("1,6,3,15", "4,3,1,22") %in% makeAllScenNames(1:4, 1:6, 1:4, 1:30)))
  expect_true(all(c("b_a_2", "a_a_4") %in% makeAllScenNames(c("a", "b"), c("a", "b"), 1:4, sep = "_")))
})

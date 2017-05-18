library(RWDataPlyr)
library(zoo)

context("check getWYFromYearMon")

test_that("dimensions are correct", {
  expect_equal(length(getWYFromYearmon(as.yearmon("Dec 1999"))), 1)
  expect_equal(length(getWYFromYearmon(as.yearmon(2000 + seq(0, 23)/12))), 24)
  expect_null(dim(getWYFromYearmon(as.yearmon(2000 + seq(0, 11)/12))))
})

test_that("WY is computed as expected", {
  expect_equal(getWYFromYearmon(as.yearmon("Dec 1999")), 2000)
  expect_equal(getWYFromYearmon(as.yearmon(c("Dec 1999", "Jan 2000", "Dec 2000"))), 
                                c(2000, 2000, 2001))
  expect_equal(getWYFromYearmon(as.yearmon(2000 + seq(0, 23)/12)), 
               c(rep(2000, 9), rep(2001, 12), rep(2002, 3)))
})

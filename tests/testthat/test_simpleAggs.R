library(RWDataPlyr)
context('check simple aggregation methods')

simpMat <- matrix(1:48, ncol = 2)
pe <- rdf_get_slot(keyRdf, 'Mead.Pool Elevation')[1:36,1:4]

# rwslot_annual_min -------------------

test_that("`rwslot_annual_min()` returns corect values", {
  expect_equal(rwslot_annual_min(simpMat), matrix(c(1, 13, 25, 37),ncol = 2))
  expect_equal(
    rwslot_annual_min(pe), 
    rbind(
      apply(pe[1:12,], 2, min), 
      apply(pe[13:24,], 2, min), 
      apply(pe[25:36,], 2, min)
    )
  )
})

test_that("`rwslot_annual_min()` matches `getMinAnnValue()`", {
  expect_identical(
    rwslot_annual_min(simpMat),
    expect_warning(getMinAnnValue(simpMat))
  )
  expect_identical(
    rwslot_annual_min(pe),
    expect_warning(getMinAnnValue(pe))
  )
})

# rwslot_annual_max ---------------

test_that("`rwslot_annual_max()` returns corect values", {
  expect_equal(rwslot_annual_max(simpMat), matrix(c(12, 24, 36, 48),ncol = 2))
  expect_equal(
    rwslot_annual_max(pe), 
    rbind(
      apply(pe[1:12,], 2, max), 
      apply(pe[13:24,], 2, max), 
      apply(pe[25:36,], 2, max)
    )
  )
})

test_that("`rwslot_annual_max()` matches `getMaxAnnValue()`", {
  expect_identical(
    rwslot_annual_max(simpMat), expect_warning(getMaxAnnValue(simpMat))
  )
  expect_identical(rwslot_annual_max(pe), expect_warning(getMaxAnnValue(pe)))
})

# rwslot_annual_sum ------------------
test_that("rwslot_annual_sum returns corect values", {
  expect_equal(
    rwslot_annual_sum(simpMat), 
    matrix(c(sum(1:12), sum(13:24), sum(25:36), sum(37:48)), ncol = 2)
  )
  expect_equal(
    rwslot_annual_sum(pe), 
    rbind(
      apply(pe[1:12,], 2, sum), 
      apply(pe[13:24,], 2, sum),
      apply(pe[25:36,], 2, sum)
    )
  )
})

test_that("rwslot_annual_sum matches sumMonth2Annual", {
  expect_identical(
    rwslot_annual_sum(simpMat),
    expect_warning(sumMonth2Annual(simpMat))
  )
  expect_identical(
    rwslot_annual_sum(pe),
    expect_warning(sumMonth2Annual(pe))
  )
})


# rwslot_fwaac -------------------
flow <- rep(c(700,800,800,900,750,900),2)*1000
saltMass <- c(25,30,35,25,25,22,35,47,21,45,34,23)*1000

test_that("`trace_fwaac()` returns correct Values", {
  expect_equal(round(RWDataPlyr:::trace_fwaac(saltMass, flow),5),27.82642)
  expect_error(
    RWDataPlyr:::trace_fwaac(saltMass[1:11],flow), 
    'Data passed to `trace_fwaac()` is not divisible by 12',
    fixed = TRUE
  )
  expect_error(
    RWDataPlyr:::trace_fwaac(saltMass,flow[1:11]), 
    "Data passed to `trace_fwaac()` is not divisible by 12",
    fixed = TRUE
  )
})

flow <- cbind(flow, flow)
flow <- rbind(flow, flow, flow, flow)
saltMass <- cbind(saltMass, saltMass)
saltMass <- rbind(saltMass, saltMass, saltMass, saltMass)

test_that("`rwslot_fwaac() returns correct values", {
  expect_is(tmp <- rwslot_fwaac(saltMass, flow), "matrix")
  expect_equal(dim(tmp), c(4, 2))
  expect_true(all(round(tmp, 5) == 27.82642))
})

test_that("`rwslot_fwaac() errors correctly", {
  expect_error(
    rwslot_fwaac(saltMass, flow[1:36, ]),
    "In `rwslot_fwaac()`, the dimensions of `flow` and `mass` must match.",
    fixed = TRUE
  )
  expect_error(
    rwslot_fwaac(saltMass[,1], flow),
    "In `rwslot_fwaac()`, the dimensions of `flow` and `mass` must match.",
    fixed = TRUE
  )
  expect_error(
    rwslot_fwaac(saltMass[1:40,], flow[1:40,]),
    "In `rwslot_fwaac()`, `mass` and `flow` are not divisible by 12.", 
    fixed = TRUE
  )
})

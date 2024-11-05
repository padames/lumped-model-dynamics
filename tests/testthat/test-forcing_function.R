source("../../R/forcing_function.R")

test_that("magnitude ratio is calculated as expected", {
  Re <- 4
  Im <- 3
  expect_equal(magnitud_ratio_abs_value_fn(Re, Im), 5)
})

test_that("magnitude ratio of negative part as expected", {
  Re <- 4
  Im <- -3
  expect_equal(magnitud_ratio_abs_value_fn(Re, Im), 5)
})


test_that("phase angle computed correclty", {
  Re <- 4
  Im <- 3
  expect_equal(phase_angle_radians_fn(Re, Im), 0.643501109)
})


test_that("phase angle computed from negative part correclty", {
  Re <- 4
  Im <- -3
  expect_equal(phase_angle_radians_fn(Re, Im), -0.643501109)
})

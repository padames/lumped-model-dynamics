source("../../R/poles_characteristic_equation.R")

test_that("pole with possitive sign is computed correctly for positive discriminant", {
  mass <- 1
  damping <- 5
  stiffness <- 1 
  num <- - damping + sqrt( damping* damping - 4 * mass * stiffness)
  den <- 2 * mass
  Re <- num / den
  expected <- as.complex(Re)
  expect_equal( pole_plus_fn(mass, damping, stiffness), expected)
})

test_that("pole with negative sign is computed correctly for positive discriminant", {
  mass <- 1
  damping <- 5
  stiffness <- 1 
  num <- - damping - sqrt( damping* damping - 4 * mass * stiffness)
  den <- 2 * mass
  Re <- num / den
  expected <- as.complex(Re)
  expect_equal( pole_minus_fn(mass, damping, stiffness), expected)
})




test_that("pole with negative sign is computed correctly for negative discriminant", {
  mass <- 2
  damping <- 1.5
  stiffness <- 5
  Re <- -damping / (2 * mass)
  Im <- -sqrt( 4 * stiffness * mass - damping * damping) / (2 * mass)
  expected <- complex(real=Re, imaginary=Im)
  expect_equal( pole_minus_fn(mass, damping, stiffness), expected )
})



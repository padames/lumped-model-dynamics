source("../../R/pole_types.R")

test_that("defining an adhoc anumeration works as expected", {
  fruit_poles <- create_enum_fn(APPLE,BANANA,MELON)
  expect_equal(fruit_poles$APPLE, fruit_poles$APPLE)
  expect_true(fruit_poles$BANANA != fruit_poles$MELON)
})


testthat::test_that("The pole types are defined correctly", {
  testthat::expect_true(pole_types$TWO_REAL_REPEATED != pole_types$TWO_COMPLEX_CONJUGATE)
  testthat::expect_true(pole_types$TWO_REAL_DISTINCT != pole_types$TWO_REAL_REPEATED)
  testthat::expect_true(pole_types$TWO_COMPLEX_CONJUGATE != pole_types$TWO_REAL_DISTINCT)
})
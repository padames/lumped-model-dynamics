source("../../R/pole_types.R")

test_that("defining an adhoc anumeration works as expected", {
  fruit_poles <- create_enum_fn(APPLE,BANANA,MELON)
  expect_equal(fruit_poles$APPLE, fruit_poles$APPLE)
  expect_true(fruit_poles$BANANA != fruit_poles$MELON)
})


testthat::test_that("Expected error when enum type expected to be a character string", {
  testthat::expect_true(pole_types$TWO_REAL_REPEATED != "TWO_REAL_REPEATED")
})

testthat::test_that("Expected error when enum type dereferenced with vector subset operator [", {
  testthat::expect_error(pole_types["TWO_COMPLEX_CONJUGATE"], "object of type 'environment' is not subsettable", fixed = TRUE)
})

testthat::test_that("Dereferencing with list subset operator [[ works if passed a string", {
  testthat::expect_equal(pole_types[["TWO_COMPLEX_CONJUGATE"]], pole_types$TWO_COMPLEX_CONJUGATE)
})

testthat::test_that("Expect error when dereferencing using list subset operator [[ with an unquoted string", {
  testthat::expect_error(pole_types[[TWO_COMPLEX_CONJUGATE]], "object 'TWO_COMPLEX_CONJUGATE' not found", fixed = TRUE)
})

testthat::test_that("The pole types are defined correctly", {
  testthat::expect_true(pole_types$TWO_REAL_REPEATED != pole_types$TWO_COMPLEX_CONJUGATE)
  testthat::expect_true(pole_types$TWO_REAL_DISTINCT != pole_types$TWO_REAL_REPEATED)
  testthat::expect_true(pole_types$TWO_COMPLEX_CONJUGATE != pole_types$TWO_REAL_DISTINCT)
})
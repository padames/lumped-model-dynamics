source("../../R/compute_Giw.R")

test_that("decomposing poles works as expected", {
  R1 <- -0.6682
  I1 <- 0.1341
  R2 <- 1.5
  I2 <- -0.73
  p1 <- complex(real = R1, imaginary = I1)
  p2 <- complex(real = R2, imaginary = I2)
  parts <- decompose_poles(p1, p2)
  R1_calc <- parts["R1"]
  expect_true(parts["R1"] == R1)
  expect_true(parts["I1"] == I1)
  expect_true(parts["R2"] == R2)
  expect_true(parts["I2"] == I2)
})


test_that("left parts are computed correctly", {
  p1 <- complex(real = 0.5, imaginary = -1.2)
  p2 <- complex(real = -0.49, imaginary = 0.24)
  w <- 0.7194
  parts <- left_parts(p1, p2, w)
  RLL <- -Re(p1) * ( Re(p1) - Re(p2) )
  ILR <- (Re(p1) - Re(p2)) * (w - Im(p1)) - Re(p1) * (Im(p1) - Im(p2)) + (w - Im(p1)) * (Im(p1) - Im(p2))
  expect_true( parts["RLL"] == RLL)
  expect_true( parts["ILR"] == ILR)
})
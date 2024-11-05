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
  RLL <- -Re(p1) * ( Re(p1) - Re(p2) )
  ILR <- (Re(p1) - Re(p2)) * (w - Im(p1)) - Re(p1) * (Im(p1) - Im(p2)) + (w - Im(p1)) * (Im(p1) - Im(p2))
  
  parts <- left_parts(p1, p2, w)

    expect_true( parts["RLL"] == RLL)
  expect_true( parts["ILR"] == ILR)
})

test_that("right parts are computed correctly", {
  p1 <- complex(real = 0.5, imaginary = -1.2)
  p2 <- complex(real = -0.49, imaginary = 0.24)
  w <- 0.7194
  
  RRL <- - Re(p2) * (Re(p2) - Re(p1))
  
  IRR <- (Re(p2) - Re(p1)) * (w + Im(p2)) - Re(p2) * (Im(p2) - Im(p1)) + (w + Im(p2)) * (Im(p2) - Im(p1))
  
  parts <- right_parts(p1, p2, w)

  expect_true( parts["RRL"] == RRL)
  expect_true( parts["IRR"] == IRR)
})


test_that("Giw is computed correctly", {
  p1 <- complex(real = 0.5, imaginary = -1.2)
  p2 <- complex(real = -0.49, imaginary = 0.24)
  w <- 0.7194
  
  L <- left_parts(p1, p2, w)
  R <- right_parts(p1, p2, w)
  
  RLL <- as.double(L["RLL"])
  ILR <- as.double(L["ILR"])
  RRL <- as.double(R["RRL"])
  IRR <- as.double(R["IRR"])
  
  RLL2 <- RLL * RLL
  ILR2 <- ILR * ILR
  
  RRL2 <- RRL * RRL
  IRR2 <- IRR * IRR
  
  den_L <- RLL2 + ILR2 
  den_R <- RRL2 + IRR2
  
  Re_L <- RLL / den_L
  Re_R <- RRL / den_R
  
  Im_L <- ILR / den_L
  Im_R <- IRR / den_R
  
  Re <- Re_L + Re_R
  Im <- Im_L + Im_R
  
  g_iw <- Giw(p1, p2, w)
  
  expect_equal(Re(g_iw), Re)
  expect_equal(Im(g_iw), Im)
})
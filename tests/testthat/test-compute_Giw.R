source("../../R/compute_Giw.R")



test_that("Giw for the system transfer function is computed as expected",
          {
            m <- 1
            c <- 0.5
            k <- 0.25
            w <- 0.85
            den <- (k - m*w*w) * (k - m*w*w) + c*w*c*w
            real_part <-  (k - m*w*w) / den
            imaginary_part <- (c*w) / den
            expected_Giw = complex(real = real_part, imaginary = imaginary_part) 
            computed_Giw <- compute_Giw_fn(m, c, k, w)
            expect_equal(computed_Giw, expected_Giw)
          })


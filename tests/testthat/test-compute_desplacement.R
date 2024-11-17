library(here)

# source("../../R/compute_desplacement.R")
# source("../../R/compute_Giw.R")
source(here::here("R","compute_displacement.R"))
source(here::here("R","compute_Giw.R"))

test_that("creates a function", 
          {
            m <- 1
            c <- 0.5
            k <- 0.25
            w <- 0.15
            x_compl <- create_particular_solution_function_fn(m, c, k ,w)
            
            expect_equal(class(x_compl), "function")
          })


test_that("creates a function that can be called", 
          {
            m <- 1
            c <- 0.5
            k <- 0.25
            w <- 0.15
            
            x_compl <- create_particular_solution_function_fn(m, c, k ,w)
            
            F0 <- 5
            t <- seq(1, 10, 0.5)
            x_v <- x_compl(F0,t)
            expect_equal(length(x_v), 19)
          })

test_that("computes the correct solution", 
          {
            m <- 1
            c <- 0.5
            k <- 0.25
            w <- 0.15
            F0 <- 5
            
            Giw_complex <- compute_Giw_fn(m, c, k, w)
            
            F0 <- 5
            t <- 1

            x_expected <- F0 * Mod(Giw_complex) * sin(w*t + Arg(Giw_complex))
            
            x_calc_fn <- create_particular_solution_function_fn(m, c, k ,w)
            
            x_calc <- x_calc_fn(F0, t)
            
            expect_equal(length(x_calc), 1)
            expect_equal(x_calc, x_expected)
          })


test_that("computes the correct solution for large t", 
          {
            m <- 1
            c <- 0.5
            k <- 0.25
            w <- 0.15
            F0 <- 5
            
            Giw_complex <- compute_Giw_fn(m, c, k, w)
            
            t <- 1:100
            F0_v <- rep(F0, times = length(t))
            MR_v <- rep( Mod(Giw_complex), times = length(t))
            w_v = rep( w, length(t))
            phase_v <- rep (Arg(Giw_complex), times = length(t))
            
            x_expected <- F0_v * MR_v  * sin(w_v*t + phase_v)
            
            x_calc_fn <- create_particular_solution_function_fn(m, c, k , w)
            
            x_calc <- x_calc_fn(F0, t)
            
            expect_equal(length(x_calc), 100)
            expect_equal(x_calc, x_expected)
          })

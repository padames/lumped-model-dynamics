library(here)

source(here::here("R","compute_complementary_solution.R"))
source(here::here("R","pole_types.R"))

test_that("creates the function to generate the complementary solution", {
  
  calc_function <- create_complementary_function_roots_real_distintc_fn(1, 100, 1, 1, 1)
  
  expect_equal(class(calc_function), "function")
})


test_that("the function to generate the complementary solution works as expected", {
  
  m <- 1
  c <- 100
  k <- 1
  F0 <- 1
  w <- 0.5
  
  poles <- compute_two_real_poles_fn(m, c, k)
  p1 <- poles$pole1
  p2 <- poles$pole2
  
  q_num <- F0 * w
  q1_den <- (p1*p1 + w*w) * (p1 - p2)
  q2_den <- (p2*p2 + w*w) * (p2 - p1)
  
  q1 <- q_num / q1_den
  q2 <- q_num / q2_den
  
  t_v <- 1:100
  num_time_points_to_simulate <- length(t_v)
  q1_v <- rep(q1, times = num_time_points_to_simulate)
  q2_v <- rep(q2, times = num_time_points_to_simulate)
  p1_v <- rep(p1, times = num_time_points_to_simulate)
  p2_v <- rep(p2, times = num_time_points_to_simulate)
  
  x_expected_value <- q1_v * exp(p1_v*t_v) + q2_v * exp(p2_v * t_v) 
    
  
  x_calc_function <- create_complementary_function_roots_real_distintc_fn(m, c, k, F0, w)
  

  expect_equal(x_calc_function(t_v), x_expected_value)
})



testthat::test_that("complex conjugate constant for under damped case computes correctly", {
  
  constant_complex_conjugate <- complex(real = -3.5, imaginary = 6.9)
  input_force <- 2
  input_frequency <- 3
  
  
  Re <- -(-3.5*input_force*input_frequency)/(4*(3.5)*(3.5)*6.9*6.9 + (3.5*3.5-6.9+input_frequency*input_frequency)*(3.5*3.5-6.9+input_frequency*input_frequency))
  
  Im <- -(input_force*input_frequency*(3.5*3.5-6.9+input_frequency*input_frequency))/(8*3.5*3.5*6.9*6.9*6.9+2*6.9*(3.5*3.5-6.9+input_frequency*input_frequency)*(3.5*3.5-6.9+input_frequency*input_frequency))
  

  expected_constant <- complex(real = Re, imaginary = Im)
  
  calculated_constant <- compute_complex_conjugate_constant_for_under_damped_complementary_solution_fn(constant_complex_conjugate, input_force, input_frequency)
  
  testthat::expect_equal(calculated_constant, expected_constant)
})



testthat::test_that("create_complementary_function_roots_complex_conjugate_fn ceates a vector of expected length", {
  mass <- 25
  damping <- 1
  stiffness <- 0.1
  input_force <-2
  input_frequency <-3
  
  solution_fn <- create_complementary_function_roots_complex_conjugate_fn(mass, 
                                                                          damping, 
                                                                          stiffness,
                                                                          input_force,
                                                                          input_frequency)
  a_solution <- solution_fn(seq(0,10))
  
  testthat::expect_equal(length(a_solution), length(seq(0,10)))
})

testthat::test_that("solution for critically damped system ceates a vector of expected length", {
  
  solution_fn <- create_complementary_function_roots_real_repeated_fn(mass = 1, damping = 4, stiffness = 4, input_force = 1, frequency = 0.25)

  a_solution <- solution_fn(seq(0,10))
  
  testthat::expect_equal(length(a_solution), length(seq(0,10)))
  
})

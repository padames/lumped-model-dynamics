source("../../R/compute_complementary_solution.R")

source("../../R/pole_types.R")

test_that("creates the function to generate the complementary solution", {
  
  calc_function <- create_x_complemetary_function_roots_real_distintc_fn(1, 100, 1, 1, 1)
  
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
    
  
  x_calc_function <- create_x_complemetary_function_roots_real_distintc_fn(m, c, k, F0, w)
  

  expect_equal(x_calc_function(t_v), x_expected_value)
})


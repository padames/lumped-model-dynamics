## for all your pole needs

source("R/pole_types.R")

pole_type_fn <- function(mass, damping, stiffness)
{
  discriminant <- damping * damping - ( 4 * mass * stiffness )
  
  if (discriminant > 0)
  {
    pole_types$TWO_REAL_REPEATED 
  } 
  else if ( discriminant < 0)
  {
    pole_types$TWO_COMPLEX_CONJUGATE
  }
  else
  {
    pole_types$TWO_REAL_DISTINCT
  }
}


create_particular_solution_function_fn <- function(m, c, k, F0, w) 
{
  poles_type = pole_type_fn(m, c, k)
  
  if (poles_type == pole_types$TWO_REAL_DISTINCT)
  {
    create_x_particular_function_roots_real_distintc_fn(m, c, k, F0, w)
  }
}


compute_two_real_poles_fn <- function(m, c, k)
{
  discriminant <- c * c - 4 * k * m

  if ( discriminant < 0 || discriminant == 0)  
  {
    cat(file = stdout(), discriminant)
    stop("Check if poles are real and distintc before calling this function")
  }
  p1 <- ( -c + sqrt(discriminant) ) / (2 * m)
  p2 <- ( -c - sqrt(discriminant) ) / (2 * m)
  
  list(pole1=p1, pole2=p2)
  
}


create_x_particular_function_roots_real_distintc_fn <- function(m, c, k, F0, w)
{
  #' Creates a function that can be called with a time vector
  #' 
  #' This will create the solution to the homogeneous linear ODE
  #' representing the behaviour of the system without external forces 
  #' 
  #' 
  
  poles <- compute_two_real_poles_fn(m, c, k)
  p1 <- poles$pole1
  p2 <- poles$pole2
  
  q_num <- F0 * w
  q1_den <- (p1*p1 + w*w) * (p1 - p2)
  q2_den <- (p2*p2 + w*w) * (p2 - p1)
  
  q1 <- q_num / q1_den
  q2 <- q_num / q2_den
  function(t_v) {
    num_time_points_to_simulate <- length(t_v)
    q1_v <- rep(q1, times = num_time_points_to_simulate)
    q2_v <- rep(q2, times = num_time_points_to_simulate)
    p1_v <- rep(p1, times = num_time_points_to_simulate)
    p2_v <- rep(p2, times = num_time_points_to_simulate)
    q1_v * exp(p1_v*t_v) + q2_v * exp(p2_v * t_v)  
  }
}


## this function returns a complex number
pole_ <- function( mass, damping_coeff, stiffness_coeff, possitive = TRUE) 
{
  sign <- ifelse(possitive, 1, -1)
  den <- 2 * mass
  discriminant <- damping_coeff * damping_coeff - 4 * stiffness_coeff * mass
  if ( discriminant > 0)
  {
    num <- -damping_coeff + sign * sqrt( discriminant)
    Re <- num / den
    Im <- 0
  }
  else
  {
    Re <- -damping_coeff / den
    Im <- sign * sqrt(-discriminant) / den
  }
  complex(real = Re, imaginary = Im)
}


## this is the pole computed with the positive square root of the discriminant
pole_plus_fn <- function(mass, damping_coeff, stiffness_coeff)
{
  pole_( mass, damping_coeff, stiffness_coeff, TRUE)
}


## this is the pole computed with the negative square root of the discriminant 
pole_minus_fn <- function( mass, damping_coeff, stiffness_coeff) 
{
  pole_( mass, damping_coeff, stiffness_coeff, FALSE)
}

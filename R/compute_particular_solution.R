## for all your pole needs
library("here")

source(here("R", "pole_types.R"))

pole_type_fn <- function(mass, damping, stiffness)
{
  discriminant <- damping * damping - ( 4 * mass * stiffness )
  
  if (discriminant > 0)
  {
    pole_types$TWO_REAL_DISTINCT 
  } 
  else if ( discriminant < 0)
  {
    pole_types$TWO_COMPLEX_CONJUGATE
  }
  else
  {
    pole_types$TWO_REAL_REPEATED
  }
}


create_particular_solution_function_fn <- function(mass, damping, stiffness, input_force, frequency) 
{
  poles_type = pole_type_fn(mass, damping, stiffness)
  
  if (poles_type == pole_types$TWO_REAL_DISTINCT)
  {
    create_particular_function_roots_real_distintc_fn(mass, damping, stiffness, input_force, frequency)
  }
  else if (poles_type == pole_types$TWO_COMPLEX_CONJUGATE)
  {
    stop("Under-damped solution not implemeted yet")
    create_particular_function_roots_complex_conjugate_fn(mass, damping, stiffness, input_force, frequency)
  }
  else
  {
    stop("Critically damped solution not implemneted yet")
    create_particular_function_roots_real_repeated_fn(mass, damping, stiffness, input_force, frequency)
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


create_particular_function_roots_real_distintc_fn <- function(mass, damping, stiffness, input_force, frequency)
{
  #' Creates a function that can be called with a time vector
  #' 
  #' This will create the solution to the homogeneous linear ODE
  #' representing the behaviour of the system without external forces 
  #' 
  #' 
  
  poles <- compute_two_real_poles_fn(mass, damping, stiffness)
  p1 <- poles$pole1
  p2 <- poles$pole2
  
  q_num <- input_force * frequency
  
  q1_den <- (p1*p1 + frequency*frequency) * (p1 - p2)
  q2_den <- (p2*p2 + frequency*frequency) * (p2 - p1)
  
  q1 <- q_num / q1_den
  q2 <- q_num / q2_den
  function(seconds_to_simulate) {
    
    num_time_points_to_simulate <- length(seconds_to_simulate)
    q1_v <- rep(q1, times = num_time_points_to_simulate)
    q2_v <- rep(q2, times = num_time_points_to_simulate)
    p1_v <- rep(p1, times = num_time_points_to_simulate)
    p2_v <- rep(p2, times = num_time_points_to_simulate)
    
    q1_v * exp(p1_v*seconds_to_simulate) + q2_v * exp(p2_v * seconds_to_simulate)  
  }
}


create_particular_function_roots_complex_conjugate_fn <- function(mass, damping, stiffness, input_force, frequency)
{
  if (FALSE)
  {
    # this won't work because the real and imaginary parts have to
    # be separated and the wave solution be formed then.
    two_complex_conjugate_poles <- compute_two_complex_conjugate_poles_fn(mass, damping, stiffness)
    
    p1 <- two_complex_conjugate_poles$p1
    p2 <- two_complex_conjugate_poles$p2
    
    q_num <- input_force * frequency
    
    q1_den <- (p1*p1 + frequency*frequency) * (p1 - p2)
    q2_den <- (p2*p2 + frequency*frequency) * (p2 - p1)
    
    q1 <- q_num / q1_den
    q2 <- q_num / q2_den
    
    real_part <- Re(p1)
    imaginary_part <- Im(p1)
    
    exp_term <- exp(real_part)
    
    q1_plus_q2 <- q1 + q2
    q1_minus_q2 <- q1 - q2
    
    function(seconds_to_simulate)
    {
      num_time_points_to_simulate <- length(seconds_to_simulate)
      # compute all vectors
      q1_plus_q2_s <- rep(q1_plus_q2, times = num_time_points_to_simulate)
      q1_minus_q2_s <- rep(q1_minus_q2, times = num_time_points_to_simulate)
      exp_term_s <- rep(exp_term, times = num_time_points_to_simulate)
      imaginary_part_s <- rep(imaginary_part, times = num_time_points_to_simulate)
      
      # now compose the particular response to the under-damped case
      exp_term_s * ( q1_plus_q2_s * cos(imaginary_part_s * seconds_to_simulate ) + 
                     q1_minus_q2_s * sin( imaginary_part_s * seconds_to_simulate))
    }  
  }
  # TODO: replace this stub for the real function
  
  function(vector_of_times)
  {
    num_time_points_to_simulate <- length(vector_of_times)
    rep(0, times = num_time_points_to_simulate)
  }
  
}


create_particular_function_roots_real_repeated_fn <- function(mass, damping, stiffness, input_force, frequency)
{
  # TODO: replace this stub for the real function1
  
  function(vector_of_times)
  {
    num_time_points_to_simulate <- length(vector_of_times)
    rep(0, times = num_time_points_to_simulate)
  }
}

compute_two_complex_conjugate_poles_fn <- function(mass, damping, stiffness)
{
  #' Computes the complex conjugate roots when the discriminant is negative
  #'
  #'
  #' This function only works when the discriminant of the characteristic equation
  #' is negative. This triggers a solution of two complex conjugate values.
  
  discriminant <- damping * damping - 4 * mass * stiffness
  
  if ( discriminant >= 0 )  
  {
    msg <- paste0("Expecting complex conjuagte poles, discriminant = ", discriminant)
    stop(msg)
  }
  
  two_times_mass = 2 * mass
  
  Re = -damping / two_times_mass
  
  Im = sqrt(-discriminant) / two_times_mass
  
  # these are complex conjugates, so they are only different in the sign of the imaginary part
  p1 <- complex( real = Re, imaginary = Im)
  p2 <- complex( real = Re, imaginary = -Im)
  
  list(pole1=p1, pole2=p2)
}

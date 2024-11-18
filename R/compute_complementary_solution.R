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


create_complementary_solution_function_fn <- function(mass, damping, stiffness, input_force, frequency) 
{
  poles_type = pole_type_fn(mass, damping, stiffness)
  
  if (poles_type == pole_types$TWO_REAL_DISTINCT)
  {
    create_complementary_function_roots_real_distintc_fn(mass, damping, stiffness, input_force, frequency)
  }
  else if (poles_type == pole_types$TWO_COMPLEX_CONJUGATE)
  {
    create_complementary_function_roots_complex_conjugate_fn(mass, damping, stiffness, input_force, frequency)
  }
  else
  {
    create_complementary_function_roots_real_repeated_fn(mass, damping, stiffness, input_force, frequency)
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


create_complementary_function_roots_real_distintc_fn <- function(mass, damping, stiffness, input_force, frequency)
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


compute_complex_conjugate_constant_for_under_damped_complementary_solution_fn <- function(a_complex_pole, input_force, input_frequency)
{
  #' calculates the complex constant necessary for the under-damped solution of the linear ODE of second order
  #' 
  #' Compute the necessary real and imaginary parts of the complex conjugate constants 
  #' appearing in the analytic solution of the under-damped linear ODE with an
  #' external oscillatory force. This solution models the transient response of the system.
  #' The final steady state is given by the particular solution, in this system 
  #' that solution is governed by the external force. 
  #' 
  #' It expects one of the complex poles of the transfer function as input and it
  #' returns the complex number representing the constant for the transient solution.      
  #' 
  #' The genesis of this complex number is the fact that the two constants appearing in the
  #' conversion of the Laplace domain to the time domain are complex numbers. The have to be
  #' complex conjugates for the complementary solution to be a Real number:
  #' 
  #'  x_complementary = q_1 * exp(p_1*t) + q_2 * exp(p_2*t)
  #'  
  #'  where p_1 = (R+i*I)*t and p_2 = (R - i*I)*t
  #'  
  #'  This equation can be transformed into
  #'  
  #'  exp(R*t) * ( (q_1+q_2) cos(I*t) + i*(q_1-q_2)*sin(I*t) )
  #'  
  #'  This solution can only be Real if (q_1+q_2) and i*(q_1-q_2) are also Real and this in turn
  #'  can only be true if and only if q_1 and q_2 are complex conjugates.

  R <- Re(a_complex_pole)
  I <- Im(a_complex_pole)
  
  
  R_square <- R * R
  I_square <- I * I
  
  frequency_squared <- input_frequency * input_frequency
  
  force_times_frequency <- input_force * input_frequency
  R_square_time_I_square <- R_square * I_square
  
  R_2_minus_I_plus_freq_2 <- R_square - I + frequency_squared 
  R_2_minus_I_plus_freq_2_all_squared <- R_2_minus_I_plus_freq_2 * R_2_minus_I_plus_freq_2
  
  # Real part
  num_Real <- - R * force_times_frequency
  den_Real <- 4 * R_square_time_I_square + R_2_minus_I_plus_freq_2_all_squared
  
  # Imaginary part
  num_Img <- - force_times_frequency * R_2_minus_I_plus_freq_2
  den_Img <- 8 * I * R_square_time_I_square + 2 * I * R_2_minus_I_plus_freq_2_all_squared
  
  Re_constant <- num_Real / den_Real
  Im_constant <- num_Img / den_Img
  
  constant_q_under_damped_case <- complex(real = Re_constant, imaginary = Im_constant)
  
  constant_q_under_damped_case
}


create_complementary_function_roots_complex_conjugate_fn <- function(mass, damping, stiffness, input_force, input_frequency)
{
  two_complex_conjugate_poles <- compute_two_complex_conjugate_poles_fn(mass, 
                                                                        damping, 
                                                                        stiffness)
  
  pole_1 <- two_complex_conjugate_poles$pole1

  constant_q_1 <- compute_complex_conjugate_constant_for_under_damped_complementary_solution_fn(pole_1, 
                                                                                            input_force, 
                                                                                            input_frequency )    
                                                                                          
  function(seconds_to_simulate)
  {
    num_time_points_to_simulate <- length(seconds_to_simulate)
    # compute all vectors
    
    Re_q1_s <- rep(2.0*Re(constant_q_1), times = num_time_points_to_simulate)
    Im_q1_s <- rep(2.0*Im(constant_q_1), times = num_time_points_to_simulate)
    R_s <- rep(Re(pole_1), times = num_time_points_to_simulate)
    I_s <- rep(Im(pole_1), times = num_time_points_to_simulate)
    
    exp_term_s <- exp(R_s * seconds_to_simulate)
    
    # now compose the particular response to the under-damped case
    exp_term_s * ( Re_q1_s * cos( I_s * seconds_to_simulate) - Im_q1_s * sin( I_s * seconds_to_simulate))
  }  
}


create_complementary_function_roots_real_repeated_fn <- function(mass, damping, stiffness, input_force, frequency)
{
  damping_over_two_times_mass <- damping / (2 * mass)
  damping_over_two_times_mass_squared <- damping_over_two_times_mass * damping_over_two_times_mass
  frequency_squared <- frequency * frequency

  first_den  <- damping_over_two_times_mass_squared + frequency_squared
  first_term <- frequency / first_den
  
  second_term <- damping * frequency / (mass * first_den * first_den)

  function(seconds_to_simulate)
  {
    num_time_points_to_simulate <- length(seconds_to_simulate)
    exp_term_s <- exp(-damping_over_two_times_mass * seconds_to_simulate)
    coefficients_first_term <- rep( first_term, times = num_time_points_to_simulate)
    
    coefficients_second_term <- rep(second_term, times = num_time_points_to_simulate)
    
    solution <- exp_term_s * ( coefficients_first_term * seconds_to_simulate + coefficients_second_term)
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
    msg <- paste0("Expecting complex conjugate poles, discriminant = ", discriminant)
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

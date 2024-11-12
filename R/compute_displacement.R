## This is the time domain response of the linear ODE for the oscillator
##
## It assumes that there is a time vector used as the input 
## independent variable and the physical parameters as constants
library(docstring)
library(here)
library(plotly)
library(dplyr)


source(here::here("R","compute_Giw.R"))
source(here::here("R","compute_particular_solution.R"))


create_complementary_solution_function_fn <- function(m, c, k, w) {
  #' Creates a closure to be called with F0 and t
  #' 
  #' The closure can compute the Giw lazily and give its Module and Argument
  #' it constructs a vector with the complementary wave solution.
  #' The maximum output of this solution is magnitude ratio or modulus of Giw
  #' and the wave function is out of phase with the input oscillation by an 
  #' angle equal to the argument of Giw
  #' 
  
  # this is part of the closure shipped with the function below
  Giw_complex <- compute_Giw_fn(m, c, k, w)
  
  # this is what you are calling when calling the closure returned 
  function(F0, t_v) 
  {
    
    num_time_points_to_simulate <- length(t_v)
    
    magnitud_ratio <- Mod(Giw_complex)
  
    phase_angle <- Arg(Giw_complex)
    
    F0_v <- rep(F0, times = num_time_points_to_simulate)
    
    magnitud_ratio_v <- rep(magnitud_ratio, times = num_time_points_to_simulate)
    
    w_v <- rep(w, times=num_time_points_to_simulate)
    
    phase_angle_v <- rep(phase_angle, times = num_time_points_to_simulate)
    
    F0_v * magnitud_ratio_v * sin( w_v * t_v + phase_angle_v)
  }  
}



## compute the displacement vector for a give time vector
x_t_fn <- function(m, c, k, w, F0, t_v)
{
  #' Compute the displacement vector for a spring immersed in fluid
  #'
  #' Plots the analytical solution in the time domain of the displacement
  #' of the center of mass of a system described by a spring immersed
  #' in a fluid and subject to an external oscillatory force. 
  #' The external force has a maximum value, F(0), in Newton, and an oscillation
  #' frequency, w, in radians per second.
  #' 
  #' 
  #' @param t a vector of points in time to solve for displacement
  #' @param m the mass of the system in kg concentrated at its center of gravity
  #' @param c the damping coefficient constant in kg/s, includes the fluid damping
  #' @param k the spring stiffness in kg/s^2
  #' @param w the frequency of the oscillatory external force, in radians per second
  #' @param F0 the maximum external force in Newton
  #' @param t_v vector of time points to solve for
  
  # number of solutions that will be computed
  simulation_points <- length(t_v)
  
  ##########
  x_complementary_fn <- create_complementary_solution_function_fn(m, c, k, w)
  
  x_v_complementary <- x_complementary_fn(F0, t_v)
  
  ##########
  x_particular_fn <- create_x_particular_function_roots_real_distintc_fn(m, c, k, F0, w)
  
  x_v_particular <- x_particular_fn(t_v)
  
  # the final vectorized solution
  x_v_particular + x_v_complementary
}


x_t_plot <- function(m, c, k, w, F0, t_v)
{
  x_t <- x_t_fn(m, c, k, w, F0, t_v)
  
  title <- paste0("Dynamic Response of damped spring system, mass=",m," kg, damping coeff=", c, " kg/s, \nstiffnesss coef=", k, " kg/s2, F0=",F0," N, frequency=", w," radians/s")
  # plot(t_v, x_t*10, type = "l",
  #      main = title,
  #      xlab = "Time, seconds",
  #      ylab = "Displacement, cm")
  data_to_plot <- data.frame(time=t_v, displacement=x_t/10)
  fig <- plot_ly(x= data_to_plot$time, y=data_to_plot$displacement, type = "scatter",
                mode = 'lines')
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Time, seconds"),
                        yaxis = list(title = "Displacement, cm"))
  fig
}


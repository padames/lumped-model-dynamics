## This is the time domain response of the linear ODE for the oscillator
##
## It assumes that there is a time vector used as the input 
## independent variable and the physical parameters as constants
library(docstring)

source("R/compute_Giw.R")
source("R/forcing_function.R")
source("R/poles_characteristic_equation.R")

## compute the displacement vector for a give time vector
x_fn <- function(t, m, c, k, w, F0, t_v)
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
  
  pole_plus <- pole_plus_fn(m, c, k)
  pole_minus <- pole_minus_fn(m, c, k)
  
  Giw <- Giw_fn(pole_plus, pole_minus, w)
  phase <- Arg(Giw)
  
  # number of solutions that will be computed
  simulation_points <- length(t_v)
  
  # compute the vectors for the solution
  w_v <- rep(w, times = simulation_points)
  phase_v <- rep(phase, times = simulation_points)
  magnitude_ratio <- Mod(Giw)
  magnitude_ratio_v <- rep( magnitude_ratio, times = simulation_points) 
  F0_v <- rep(F0, times = simulation_points)
    
  # vectorized equation
  x_v_particular <-  magnitude_ratio_v * sin( w_v * t_v + phase_v)
  x_v_complementary <- w_v 
    
  x_v = F0 * ( x_v_particular + x_v_complementary)  
  
}

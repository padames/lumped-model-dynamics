## This is the time domain response of the linear ODE for the oscillator
##
## It assumes that there is a time vector used as the input 
## independent variable and the physical parameters as constants
library(docstring)

source("R/compute_Giw.R")
source("R/forcing_function.R")
source("R/poles_characteristic_equation.R")
source("R/pole_types.R")


compute_Giw_fn <- function(m, c, k) {
  
  list_of_poles <- poles_fn(m, c, k)
  
  if (list_of_poles$case_type == pole_types$TWO_REAL_DISTINCT )
  {
    # compute the complementary solution and Giw for the particular solution (wave) 
    Giw <- Giw_TwoRealRoots_fn(list_of_poles$p1, list_of_poles$p2, w)
    }
  else if (list_of_poles$case_type == pole_types$TWO_REAL_REPEATED )
  {
    # compute the complementary solution for this case and Giw (for the wave form)
    Giw <- Giw_OneRealRepeatedPole_fn(list_of_poles$p, w)
  }  
  else 
  {
    # # list_of_poles$case_type == pole_types$TWO_COMPLEX_CONJUGATE
    # compute the complementary solution (wave form) and the Giw (for the wave form)
    # list_of_poles$p1 #complex
    # list_of_poles$p2 #complex
    Giw <- Giw_TwoComplexConjPoles_fn(list_of_poles$p1, list_of_poles$p2, w)
  }
}


Giw_TwoRealRoots_fn <- function(p1, p2, w)
{
  1
}


Giw_OneRealRepeatedPole_fn <- function(p, w)
{
  2
}


Giw_TwoComplexConjPoles_fn <- function(pc1, pc2, w)
{
  3
}

  
create_complementary_fn <- function(Giw, poles_case=pole_types$TWO_REAL_DISTINCT) {
  if (poles_case == pole_types$TWO_REAL_DISTINCT) {
    function(t) {
      "TWO_REAL"
    }  
  }
  else if (poles_case == pole_types$TWO_REAL_DISTINCT)
  {
    function(t) {
      "TWO_REAL_REPEATED"
    }
  }
  else if (poles_case == pole_types$TWO_COMPLEX_CONJUGATE)
  {
    function(t) {
      "TWO_COMPLEX_CONJUGATE"
    }
    # stop("Two complex conjugate poles not implemented yet")
  }
}


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
  
  Giw <- compute_Giw_fn()
  
  phase <- Arg(Giw)
  magnitude_ratio <- Mod(Giw)
  
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
  
  x_v = F0 * ( x_v_particular + x_v_complementary)  
  
}
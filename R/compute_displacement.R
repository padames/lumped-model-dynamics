## This is the time domain response of the linear ODE for the oscillator
##
## It assumes that there is a time vector used as the input 
## independent variable and the physical parameters as constants
library(docstring)
library(here)
library(plotly)
library(dplyr)


source(here::here("R","compute_Giw.R"))
source(here::here("R","compute_complementary_solution.R"))


create_particular_solution_function_fn <- function(mass, damping_coefficient, stiffness_coefficient, input_frequency) {
  #' Creates a closure to be called with F0 and t
  #' 
  #' This function returns another function to compute the particular solution
  #' to the linear ordinary differential solution (ODE) representing
  #' a spring immersed in a liquid and with an external force applied to it.
  #' 
  #' The returned function computes the Giw lazily to provide its Modulus 
  #' and Argument. The function returns a vector with solution.
  #' The maximum output of this solution is the magnitude ratio or modulus of Giw
  #' and the resulting wave is out of phase with the input oscillation by an 
  #' angle equal to the argument of Giw
  #' 
  
  # this is part of the closure shipped with the function below
  Giw_complex <- compute_Giw_fn(mass, damping_coefficient, stiffness_coefficient, input_frequency)
  
  # this is the particular solution summoned when calling the closure 
  function(input_force, vector_of_times) 
  {
    num_time_points_to_simulate <- length(vector_of_times)
    
    magnitud_ratio <- Mod(Giw_complex)
  
    phase_angle <- Arg(Giw_complex)
    
    vector_of_input_forces <- rep(input_force, times = num_time_points_to_simulate)
    
    vector_of_magnitud_ratios <- rep(magnitud_ratio, times = num_time_points_to_simulate)
    
    vector_of_frequencies <- rep(input_frequency, times=num_time_points_to_simulate)
    
    vector_of_phase_angles <- rep(phase_angle, times = num_time_points_to_simulate)
    
    paricular_solution <- vector_of_input_forces * vector_of_magnitud_ratios * sin( vector_of_frequencies * vector_of_times + vector_of_phase_angles)
    
    paricular_solution
  }  
}


## compute the displacement vector for a give time vector
displacement_fn <- function(arguments)
{
  #' Compute the displacement vector for a spring immersed in fluid
  #'
  #' Plots the analytical solution in the time domain of the displacement
  #' of the center of mass of a system described by a spring immersed
  #' in a fluid and subject to an external oscillatory force. 
  #' The external force has a maximum value, F(0), in Newton, and an oscillation
  #' frequency, w, in radians per second.
  #' 
  #' @param arguments list of parameters needed for the simulation. Below their description.
  #'        mass the mass of the system in kg concentrated at its center of gravity
  #'        damping_coefficient the damping constant in kg/s, includes the fluid damping
  #'        stiffness_coefficient the spring stiffness in kg/s^2
  #'        force_frequency the frequency of the oscillatory external force, in radians per second
  #'        input_force the maximum external force in Newton
  #'        seconds_to_simulate time points to solve for
  
                 mass  <-  arguments$mass
              damping  <-  arguments$damping
            stiffness  <-  arguments$stiffness
            frequency  <-  arguments$frequency
          input_force  <-  arguments$input_force
  seconds_to_simulate  <-  arguments$seconds_to_simulate  
  
  
  ##########
   
  displacement_complementary_fn <- create_complementary_solution_function_fn(mass, damping, stiffness, input_force, frequency)
  displacement_complementary_solution <- displacement_complementary_fn(seconds_to_simulate)
  
  ##########
  
  displacement_particular_fn <- create_particular_solution_function_fn(mass, damping, stiffness, frequency)
  displacement_particular_solution <- displacement_particular_fn(input_force, seconds_to_simulate)
  
  # the complete vectorized solution
  
  displacement <- displacement_particular_solution + displacement_complementary_solution
  
  displacement
}


displacement_plot_fn <- function(arguments)
{
  #' Create a plot for easy visualization of the dynamic response
  #' 
  #' 
  #' Creates a plot with the displacement and times.
  #' It relies on the function displacement_fn to compute the dynamic response
  #' to the damped spring system subject to an external force, starting at rest
  #' and in its rest position (with zero displacement). 
  #' 
  #' @param arguments a list of values including the mass, damping, stiffness,
  #' frequency, input force, and the times to be used for the simulation of the
  #' displacement of the spring.
  
  
  vector_of_displacements <- displacement_fn(arguments)
  
  title <- paste0("Dynamic Response of damped spring system, mass=", arguments$mass,
                  " kg, damping coeff=", arguments$damping, 
                  "kg/s, \nstiffnesss coef=", arguments$stiffness,
                  " kg/s2, input force=", arguments$input_force,
                  " N, frequency=", arguments$frequency," radians/s")
  
  data_to_plot <- data.frame(time=arguments$seconds_to_simulate, 
                             displacement=vector_of_displacements/10)

  fig <- plot_ly(x    = data_to_plot$time, 
                 y    = data_to_plot$displacement, 
                 type = "scatter",
                 mode = 'lines')
  
  fig <- fig %>% layout(title = title,
                        xaxis = list(title = "Time, seconds"),
                        yaxis = list(title = "Displacement, cm"))
  
  # this will make it show in an interactive RStudio session
  fig
}


displacement_plot_simple_fn <- function(arguments)
{
  #' Create a plot for easy visualization of the dynamic response
  #' 
  #' 
  #' Creates a plot with the displacement and times.
  #' It relies on the function displacement_fn to compute the dynamic response
  #' to the damped spring system subject to an external force, starting at rest
  #' and in its rest position (with zero displacement). 
  #' 
  #' @param arguments a list of values including the mass, damping, stiffness,
  #' frequency, input force, and the times to be used for the simulation of the
  #' displacement of the spring.
  
  
  vector_of_displacements <- displacement_fn(arguments)
  
  title <- paste0("Dynamic Response of damped spring system, mass=", arguments$mass,
                  " kg, damping coeff=", arguments$damping, 
                  "kg/s, \nstiffnesss coef=", arguments$stiffness,
                  " kg/s2, input force=", arguments$input_force,
                  " N, frequency=", arguments$frequency," radians/s")
  

  plot(arguments$seconds_to_simulate, vector_of_displacements/10, type = "l",
       main = title,
       xlab = "Time, seconds",
       ylab = "Displacement, cm")
}



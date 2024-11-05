#!/usr/bin/env Rscript

# Plot the time response of the spring given fixed values

## Fixed values:

# mass of the spring and load
# units: kg
m <- 1

# damping coefficient including fluid damping
# units: kg/s
c <- 1

# Stiffness of the structure
# units = kg/s^2
k <- 1

### Initial conditions:

# Initial displacement of the center of mass
x_0 <- 0

# Initial velocity
dx_dt_0 <- 0


### Input force

# Max input force
# Units: N 
F_0 <- 1 

# frequency of the oscillator imposing the external force on the system
# units: radian/second
w <- 1

#### VARIABLES

# Linear displacement of the mass with respect to starting point, no displacement
# units: m
# x(t)  

# Linear velocity of the mass
# units: m/s
# v(t)

# Linear acceleration of the mass
# units: m/s^2
# a(t)

### GOAL

# Given a linear ordinary differential equation describing the force balance 
# of the lumped-model of the system, we want to describe the dynamic response
# in the time and frequency domains to understand how a real system would 
# behave if it were described by this mathematical formulation.

# Provide a plot of x(t) versus time, t, for a given set of parameters

source("functions.R")

mr <- magnitud_ratio_fn(14, 2)

phase_angle <- phase_angle_fn(1,2)

print(paste("Mangnitude Ratio = ", mr))

print(paste("Phase angle = ", phase_angle))





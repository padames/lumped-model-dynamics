## this functions compute the poles of the characteristic equation derived
## from applying the Laplace transform to the second-order ODE describing
## the lumped-model of a spring oscillator immersed in a fluid



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

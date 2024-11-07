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

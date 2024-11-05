

# functions to be sourced into main program

# Re, the real part
# Im, the imaginary part
# magnitude ratio is the ratio between the largest output and the largest input
magnitud_ratio_fn <- function(Re, Im) 
{
  sqrt(Re * Re + Im * Im)
}

phase_angle_fn <- function(Re, Im) 
{
  atan(Im / Re)
}



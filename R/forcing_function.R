
## use
## to calculate the output of a forcing function on a lumped system described by
## a harmonic oscillator wit mass m, damping factor c, and stiffness k 

## The output will be the magnitude ratio multiplied by the sin of the 
## frequency times time plus the phase angle
## 
## Output = MR * sin( wt + alpha)
##
## where MR is the magnitude ratio
##       w is the frequency of the input force
##       t is time
##       alpha is the phase angle in radians


# Re, the real part
# Im, the imaginary part
# magnitude ratio is the ratio between the largest output and the largest input
magnitud_ratio_abs_value_fn <- function(Re, Im) 
{
  cn <- complex(real = Re, imaginary = Im)
  Mod(cn )
  # sqrt(Re * Re + Im * Im)
}

# The returned value is in radians
phase_angle_radians_fn <- function(Re, Im) 
{
  cn <- complex(real = Re, imaginary = Im)
  Arg(cn)
  # atan(Im / Re)
}



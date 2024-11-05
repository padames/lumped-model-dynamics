## this functions compute the real and imaginary parts of the 
## transfer function in the frequency domain, G(iw)

## The input are the poles and the frequency of the external force 

decompose_poles <- function(p1, p2)
{
  R1 <- Re(p1)
  R2 <- Re(p2)
  I1 <- Im(p1)
  I2 <- Im(p2)

  c(R1=R1, R2=R2, I1=I1, I2=I2)  
}


left_parts <- function(p1, p2, w)
{
  parts <- decompose_poles(p1, p2)
  
  R1 <- parts["R1"]
  R2 <- parts["R2"]
  I1 <- parts["I1"]
  I2 <- parts["I2"]
  
  RLL <- -R1 * ( R1 - R2 )
  
  w_minus_I1 <- w - I1
  I1_minus_I2 <- I1 - I2
  R1_minus_R2 <- R1 - R2
  
  ILR <- R1_minus_R2 * w_minus_I1 - R1 * I1_minus_I2 + w_minus_I1 * I1_minus_I2
  
  c(RLL=RRl, ILR=ILR)
}


right_parts <- function(p1, p2, w)
{
  parts <- decompose_poles(p1, p2)
  
  R1 <- parts["R1"]
  R2 <- parts["R2"]
  I1 <- parts["I1"]
  I2 <- parts["I2"]
  
  w_plus_I2 <- w + I2
  I2_minus_I1 <- I2 - I1
  R2_minus_R1 <- R2 - R1
  
  RRL <- - R2 * R2_minus_R1
  
  IRR <- R2_minus_R1 * w_plus_I2 - R2 * I2_minus_I1 + w_plus_I2 * I2_minus_I1
  
  c(RRL=RRL, IRR=IRR)
}


Giw <- function(p1, p2, w) 
{
  L <- left_parts(p1, p2, w)
  R <- right_parts(p1, p2, w)
  
  RLL <- L["RLL"]
  ILR <- L["ILR"]
  RRL <- R["RRL"]
  IRR <- R["IRR"]
  
  RLL2 <- RLL * RLL
  ILR2 <- ILR * ILR
  
  RRL2 <- RRL * RRL
  IRR2 <- IRR * IRR
  
  den_L <- RLL2 + ILR2 
  den_R <- RRL2 + IRR2
  
  Re_L <- RLL / den_L
  Re_R <- RRL / den_R
  
  Im_L <- ILR / den_L
  Im_R <- IRR / den_R
  
  Re <- ReL + Re_R
  Im <- Im_L + Im_R
  
  complex(Re, Im)
}

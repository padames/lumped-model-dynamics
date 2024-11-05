## this functions compute the real and imaginary parts of the 
## transfer function in the frequency domain, G(iw)

## The input are the poles and the frequency of the external force 


Re_Giw <- function(p1, p2, w) 
{
  Re1 <- real_left(p1, p2, w)
  Re2 <- real_right(p1, p2, w)
  Re1 + Re2
}

Im_Giw <- function(p1, p2, w)
{
  Im1 <- imaginary_left(p1, p2, w)
  Im2 <- imaginary_right(p1, p2, w)
  Im1 + Im2
}

real_left <- function(p1, p2, w)
{
  R1 <- Re(p1)
  R2 <- Re(p2)
  I1 <- Im(p1)
  I2 <- Im(p2)
  
  RLL <- -R1 * ( R1 - R2 )
  RLL2 <- RLL * RLL
  
  w_minus_I1 <- w - I1
  I1_minus_I2 <- I1 - I2
  R1_minus_R2 <- R1 - R2
  
  ILR <- R1_minus_R2 * w_minus_I1 - R1 * I1_minus_I2 + w_minus_I1 * I1_minus_I2
  ILR2 <- ILR * ILR
  
  RRL <- - R1_minus_R2 * R2
  RRL2 <- RRL * RRL
  
  
  
  num 
  den 
}


real_righ <- function(p1, p2, w)
{
  
}


imaginary_right <- function(p1, p2, w)
{
  
  
}

imaginary_right <- function(p1, p2, w)
{
  
  
}
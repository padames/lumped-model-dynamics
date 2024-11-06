## this functions compute the real and imaginary parts of the 
## transfer function in the frequency domain, G(iw)

## The input are the poles and the frequency of the external force 

library(docstring)

decompose_poles_fn <- function(p1, p2)
{
  #' Get the Real and Imaginary parts of two complex numbers
  #'
  #' Each of the two input complex numbers is decomposed into 
  #' real and imaginary components and returned in a named vector
  #' 
  #' @param p1 first complex number
  #' @param p2 second complex number
  #' 

  R1 <- as.double(Re(p1))
  R2 <- as.double(Re(p2))
  I1 <- as.double(Im(p1))
  I2 <- as.double(Im(p2))

  c(R1=R1, R2=R2, I1=I1, I2=I2)  
}


left_parts_fn <- function(p1, p2, w)
{
  #' Computes the components of the left fractions
  #' 
  #' This is a near optimal way of computing the values 
  #' that are used to compute the left fraction of each
  #' of the Real and Imaginary components of a given solution
  #' for two complex values. 
  #' Values are computed only once.
  #'
  #' @param p1 first complex pole
  #' @param p2 second complex pole
  #' @param w the frequency of oscillation of the external force 
  
  parts <- decompose_poles_fn(p1, p2)
  
  R1 <- as.double(parts["R1"])
  R2 <- as.double(parts["R2"])
  I1 <- as.double(parts["I1"])
  I2 <- as.double(parts["I2"])
  
  RLL <- -R1 * ( R1 - R2 )
  
  w_minus_I1 <- w - I1
  I1_minus_I2 <- I1 - I2
  R1_minus_R2 <- R1 - R2
  
  ILR <- R1_minus_R2 * w_minus_I1 - R1 * I1_minus_I2 + w_minus_I1 * I1_minus_I2
  
  c(RLL=RLL, ILR=ILR)
}


right_parts_fn <- function(p1, p2, w)
{
  #' Computes the components of the right fractions
  #' 
  #' This is a near optimal way of computing the values 
  #' that are used to compute the right fraction of each
  #' of the Real and Imaginary components of a given solution
  #' for two complex values.
  #' Values are only computed once.
  #'
  #' @param p1 first complex pole
  #' @param p2 second complex pole
  #' @param w the frequency of oscillation of the external force 
  #' 
  
  parts <- decompose_poles_fn(p1, p2)
  
  R1 <- as.double(parts["R1"])
  R2 <- as.double(parts["R2"])
  I1 <- as.double(parts["I1"])
  I2 <- as.double(parts["I2"])
  
  w_plus_I2 <- w + I2
  I2_minus_I1 <- I2 - I1
  R2_minus_R1 <- R2 - R1
  
  RRL <- - R2 * R2_minus_R1
  
  IRR <- R2_minus_R1 * w_plus_I2 - R2 * I2_minus_I1 + w_plus_I2 * I2_minus_I1
  
  c(RRL=RRL, IRR=IRR)
}


Giw_fn <- function(p1, p2, w) 
{
  #' Computes de complex number representing the output in the frequency domain
  #' 
  #' The output of the system in the frequency domain cam be a complex number.
  #' Its magnitude ratio is the maximum output of the system in a transfer function
  #' sense. The argument of this number represents the phase angle leading or 
  #' lagging the input force angle (wt)
  #' 
  #' @param p1 first complex pole
  #' @param p2 second complex pole
  #' @param w the frequency of oscillation of the external force 
  #'

  L <- left_parts_fn(p1, p2, w)
  R <- right_parts_fn(p1, p2, w)
  
  RLL <- as.double(L["RLL"])
  ILR <- as.double(L["ILR"])
  RRL <- as.double(R["RRL"])
  IRR <- as.double(R["IRR"])
  
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
  
  Re <- Re_L + Re_R
  Im <- Im_L + Im_R
  
  complex(real = Re, imaginary = Im)
}

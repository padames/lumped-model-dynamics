## this functions compute the real and imaginary parts of the 
## transfer function in the frequency domain, G(iw)

## The input are the poles and the frequency of the external force 

library(docstring)

compute_Giw_fn <- function(m, c, k, w)
{
  #' Evaluates the transfer function for the system at iw
  #' 
  #' G(s) is the result of applying the Laplace transform
  #' to the original linear ODE representing the system, and then
  #' solving for G(s)=X(s)/M(s).
  #' 
  #' Where X(s) is the output of the system in the Laplace domain
  #' X(s) is the input of the system in the Laplace domain and 
  #' M(s) is the external force or disturbance in the Laplace domain
  #' 
  #' The solution is to replace s by iw into G(s)
  #' 
  #'               1
  #' G(s)= ------------------- 
  #'       ( ms^2 + c*s + k)
  #'
  #' and then separate the real and imaginary parts to create a complex number       
  
  k_minus_m_w2 <- k - m * w * w
  cw <- c * w
  c2_times_w2 <- cw * cw
  
  denominator <- k_minus_m_w2 * k_minus_m_w2 + c2_times_w2
  
  real_part <- k_minus_m_w2 / denominator
  
  imaginary_part <- cw / denominator
  
  # create the value and return it  
  complex(real = real_part, imaginary = imaginary_part)
}

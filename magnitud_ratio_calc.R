#!/usr/bin/env Rscript

# computes the magnitude ratio given the real and imaginary parts of the 
# transfer function evaluated inthe frequency domain where s= iw


# see http://wresch.github.io/2013/06/20/commandline-args-in-R.html
# https://monashbioinformaticsplatform.github.io/2015-09-28-rbioinformatics-intro-r/05-cmdline.html
args <- commandArgs(trailing = TRUE)

if (length(args) < 2) {
  cat("Expecting two arguments: Real and Imaginary components of G(iw)\n")
  quit()
}

Re <- as.double(args[1])
Im <- as.double(args[2])

mr <- sqrt(Re * Re + Im * Im)

cat(paste0(mr,"\n"))
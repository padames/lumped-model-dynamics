## define the type of the solutions for the poles of the transfer function
library("docstring")

## Source: https://stackoverflow.com/a/44152358/1585486


create_enum_fn <- function(...) {
  #' Creates an immutable enumeration from provided values
  #' 
  #' Pass comma-separated, un-quoted names, usually upper-case. 
  #' These names will be referenced using the dollar operator
  #' from the environment name. Their actual value should be irrelevant.
  #' They should be used to match cases.
  #' 
  #' 
  #' @param UNQUATEDNAMES a variable comma-separated sequence of names
  #' 

  ## EDIT: use solution provided in comments to capture the arguments
  values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)
  
  # the provided values have to be unique
  stopifnot(identical(unique(values), values))
  
  res <- setNames(seq_along(values), values)

  # lock them up in an environment
  res <- as.environment(as.list(res))
  lockEnvironment(res, bindings = TRUE)
  res
}

# create the pole types here
pole_types <- create_enum_fn(TWO_REAL_DISTINCT, TWO_REAL_REPEATED, TWO_COMPLEX_CONJUGATE)
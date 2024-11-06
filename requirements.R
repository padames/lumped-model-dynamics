

## functional and vectorized single function from 
## https://stackoverflow.com/a/44660688/1585486

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# install.packages("usethis")
# install.packages("testthat")


using("devtools", "usethis", "testthat")

install_github("dasonk/docstring")
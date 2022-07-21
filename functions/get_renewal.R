source("functions/disc_gamma.R")

### Take in full length of y and w, return sum(y[(n-1):1]*w(1:(n-1)))

get_renewal <- function(y, shape, scale){
  size <- length(y)
  renew <- sum(y[])
  return(renew)
}

get_renewal(2, 2, 2)

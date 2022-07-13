

### Take in full length of y and w, return sum(y[(n-1):1]*w(1:(n-1)))

get_renewal <- function(y, w){
  size <- length(y)
  renew <- sum(y[(size-1):1]*w[1:(size-1)])
  return(renew)
}

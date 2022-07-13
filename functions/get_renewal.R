


get_renewal <- function(y){
  size <- length(y)
  w <- disc_gamma(1:size, 2, 2)
  renew <- sum(y[(size-1):1]*w[1:(size-1)])
  return(renew)
}

get_renewal(covid$y[1:10])
library("pomp")
source("functions/disc_gamma.R")


meas_pois <- function(t, x, T, Y, y, ..., log){
  if(t > 1){
    lambda = abs(x)*sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2.2, scale = 2.2))
    lambda <- ceiling(lambda)
    dpois(y, lambda = lambda)
  }else{
    dpois(y, lambda = 1)
  }
}


meas_pois_lnorm <- function(t, x, T, Y, y, ..., log){
  if(t > 1){
    lambda = x*sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2.7, scale = 2.7))
    lambda <- ceiling(lambda)
    # print(lambda)
    dpois(y, lambda = lambda)
  }else{
    dpois(y, lambda = 1)
  }
}
library("pomp")
source("functions/disc_gamma.R")


meas_pois <- function(t, x, T, Y, y, ..., log){
  lambda = abs(x)*sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2.2, scale = 2.2))
  if(t > 1){
    dpois(y, lambda = lambda)
  }else{
    dpois(y, lambda = 1)
  }
}


meas_pois_lnorm <- function(t, x, T, Y, y, ..., log){
  if(t > 1){
    lambda = x*sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2.2, scale = 2.2))
    # print("### start")
    # print(x)
    # print(lambda)
    # print(Y[t])
    # print("### end")
    dpois(y, lambda = lambda)
  }else{
    dpois(y, lambda = 1)
  }
}
library("pomp")
source("functions/disc_gamma.R")


meas_pois <- function(t, x, T, Y, y, ..., log){
  if(t > 1){
    lambda = abs(x)*sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2.2, scale = 2.2))
    dpois(round(y), lambda = lambda)
  }else{
    dpois(round(y), lambda = 1)
  }
}


meas_pois_lnorm <- function(t, x, T, Y, y, ..., log){
  if(t > 1){
    lambda = x * sum(Y[(t-1):1] * disc_gamma(1:(t-1), shape = 2, scale = 2))
    dpois(round(y), lambda = lambda)
  }else{
    dpois(round(y), lambda = 1)
  }
}


meas_norm <- function(t,x,T,Y,y,...,log){
  dnorm(y, x, 0.2)
}

meas_pois <- function(t,x,T,Y,y,...,log){
  dpois(round(y), lambda=x)
}
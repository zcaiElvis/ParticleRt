library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

source("functions/plot_case_Rt.R")


t <- c(1:20)
y <- rnorm(20, mean = abs(rnorm(20, 1, 0.3)))


pfilter(
  Np = 1,
  times = t,
  t0 = 1,
  data = data.frame(series = y),
  rinit = function(x0, ...){
    c(x = x0)
  },
  params = c(sigma1 = 1, x0=1),
  
  
  ### rprocess ###
  rprocess = discrete_time(
    function(x, sigma1, ...){
      xnext <- rnorm(1, x, sd = 0.3)
      c(x = xnext)
    },
    delta.t = 1
  ),
  
  ### dmeasure ###
  dmeasure = function (t, x, T, Y, ..., log) {
    i <- min(T>=t)
    print(t)
    lik <- 1
  },
  T = t, Y = y,
  verbose=TRUE
)



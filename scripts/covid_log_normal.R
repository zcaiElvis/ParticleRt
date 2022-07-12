library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

### Sourcing functions ###
source("scripts/read_data.R")
source("constants/file_loc.R")

### Read data ###
covid <- read_data(covid_file_loc, inter=c(50:800))

### Notes ###
# 1. Need to recalculate R_t in the beginning to avoid sudden change in the beginning


### Particle filter configuration ###
Np = 1000
init_vals = c(sigma1 = 0.2, x0 = 1)


### Particle filter function setups ###

## Transition Simulation ##
rprocess_config <- discrete_time(
  function(x, sigma1, ...){
    xnext <- rnorm(1, x, sd = sigma1)
    c(x = xnext)
  },
  delta.t = 1.5
)

## Measurement Evaluation ##

dmeasure_config <- function(t, x, T, Y, y, ..., log){
  lambda = abs(x)*sum(Y[(t-1):1] * dgamma(1:(t-1), shape = 2.2, scale = 2.2))
  dpois(y, lambda = lambda)
}


### Run Particle Filter ###
pf_covid <- pfilter(
  Np= Np,
  times = "idx",
  t0 = 1,
  data = covid,
  rinit = function(x0, ...){
    c(x = x0)
  },
  params = init_vals,
  
  rprocess = tran_lognormal,
  dmeasure = meas_pois_lnorm,
  
  T = t, 
  Y = covid$y,
  statenames = "x",
  pred.mean = TRUE,
  pred.var = TRUE,
  filter.mean = TRUE,
  filter.traj = TRUE,
  verbose=FALSE
)

### Post process ###
covid_result <- as.data.frame(pf_covid)
plot(covid_result$pred.mean.x)







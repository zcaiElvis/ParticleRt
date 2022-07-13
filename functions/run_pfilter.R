library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")

source("functions/d_measure.R")
source("functions/r_process.R")


run_pfilter <- function(rprocess_config, dmeasure_config, 
                        data, Np= 300,
                        x_init = 1,
                        sigma1 = 1, sigma2 = 2,
                        sdlog = 0.02,
                        shape = 2, scale = 2,
                        verbose = FALSE){
  Np = Np
  init_vals = c(sigma1 = sigma1, sigma2 = sigma2, sdlog = sdlog, x0=x_init, shape = shape, scale = scale)
  
  pf <- pfilter(
    Np= Np,
    times = "idx",
    t0 = 1,
    data = data,
    rinit = function(x0, ...){
      c(x = x0)
    },
    params = init_vals,
    
    rprocess = rprocess_config,
    dmeasure = dmeasure_config,
    
    T = t, 
    Y = data$y,
    statenames = "x",
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE,
    verbose=verbose
  )
  return(pf)
}



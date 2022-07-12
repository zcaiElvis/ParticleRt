library("tidyverse")
library("dplyr")
library("zoo")
library("pomp")


run_pfilter <- function(rprocess_config, dmeasure_config, data, Np= 300, verbose = FALSE){
  Np = Np
  init_vals = c(sigma1 = 0.2, x0=1.5)
  
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



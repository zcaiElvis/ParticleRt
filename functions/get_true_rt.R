source("functions/disc_gamma.R")

get_true_rt <- function(y, g_shape = 2, g_scale = 2){
  n <- length(y)
  w <- disc_gamma(1:n, shape = g_shape, scale = g_scale)
  rt <- rep(0, n)
  rt[1] = 0
  
  for(j in 2:n){
    rt[j] <- y[j]/(sum(y[(j-1):1]*w[1:(j-1)]))
  }
  return(rt)
}

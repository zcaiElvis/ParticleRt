
get_est_Rt <- function(cases, shape = 2, scale = 2){
  R_t <- rep(0, length(cases)-1)
  for(i in 2:length(cases)){
    R_t[i] <- cases[i]/sum(cases[i:1]*dgamma(1:i, shape = shape, scale = scale))
  }
  return(R_t)
}

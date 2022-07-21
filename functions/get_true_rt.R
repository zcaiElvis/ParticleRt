source("functions/disc_gamma.R")

# get_true_rt <- function(y, g_shape = 2, g_scale = 2){
#   size <- length(y)
#   rt <- rep(0, size)
#   rt[1] = 1
#   
#   for(j in 2:size){
#     rt[j] <- y[j]/(sum(y[(j-1):1]*disc_gamma(1:(j-1), g_shape, g_scale)))
#   }
#   return(rt)
# }

get_true_rt <- function(y, shape, scale){
  est_lday <- rep(0, length(y))
  for(k in 2:length(y)){
    est_lday[k] <- sum(y[(k-1):1]*disc_gamma(1:(k-1), shape = shape, scale = scale))
  }
  est_lday[1] <- est_lday[2]
  return(y/est_lday)
}

get_lambda <- function(y, shape, scale){
  lambs <- rep(0, length(y))
  for(k in 2:length(y)){
    lambs[k] <- sum(y[(k-1):1]*disc_gamma(1:(k-1), shape = shape, scale = scale))
  }
  lambs[1] <- lambs[2]
  return(lambs)
}



# rt <- get_true_rt(covid$y)
# plot(rt[50:length(rt)])


### Cant use I/sum Iw. If sumIw really small, then I/sumIw really big
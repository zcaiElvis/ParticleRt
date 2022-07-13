source("functions/disc_gamma.R")

get_true_rt <- function(y, g_shape = 2, g_scale = 2){
  size <- length(y)
  rt <- rep(0, size)
  rt[1] = 1
  
  for(j in 2:size){
    rt[j] <- y[j]/(sum(y[(j-1):1]*disc_gamma(1:(j-1), g_shape, g_scale)))
  }
  return(rt)
}



# rt <- get_true_rt(covid$y)
# plot(rt[50:length(rt)])
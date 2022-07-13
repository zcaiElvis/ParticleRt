library(truncnorm)
source("functions/disc_gamma.R")


gen_lnormal_pois_rw <- function(size, sd_log, init_x = 1, g_shape=2, g_scale = 2){
  x <- rep(1, size)
  y <- rep(1, size)
  x[1] <- init_x
  
  for(j in 2:size){
    x[j] <- rlnorm(n=1, meanlog=log(x[j-1]), sdlog = sd_log)
    y[j] <- x[j-1]*sum(y[(j-1):1]*disc_gamma(1:(j-1), shape = g_shape, scale = g_scale))
    y[j] <- round(y[j])
  }
  return(data.frame(idx = 1:length(x), x=x, y=y))
}


# gened <- gen_lnormal_pois_rw(size = 1000, sd_log = 0.02, init_x = 1, g_shape = 2, g_scale = 2)
# 
# gened %>%
#   pivot_longer(cols = c(x, y))%>%
#   ggplot(aes(x=idx, y = value))+
#   geom_line()+
#   facet_wrap(vars(name), scales = "free")


gen_normal_rw <- function(size, sd1 = 1, sd2 = 1, init_x = 1, init_y = 1){
  x <- rep(1,size)
  y <- rep(1,size)
  
  x[1] = init_x
  y[1] = init_y
  
  for(j in 2:size){
    x[j] <- rnorm(1, mean=x[j-1], sd= sd1)
    y[j] <- rnorm(1, mean=x[j], sd = sd2)
  }
  return(data.frame(idx = 1:length(x), x=x, y=y))
}


gen_pois_trnormal_rw <- function(size, sd1 = 1, init_x = 1){
  x <- rep(1,size)
  y <- rep(1,size)
  x[1] = init_x
  y[1] = init_x
  
  for(j in 2:size){
    x[j] <- rtruncnorm(1, mean=x[j-1], a= 0, sd= sd1)
    y[j] <- rpois(1, lambda = x[j])
  }
  return(data.frame(idx = 1:length(x), x=x, y=y))
}


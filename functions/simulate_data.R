source("functions/disc_gamma.R")

gen_lnormal_pois_rw <- function(size, sd_log, init_x = 1, g_shape=2, g_scale = 2){
  x <- rep(1, size)
  y <- rep(1, size)
  w <- disc_gamma(1:size, shape = g_shape, scale = g_scale)
  x[1] <- init_x
  
  for(j in 2:size){
    x[j] <- rlnorm(n=1, meanlog=log(x[j-1]), sdlog = sd_log)
    y[j] <- x[j-1]*sum(y[(j-1):1]*w[1:(j-1)])
  }
  return(data.frame(idx = 1:length(x), x=x, y=y))
}


gened <- gen_lnormal_pois_rw(1000, 0.03, 1, 2, 2)

gened %>%
  pivot_longer(cols = c(x, y))%>%
  ggplot(aes(x=idx, y = value))+
  geom_line()+
  facet_wrap(vars(name), scales = "free")

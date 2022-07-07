library("tidyverse")
library("dplyr")
library("zoo")
#Load pomp after
library("pomp")
source("gen_rw.R")


### Generate data ###
rw_pois_100 <- gen_bound_pois_rw(100)
x_train <- rw_pois_100$x
rw_pois_100 <- rw_pois_100 %>% select(-x)

rw_pois_100 |>
  ggplot(aes(x=idx, y = y))+
  geom_line()


### Create pomp object ###
rw_pois_100 |>
  pomp(
    times="idx", t0 = 1,
    rinit = function(x0, ...){
      c(x = x0)
    },
    rprocess = discrete_time(
      function(x, sigma1, ...){
        xnext <- rnorm(1, mean=x, sd = sigma1)
        while(xnext <0){
          xnext <- rnorm(1, mean=x, sd = sigma1)
        }
        c(x = xnext)
      },
      delta.t = 1
    ),
    
    rmeasure = function(x, ...){
      ynext <- rpois(lambda = x)
      c(y = ynext)
    },
    
    statenames = "x",
    
    paramnames = c("sigma1", "x0")
    
    
  ) -> pomp_pois_rw


### Build particle filter ###
pomp_pois_rw |>
  pfilter(
    Np = 10000,
    params = c(sigma1 = 1, x0=1),
    dmeasure = function(y, x,sigma1, ..., log){
      dpois(y, lambda=x, log = log)
    },
    statenames = "x",
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE
  ) -> filter_pois_rw


filter_result <- as.data.frame(filter_pois_rw)




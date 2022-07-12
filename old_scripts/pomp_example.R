library("tidyverse")
library("dplyr")
library("zoo")
#Load pomp after
library("pomp")
source("gen_rw.R")


### Generate data ###

rw_normal_100 <- gen_normal_rw(100)
x_train <- rw_normal_100$x
rw_normal_100 <- rw_normal_100 %>% 
  select(-x)
rw_normal_100 |>
  ggplot(aes(x=idx, y = y))+
  geom_line()




### Creating pomp object ###

rw_normal_100 |>
  pomp(
    times="idx", t0 = 1,
    rinit = function(x0, ...){
      c(x = x0)
    },
    rprocess = discrete_time(
      function(x, sigma1, ...){
        xnext <- rnorm(1, mean=x, sd = sigma1)
        c(x = xnext)
      },
      delta.t = 1
    ),
    
    rmeasure = function(x, sigma2, ...){
      ynext <- rnorm(1, mean = x, sd = sigma2)
      c(y = ynext)
    },
    
    statenames = "x",
    
    paramnames = c("sigma1", "sigma2", "x0")

    
  ) -> pomp_normal_rw



### Use particle filter ###

pomp_normal_rw |>
  pfilter(
    Np = 10000,
    params = c(sigma1 = 1, sigma2 = 1, x0=1),
    dmeasure = function(y, x,sigma1, ..., log){
      dnorm(y, mean=x, sd = sigma1, log = log)
    },
    statenames = "x",
    pred.mean = TRUE,
    pred.var = TRUE,
    filter.mean = TRUE,
    filter.traj = TRUE
  ) -> filter_normal_rw

filter_result <- as.data.frame(filter_normal_rw)









library("pomp")
library("truncnorm")


tran_normal_unconst <- discrete_time(
  function(x, sigma1, ...){
    xnext <- rnorm(1, x, sd = sigma1)
    c(x = xnext)
  },
  delta.t = 1
)


tran_normal_const <- discrete_time(
  function(x, sigma1, ...){
    xnext <- rtruncnorm(1, a = 0, b = 2, mean = x, sd = 0.2)
    c(x = xnext)
  },
  delta.t = 1
)

tran_normal_epifilter <- discrete_time(
  function(x, sigma1, ...){
    xnext <- rnorm(1, x, sd = 0.1*sqrt(abs(x)))
    c(x=xnext)
  },
  delta.t = 1
)


tran_lognormal <- discrete_time(
  function(x, sigma1, ...){
    xnext <- rlnorm(1, meanlog = log(x), sdlog = 0.03)
    c(x = xnext)
  },
  delta.t = 1
)




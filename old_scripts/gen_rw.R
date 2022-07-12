

gen_normal_rw <- function(len, sd1 = 1, sd2 = 1, x0 = 1, y0 = 1){
  x <- rep(0, len)
  y <- rep(0, len)
  x[1] = x0
  y[1] = y0
  
  for(i in 2:len){
    x[i] <- rnorm(1, x[i-1], sd1)
    y[i] <- rnorm(1, x[i], sd2)
  }
  
  return(data.frame(idx = 1:len, x=x, y=y))
}


gen_bound_pois_rw <- function(len, sd1 = 1, x0 = 1, y0 = 1){
  x <- rep(0, len)
  y <- rep(0, len)
  x[1] = x0
  y[1] = y0
  
  for(i in 2:len){
    x[i] <- rnorm(1, x[i-1], sd1)
    while(x[i]<0){
      x[i] <- rnorm(1, x[i-1], sd1)
    }
    y[i] <- rpois(1, x[i])
  }
  return(data.frame(idx = 1:len, x=x, y=y))
}



# Generate covid data

# 1. Gen list of R
# 2. Set generation interval distribution, gamma(a, b)


gen_covid_rw <- function(len, sd1 = 0.3, x0 = 1.2, y0 = 1, a = 2, b = 2){
  x <- rep(0, len)
  y <- rep(0, len)
  ls <- rep(0, len)
  
  cut = 10
  x[1:cut] = x0
  y[1:cut] = y0
  
  w_all <- dgamma(1:len, shape = a, scale = b)
  
  
  ### Generate R ###
  for(i in 2:len){
    x[i] <- rnorm(1, x[i-1], sd1)
    while(x[i] < 0.5 || x[i] > 3 ){
      x[i] <- rnorm(1, x[i-1], sd1)
    }
  }
  
  for(j in cut:len){
    rate <- x[j] * sum(y[j-1:1] * w_all[1:j-1])
    ls[j] <- rate
    y[j] <- rpois(1, lambda = rate)
  }
  return(data.frame(idx = 1:len, x=x, y=y, lambdas = ls))
}

gen_covid_rw(100)

gen_covid <- gen_covid_rw(100)
plot(gen_covid$x)
